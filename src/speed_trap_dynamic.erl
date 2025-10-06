%%%=============================================================================
%%% @doc speed_trap_dynamic - Dynamic rate limiter based on rejection rates
%%%
%%% This module implements a dynamic rate limiter that automatically adjusts
%%% bucket_size based on rejection rates. It monitors the rejection rate over a
%%% configurable time interval and scales bucket_size up or down accordingly.
%%%
%%% Key features:
%%% - Automatic upscaling when rejection rate exceeds threshold
%%% - Automatic downscaling when rejection rate is below threshold
%%% - Configurable min/max bucket_size, scaling time intervals, and adjustment increments
%%% - Gradual bucket_size changes to allow downstream systems to scale
%%% - Uses atomics for high-performance rejection/acceptance tracking
%%%
%%% @copyright 2025 Klarna Bank AB
%%% @end
%%%=============================================================================
-module(speed_trap_dynamic).

-export([start_link/0, register_dynamic_limiter/2, unregister_dynamic_limiter/1, record_rejection/1,
         record_acceptance/1]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).
-define(ETS_DYNAMIC_COUNTERS, speed_trap_dynamic_counters).
-define(REJECTION_IDX, 1).
-define(SUCCESS_IDX, 2).

-record(dynamic_state,
        {id :: speed_trap:id(),
         counters :: atomics:atomics_ref(),
         timer_ref :: timer:tref() | undefined}).

-type dynamic_state() :: #dynamic_state{}.
-type state() :: #{speed_trap:id() => dynamic_state()}.

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec register_dynamic_limiter(speed_trap:id(), speed_trap:options()) ->
                                ok | {error, already_registered}.
register_dynamic_limiter(Id, Options) ->
  gen_server:call(?SERVER, {register, Id, Options}).

-spec unregister_dynamic_limiter(speed_trap:id()) -> ok.
unregister_dynamic_limiter(Id) ->
  gen_server:call(?SERVER, {unregister, Id}).

-spec record_rejection(speed_trap:id()) -> ok.
record_rejection(Id) ->
  case ets:lookup(?ETS_DYNAMIC_COUNTERS, Id) of
    [{Id, Counters}] ->
      atomics:add(Counters, ?REJECTION_IDX, 1);
    [] ->
      ok
  end.

-spec record_acceptance(speed_trap:id()) -> ok.
record_acceptance(Id) ->
  case ets:lookup(?ETS_DYNAMIC_COUNTERS, Id) of
    [{Id, Counters}] ->
      atomics:add(Counters, ?SUCCESS_IDX, 1);
    [] ->
      ok
  end.

%%-----------------------------------------------------------------------------
%% gen_server callbacks
%%-----------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
  ?ETS_DYNAMIC_COUNTERS =
    ets:new(?ETS_DYNAMIC_COUNTERS, [set, protected, named_table, {read_concurrency, true}]),
  {ok, #{}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call({register, Id, Options}, _From, State) ->
  case maps:is_key(Id, State) of
    true ->
      {reply, {error, already_registered}, State};
    false ->
      #{scaling_time_interval := TimeInterval} = Options,
      % Create atomic counters for rejections and acceptances
      Counters = atomics:new(2, [{signed, false}]),
      atomics:put(Counters, ?REJECTION_IDX, 0),
      atomics:put(Counters, ?SUCCESS_IDX, 0),
      {ok, TimerRef} = timer:send_after(TimeInterval, self(), {check_and_adjust, Id}),
      DynState =
        #dynamic_state{id = Id,
                       counters = Counters,
                       timer_ref = TimerRef},
      NewState = State#{Id => DynState},
      ets:insert(?ETS_DYNAMIC_COUNTERS, {Id, Counters}),
      {reply, ok, NewState}
  end;
handle_call({unregister, Id}, _From, State) ->
  case maps:take(Id, State) of
    {DynState, NewState} ->
      case DynState#dynamic_state.timer_ref of
        undefined ->
          ok;
        TimerRef ->
          timer:cancel(TimerRef)
      end,
      ets:delete(?ETS_DYNAMIC_COUNTERS, Id),
      {reply, ok, NewState};
    error ->
      {reply, ok, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({check_and_adjust, Id}, State) ->
  case maps:get(Id, State, undefined) of
    undefined ->
      {noreply, State};
    DynState ->
      NewDynState = check_and_adjust_bucket_size(DynState),
      NewState = State#{Id => NewDynState},
      {noreply, NewState}
  end;
handle_info(_Info, State) ->
  {noreply, State}.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
-spec check_and_adjust_bucket_size(dynamic_state()) -> dynamic_state().
check_and_adjust_bucket_size(DynState) ->
  #dynamic_state{id = Id, counters = Counters} = DynState,
  {ok,
   #{bucket_size := CurrentBucketSize,
     min_bucket_size := MinBucketSize,
     max_bucket_size := MaxBucketSize,
     refill_count := RefillCount,
     scaling_time_interval := TimeInterval,
     rejection_rate_threshold := Threshold,
     scaling_bucket_size_adjust_count := AdjustCount}} =
    speed_trap_token_bucket:options(Id),
  % Read and reset counters atomically
  TotalRejections = atomics:exchange(Counters, ?REJECTION_IDX, 0),
  TotalSuccesses = atomics:exchange(Counters, ?SUCCESS_IDX, 0),
  TotalRequests = TotalRejections + TotalSuccesses,
  RejectionRate =
    case TotalRequests of
      0 ->
        0;
      _ ->
        round(TotalRejections / TotalRequests * 100)
    end,
  if RejectionRate > Threshold andalso CurrentBucketSize < MaxBucketSize ->
       % Upscale: increase bucket_size
       NewQ = min(CurrentBucketSize + AdjustCount, MaxBucketSize),
       adjust_bucket_size(Id, CurrentBucketSize, NewQ, RefillCount),
       NewQ;
     RejectionRate < Threshold andalso CurrentBucketSize > MinBucketSize ->
       % Downscale: decrease bucket_size
       NewQ = max(CurrentBucketSize - AdjustCount, MinBucketSize),
       adjust_bucket_size(Id, CurrentBucketSize, NewQ, RefillCount),
       NewQ;
     true ->
       % No adjustment needed
       CurrentBucketSize
  end,
  % Schedule next check
  {ok, TimerRef} = timer:send_after(TimeInterval, self(), {check_and_adjust, Id}),
  DynState#dynamic_state{timer_ref = TimerRef}.

-spec adjust_bucket_size(speed_trap:id(), pos_integer(), pos_integer(), pos_integer()) -> ok.
adjust_bucket_size(Id, CurrentBucketSize, NewBucketSize, RefillCount) ->
  %% Calculate scaling factor
  ScalingFactor = NewBucketSize / CurrentBucketSize,
  %% Scale refill_count proportionally
  NewRefillCount = round(RefillCount * ScalingFactor),
  %% Ensure minimum refill count
  AdjustedRefillCount = max(1, NewRefillCount),
  ModifyOpts = #{bucket_size => NewBucketSize, refill_count => AdjustedRefillCount},
  case speed_trap:modify(Id, ModifyOpts) of
    ok ->
      ok;
    {error, _Reason} ->
      ok % Ignore errors, maybe the bucket doesn't exist yet
  end.
