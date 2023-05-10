# rebar variables
REBAR3 := rebar3
ELVIS := _build/elvis/lib/elvis/_build/default/bin/elvis

.PHONY: all
.NOTPARALLEL: all
all: compile elvis test cover check doc format-verify

.PHONY: compile
compile:
	$(REBAR3) compile

.PHONY: doc
doc:
	$(REBAR3) ex_doc

.PHONY: check
check: xref hank dialyzer app_calls

.PHONY: dialyzer
dialyzer:
	$(REBAR3) dialyzer

.PHONY: app_calls
app_calls:
	$(REBAR3) check_app_calls

.PHONY: xref
xref:
	$(REBAR3) xref

.PHONY: hank
hank:
	@ERL_FLAGS="-enable-feature maybe_expr" $(REBAR3) hank

.PHONY: gradualizer
gradualizer:
	$(REBAR3) gradualizer

.PHONY: test
test: eunit

.PHONY: eunit
eunit:
	$(REBAR3) as test eunit --cover

.PHONY: cover
cover:
	$(REBAR3) as test cover --verbose

.PHONY: clean
clean:
	$(REBAR3) clean
	rm -rf _build

.PHONY: format
format:
	@ERL_FLAGS="-enable-feature maybe_expr" $(REBAR3) format

.PHONY: format-verify
format-verify:
	@ERL_FLAGS="-enable-feature maybe_expr" $(REBAR3) format --verify

.PHONY: elvis
elvis: $(ELVIS)
	$(ELVIS) rock -V

$(ELVIS):
	$(REBAR3) as elvis compile
	cd _build/elvis/lib/elvis && $(REBAR3) escriptize
