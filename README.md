# speed_trap
A basic Token Bucket rate limiter written in Erlang with no external dependencies.

[![Build](https://github.com/klarna-incubator/speed_trap/actions/workflows/build.yml/badge.svg)](https://github.com/klarna-incubator/speed_trap/actions/workflows/build.yml)
[![License](https://camo.githubusercontent.com/7b7d3d8a196e3828ccb78dfedc3c626aab728508cbd8e1cfbe27ef9c8216229e/68747470733a2f2f696d672e736869656c64732e696f2f62616467652f6c6963656e73652d417061636865253230322d626c75653f7374796c653d666c61742d737175617265)](http://www.apache.org/licenses/LICENSE-2.0)
![GitHub last commit](https://img.shields.io/github/last-commit/klarna-incubator/speed_trap?style=flat-square)
[![Developed at Klarna](https://camo.githubusercontent.com/3b04c776d2140980743e386cf674fa4d3ad9dad97a2d514833f47eb77a5bb37e/68747470733a2f2f736869656c64732e696f2f62616467652f446576656c6f70656425323061742532304b6c61726e612d626c61636b3f6c6f676f3d6b6c61726e61)](https://github.com/klarna-incubator)

## Features
A basic [Token Bucket](https://en.wikipedia.org/wiki/Token_bucket) based rate limiter for Erlang.
This is a rate limiter in it's most basic form and uses atomics in order to keep track of
whether or not a request should be rate limited on a per node basis.

Tests were performed across an [atomics](https://www.erlang.org/doc/man/atomics.html),
[ets](https://www.erlang.org/doc/man/ets.html) and a [gen_server](https://www.erlang.org/doc/man/gen_server.html)
based implementation.

Although the benchmark is not provided as part of `speed_trap`, the results are as follows:

| Implementation | Throughput     | Standard deviation   |
|----------------|----------------|----------------------|
| `atomics`      | 31,000,000 cps |             0.915 ms |
| `ets`          |    760,000 cps |                11 ms |
| `gen_server`   |    640,000 cps |             0.517 ms |

### How to parameterise a rate limiter?
A token bucket rate limiter is defined by four mandatory parameters:

* `bucket_size`, positive integer, represents maximum number of tokens that can be available
  to a client
* `refill_interval`, milliseconds, how often the bucket is refilled with tokens
* `refill_count`, positive integer, how many tokens are added every refill
* `delete_when_full`, boolean, whether or not to delete the bucket when it's filled up

Optional parameters:
* `override`, `none | not_enforced | blocked`, whether to override the rate limiter to always allow or block requests. `not_enforced` behaves the same as any other regular rate limiter with the exception that once the bucket is empty, one would receive `{ok, rate_limit_not_enforced}` rather than `{error, too_many_requests}`. This features allows a soft-launch of any rate limiter and allows one to tweak the limits before enforcing the rate limiter. `blocked` on the other hand will always return `{error, blocked}` and essentially short circuits that way. This can be useful when you truly don't want to accept any requests any more for this particular rate limiter.

A new speed trap always starts with a full bucket.

There is several different ways to achieve the same rate limit. For example, one wants to have
the base rate of 100 requests per second. All the following ways to define it are correct:
1. Set the refill interval to 1 second, and the refill count to 100;
2. Set the refill interval to 100 milliseconds, and the refill count to 10;
3. Set the refill interval to 10 milliseconds, and the refill count to 1.

In all cases all available tokens can be consumed as soon as possible and then a client will get
`{error, too_many_requests}` until the bucket is refilled again. The difference, however, is how
new tokens are distributed across the 1 second period. Assume that one creates speed traps with
the bucket size of 100.

Let's say the trap with the refill options from the first way was created at 00.000s and a client
consumes one token per milliseconds. The bucket becomes empty at 00.100s and will be refilled to
100 at 01.000s.

|  Time   |  Available Tokens  |
|---------|--------------------|
| 00.000s | 100 (full bucket)  |
| 00.001s | 99 (1 consumed)    |
| 00.002s | 98 (1 consumed)    |
| ...     | ...                |
| 00.100s | 0  (1 consumed)    |
| 01.000s | 100 (100 refilled) |

In the second and the third cases, the availability of tokens are distributed evenly accross the
correspondent refill intervals. Let's take the 3rd configuration as an example.

* The trap is created at 00.000s, 100 tokens are available to a client. The client consumes one
  token per millisecond.
* At 00.010s the number of available tokens is 91: 90 that haven't been consumed yet plus one
  which is refilled.
* At 00.020 the total number of tokens in the bucket is 82: 81 haven't been consumed plus
  the refilled one.
* And so on.

|  Time   | Available Tokens  |
|---------|-------------------|
| 00.000s | 100 (full bucket) |
| 00.001s | 99 (1 consumed)   |
| 00.002s | 98 (1 consumed)   |
| ...     | ...               |
| 00.010s | 90 (1 consumed)   |
| 00.010s | 91 (1 refilled)   |
| 00.011s | 90 (1 consumed)   |
| 00.011s | 89 (1 consumed)   |
| ...     | ...               |
| 00.020s | 81 (1 consumed)   |
| 00.020s | 82 (1 refilled)   |
| 00.021s | 81 (1 consumed)   |

Notice, that the momentary peak rate can be as high as the bucket size. This might be especially
important when the requests are distributed unevenly. So, if one wants to support short periods
with burst of requests on the system with a relatively low base rate, one could set the bucket
size parameter to a higher value.

It does not make sense to set the bucket size to a lower value than the refill count because
in this case the bucket will contain the overflowing tokens anyway.

## Usage example
Example of setting up and using a rate limiter for POST requests to the captcha page
that should handle a 10 requests per minute base load and allow 50 requests per minute
peaks:

```erlang
application:ensure_all_started(speed_trap).
Id = {<<"/api/v1/captcha_challenge">>, <<"POST">>}.
Options = #{bucket_size => 40,                   % difference of peak and base rates
            refill_interval => timer:seconds(6), % how often the bucket is refilled
            refill_count => 1,                   % number of tokens to refill
            delete_when_full => false            % Do not delete bucket when full
            override => none                     % No overrides
           },
speed_trap:new(Id, Options).                     % ok | raises the bad_options error
speed_trap:try_pass(Id).                         % {ok, RemainingTokens | rate_limit_not_enforced} | {error, too_many_requests}
```

In order to modify the rate limiters one can use modify/2:
```erlang
NewRefillInterval = timer:seconds(1),
speed_trap:modify(Id, #{refill_interval => NewRefillInterval}).
```

In order to delete a rate limiter:
```erlang
speed_trap:delete(Id).
```

## Template rate limiters
When you do not know upfront all the rate limiters that you need you can add templates for rate limiters and connect them to rate limiter id patterns.
When a speed trap is not present when calling speed_trap:try_pass/1, speed_trap looks for a pattern
that matches the supplied id.
If found a rate limiter with the id is created.

The template is a tuple of a template identifier and the same options map as used when calling speed_trap:new/2.
An id pattern is a tuple of a match head and a template identifier for the pattern.

Say that you want a unique rate limiter (with the same configuration) for each user for a resource called my_resource.
For the user "Alex" the rate limiter id would be {my_resource, "Alex"}.

```erlang
{templates,
 #{my_resource_template =>
  #{bucket_size => 40,
    refill_interval => 6000,
    refill_count => 1,
    delete_when_full => true,
    override => none
   }
 }
},
{id_patterns,
 [
  {
   {my_resource, '_'},
    my_resource_template
  }
 ]
}
```

## Development
In order to build the project simply run `make`.

This repo uses `rebar3 format` to ensure consistent formatting of the codebase.
If the pipeline fails with:
```sh
===> The following files are not properly formatted:
src/<file>.erl
```
Please run `make format` and commit the changes.

## Running tests
```bash
make test
```
