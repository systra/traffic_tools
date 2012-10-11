Traffic limiting tools
======================

This is a **work in progress** set of modules whose aim is to provide tools for traffic limiting and shaping.

Most important are:

* **traffic\_guard** - Simple [token bucket](http://en.wikipedia.org/wiki/Token_bucket) implementation as a request counter for limiting a request flow to a user defined rate.
                      It can be uses to test the conformance to given traffic policy.

Example usage:

    {ok, Pid} = traffic_guard:start_link(http_limiter, 10, 60), % 10 reqs within 60 sec
    ...
    % handle request
    case traffic_guard:check(Pid) of
       ok -> % allow execution
             some_api:fun();
       reject -> % reject request
             {error, too_many_requests} % return eg. HTTP 420 Status
    end,
    ...


* **traffic\_limiter** - Simple [leaky bucket](http://en.wikipedia.org/wiki/Leaky_bucket) implementation as a request frequency counter. Request flow exeeding given frequency
                         (number of requests per second) can be rejected or slowed down (on simple wait) to conform given policy. In the latter scenario, the server works as
                         a simple traffic shaper - however, currently there is no control on the number of queued request waiting to pass (planning to implement it in the future).

Example usage (rejecting requests that do not meet policy):

    {ok, Pid} = traffic_limiter:start_link(http_limiter, 5), % 5 reqs/s
    ...
    % handle request
    case traffic_limiter:check(Pid) of
       ok -> % allow execution
             some_api:fun();
       reject -> % reject request
             {error, too_many_requests}
    end,
    ...

Example usage (shaping traffic):

    {ok, Pid} = traffic_limiter:start_link(http_limiter, 5, wait), % 5 reqs/s
    ...
    % handle request
    ok = traffic_limiter:check(Pid),
    some_api:fun();
    ...


[![endorse](http://api.coderwall.com/systra/endorse.png)](http://coderwall.com/systra)
