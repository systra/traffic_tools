Traffic limiting tools
======================

This is a **work in progress** set of modules whose aim is to provide tools for traffic limiting and shaping.

Most important are:

* **traffic\_guard** - Simple [token bucket](http://en.wikipedia.org/wiki/Token_bucket) implementation as a request counter for limiting a request flow to a user defined rate limit.
                      It can be uses to test the conformance to given traffic policy.

Example usage:

    {ok, Pid} = traffic_guard:start_link(http_limiter, 10, 60), % 2 req/min
    ...
    % handle request
    case traffic_guard:check(Pid) of
       ok -> % allow execution
             some_api:fun();
       reject -> % reject request
             {error, too_many_requests}
    end,
    ...
           


* **traffic\_limiter** - Simple rate limiting server.
          
...more soon...


[![endorse](http://api.coderwall.com/systra/endorse.png)](http://coderwall.com/systra)
