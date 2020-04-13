snitch
=====

An OTP application

Build
-----

    $ rebar3 compile

TODO
----
* Make multiple requests on first add of domain to get most of IP's from DNS
* ~~Cool down domains~~
* Replace with mnesia? this will also help with ordsets in cumulative record might be
* Separate genserver for storage?
* Separate genserver for query?
* ~~Return DNS status code response and save it~~
* Ets stores all returned records even when only 1 was new
* Load DNS servers from a file
