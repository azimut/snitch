snitch
=====

An OTP application

Build
-----

    $ rebar3 compile

TODO
----
* Make multiple requests on first add of domain to get most of IP's from DNS
* Cool down domains
* Replace with mnesia?
* Separate genserver for storage?
* Separate genserver for query?
* Return DNS status code response and save it
