snitch
=====
[![Build](https://github.com/azimut/snitch/actions/workflows/main.yml/badge.svg)](https://github.com/azimut/snitch/actions/workflows/main.yml)

An OTP application, that periodically queries provided DNS domains. And inserts them on a postgresql database. Opinionated on which fields are recorded.

Build
-----

    $ rebar3 compile

TODO
----
* Make multiple requests on first add of domain to get most of IP's from DNS
* Stats of how much is processing
* Config file
