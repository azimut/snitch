ERLS = $(wildcard apps/*/src/*.erl)
HRLS = $(wildcard apps/*/src/*.hrl)

_build/default/rel/snitch/bin/snitch: $(ERL) $(HRLS)
	rebar3 release

.PHONY: run
run: _build/default/rel/snitch/bin/snitch
	_build/default/rel/snitch/bin/snitch foreground

.PHONY: clean
clean:
	find . \( -name '*.dump' -or -name '*.crashdump' \) -delete
	rm -rf ./apps/*/_build
	rm -rf ./apps/*/ebin
	rm -rf ./_build
	rebar3 clean
