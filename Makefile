ERLS = $(wildcard apps/*/src/*.erl)
HRLS = $(wildcard apps/*/src/*.hrl)

_build/prod/rel/snitch/bin/snitch: $(ERLS) $(HRLS)
	rebar3 as prod release

.PHONY: run
run: _build/prod/rel/snitch/bin/snitch
	_build/prod/rel/snitch/bin/snitch foreground

.PHONY: clean
clean:
	find . \( -name '*.dump' -or -name '*.crashdump' \) -delete
	rm -rf ./apps/*/_build
	rm -rf ./apps/*/ebin
	rm -rf ./_build
	rebar3 clean
