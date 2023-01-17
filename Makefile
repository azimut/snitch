.PHONY: compile
compile:
	rebar3 compile

.PHONY: shell
shell:
	rebar3 shell

.PHONY: dialyzer
dialyzer:
	rebar3 dialyzer

.PHONY: clean
clean:
	find . \( -name '*.dump' -or -name '*.crashdump' \) -delete
	rm -rf ./apps/*/_build
	rm -rf ./apps/*/ebin
	rebar3 clean
