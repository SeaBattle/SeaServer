PROJECT = seaserver

DIALYZER = dialyzer

all: rel

# Application.

deps:
	git submodule update --init --recursive

build:  deps
	./rebar3 compile

rel: build
	./rebar3 release

clean:
	./rebar3 clean
	rm -f test/*.beam
	rm -f erl_crash.dump

# Tests.
tests: clean app eunit ct

eunit:
	./rebar3 eunit skip_deps=true

ct:
	./rebar3 ct skip_deps=true

# Dialyzer.
.$(PROJECT).plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt -r deps \
		--apps erts kernel stdlib sasl inets crypto public_key ssl mnesia syntax_tools asn1

clean-plt:
	rm -f .$(PROJECT).plt

build-plt: clean-plt .$(PROJECT).plt

dialyze: .$(PROJECT).plt
	@$(DIALYZER) -I include -I deps --src -r src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns

.PHONY: deps clean-plt build-plt dialyze