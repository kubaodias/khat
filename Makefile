NAME=khat
.PHONY: test clean

compile:
	rebar3 compile

test:
	rebar3 eunit -c

run:
	rebar3 shell --apps khat --config sys.config

clean:
	rebar3 clean
	rm -rf _builds
