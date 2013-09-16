PROJECT = spas

CT_SUITES = eunit

DEPS = cowboy jsx oauth
dep_cowboy = https://github.com/extend/cowboy.git 0.8.6
dep_jsx = https://github.com/talentdeficit/jsx.git
dep_oauth = https://github.com/tim/erlang-oauth.git

include erlang.mk
 
REBAR=$(shell which rebar)

debug: all
	erl -pa ebin -pa deps/*/ebin -s spas

eunit: app
	$(REBAR) skip_deps=true eunit