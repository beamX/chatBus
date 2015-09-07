
all: compile release


compile:
	rebar3	 compile

release:
	rebar3	 release

console:
	_build/default/rel/chatbus/bin/chatbus console
