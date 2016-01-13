CLIENT_PATH := _build/default/rel/chatbus/lib/chatbus-0.1.0


all: compile release client


compile:
	./rebar3	 compile

release:
	./rebar3	 release


client:
	rm -rf $(CLIENT_PATH)/priv
	mkdir -p $(CLIENT_PATH)/priv
	cp -R apps/chat_client/ionic/* $(CLIENT_PATH)/priv/
	# cp -R apps/chat_client/polymer/app/* $(CLIENT_PATH)/priv/


console:
	_build/default/rel/chatbus/bin/chatbus console
