-module(bus_listener).

-behaviour(ebus_handler).

%% API
-export([handle_msg/2]).

handle_msg({_Channel, {Sender, Type, Msg}}, User) ->
    if
        Sender =:= User -> ok;
        true            -> User ! {Type, Msg}
    end.
