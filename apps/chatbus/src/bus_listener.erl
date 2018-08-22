-module(bus_listener).


%% API
-export([handle_msg/2]).

handle_msg({Sender, Type, Msg}, User) ->
    if
        Sender =:= User -> ok;
        true            -> User ! {Type, Msg}
    end.
