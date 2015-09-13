-module(ws_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-export([send_active_channels/1]).



init(Req, _Opts) ->
    io:format("connected !~n"),

    %% subscribe to default bus
    BusFd = ebus_handler:new(bus_listener, self()),
    ok    = ebus:sub(default, [BusFd]),

    %% send subscribes bus name
    auto_send(<<"bus_subscribed">>, default),
    {cowboy_websocket, Req, #{bus => default, bus_fd => BusFd}}.


websocket_handle({text, Msg}, Req, #{bus := BusName, bus_fd := BusFd} =  State) ->
    {ok, {Type, Msg1}} = parse_message(Msg),
    case Type of

        <<"chat">> ->
            ok = ebus:pub(BusName, {self(), Type, Msg1}),
            {ok, Req, State};

        <<"bus_list">> ->
            {ok, List}  = bus_manager:bus_list(),
            {ok, Reply} = encode_message(<<"bus_list">>, List),
            {reply, {text, Reply}, Req, State};

        <<"add_bus">> ->
            BusNewName  = erlang:binary_to_atom(Msg1, utf8),

            ok      = ebus:unsub(BusName, BusFd),
            ok      = ebus:sub(BusNewName, [BusFd]),

            %% signal bus_manager to send all client list of active buses
            bus_manager:check_bus(BusName),

            %% send message to client updating his bus
            {ok, Reply} = encode_message(<<"bus_subscribed">>, BusNewName),

            {reply, {text, Reply}, Req, State#{bus => BusNewName}}
    end;

websocket_handle(Data, Req, State) ->
    io:format("received ~p~n", [Data]),
    {ok, Req, State}.


%% handle erlang messages
websocket_info({Type, Msg}, Req, State) ->
    {ok, Reply} = encode_message(Type, Msg),
    {reply, {text, Reply}, Req, State};

websocket_info(_Info, Req, State) ->

    {ok, Req, State}.


 
websocket_terminate(_Reason, _Req, #{bus := BusName, bus_fd := BusFd}) ->
    ebus:unsub(BusName, BusFd),
    ebus_handler:delete(BusFd),
    ok.  


%% =============================================================================
%% other exports
%% =============================================================================

send_active_channels(Channels) ->
    lists:map(fun(Bus) ->
                      ok = ebus:pub(Bus, {none, <<"bus_list">>, Channels})
              end, Channels).


%% =============================================================================
%% internal functions
%% =============================================================================
auto_send(Mtype, Msg) ->
    %% send subscribes bus name
    timer:send_after(10, self(), {Mtype, Msg}).


parse_message(Msg) ->
    {struct, Msg1}         = mochijson2:decode(Msg),
    {<<"type">>, Type}     = lists:keyfind(<<"type">>, 1, Msg1),
    {<<"msg">>,  Content}  = lists:keyfind(<<"msg">>, 1, Msg1),
    {ok, {Type, Content}}.


encode_message(Type, Msg) ->
    Reply = {[{type, Type}, {msg, Msg}]},
    {ok, iolist_to_binary(mochijson2:encode(Reply))}.
