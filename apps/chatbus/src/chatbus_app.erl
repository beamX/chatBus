%%%-------------------------------------------------------------------
%% @doc chatbus public API
%% @end
%%%-------------------------------------------------------------------

-module('chatbus_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->

    ok = application:ensure_started(ebus),

    %% taken from cowboy websocket tutorial
    Dispatch = cowboy_router:compile(
                 [{'_', [
                         {"/", cowboy_static, {priv_file, chatbus, "./www/index.html"}},
                         {"/ws", ws_handler, []},
                         %{"/static/[...]", cowboy_static, {priv_dir, chatbus, "static"}}
                         {"/[...]", cowboy_static, {priv_dir, chatbus, "./www"}}

                        ]}
                 ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 9090}], [{env, [{dispatch, Dispatch}]}]),
    'chatbus_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================


