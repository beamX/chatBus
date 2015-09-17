-module(bus_manager).

-behaviour(gen_server).

%% API functions
-export([start_link/0
        ,store_username/2
        ,get_hitchhickers/1
        ,remove_hitchhicker/1
        ,check_bus/1
        ,bus_list/0
        ]).

-export([bind_test_listener/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store_username(BusName, Username) ->
    gen_server:call(?MODULE, {store_username, BusName, Username}).

get_hitchhickers(BusName) ->
    gen_server:call(?MODULE, {get_hitchhickers, BusName}).

remove_hitchhicker(Hitchhicker) ->
    gen_server:call(?MODULE, {remove_hitchhickers, Hitchhicker}).

check_bus(BusName) ->
    gen_server:cast(?MODULE, {check_bus, BusName}).

bus_list() ->
    gen_server:call(?MODULE, {bus_list}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ets:new(?MODULE, [set, public, named_table
                     ,{write_concurrency, false}
                     ,{read_concurrency, true}]),

    {ok, #{users => ?MODULE}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({store_username, BusName, Username}, _From, #{users := Users} = State) ->
    Reply = case ets:match(Users, {Username, '$1'}, 1) of
                '$end_of_table' ->
                    ets:insert(?MODULE, {Username, BusName}),
                    {ok, Username};
                _ ->
                    {ok, error}
            end,
    {reply, Reply, State};

handle_call({get_hitchhickers, BusName}, _From, #{users := Users} = State) ->
    Reply = case ets:match(Users, {'$1', BusName}) of
                '$end_of_table' ->
                    {ok, []};
                List ->
                    {ok, lists:flatten(List)}
            end,
    io:format("hitchhickers : ~p~n", [Reply]),
    {reply, Reply, State};

handle_call({remove_hitchhickers, Hitchhicker}, _From, #{users := Users} = State) ->
    ets:delete(Users, Hitchhicker),
    {reply, ok, State};

handle_call({bus_list}, _From, State) ->
    {reply, {ok, ebus:channels()}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({check_bus, BusName}, State) ->
    case ebus:subscribers(BusName) of
        %% ebus doesn't provide any api to delete
        [] when BusName =/= default -> pg2:delete(BusName);
        _  -> ok
    end,
    ws_handler:send_active_channels(ebus:channels()),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
bind_test_listener() ->
    F = fun({Channel, Msg}, Ctx) ->
      io:format("[Pid: ~p][Channel: ~p][Msg: ~p][Ctx: ~p]~n",
                [self(), Channel, Msg, Ctx])
    end,

    Listener = ebus_handler:new(F, {listener, <<"test">>}),
    ok  = ebus:sub(default, [Listener]).
