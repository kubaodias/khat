%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% Process handling TCP connection to the client.
%%% @end
%%% Created : 19. mar 2015 17:05
%%%-------------------------------------------------------------------
-module(khat_client_worker).

-behaviour(gen_server).

%% Includes
-include("../include/khat.hrl").
-include("../include/khat_logger.hrl").

%% API
-export([
    start_link/2,
    start/2,
    stop/1,
    send/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Socket :: port(), InactivityTimeout :: pos_integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Socket, InactivityTimeout) ->
    gen_server:start_link(?MODULE, [Socket, InactivityTimeout], []).

-spec(start(Socket :: port(), InactivityTimeout :: pos_integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start(Socket, InactivityTimeout) ->
    gen_server:start(?MODULE, [Socket, InactivityTimeout], []).

-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop).

-spec send(Pid :: pid(), Data :: binary()) -> ok.
send(Pid, Data) ->
    gen_server:cast(Pid, {send, Data}).

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
-spec(init(Args :: term()) ->
    {ok, State :: #khat_client{}} | {ok, State :: #khat_client{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Socket, InactivityTimeout]) ->
    ok = khat_group:subscribe(?GLOBAL_GROUP),
    State = #khat_client{socket = Socket, inactivity_timeout = InactivityTimeout},
    schedule_timeout(State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #khat_client{}) ->
    {reply, Reply :: term(), NewState :: #khat_client{}} |
    {reply, Reply :: term(), NewState :: #khat_client{}, timeout() | hibernate} |
    {noreply, NewState :: #khat_client{}} |
    {noreply, NewState :: #khat_client{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #khat_client{}} |
    {stop, Reason :: term(), NewState :: #khat_client{}}).
handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #khat_client{}) ->
    {noreply, NewState :: #khat_client{}} |
    {noreply, NewState :: #khat_client{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #khat_client{}}).
handle_cast({send, Data}, State) ->
    #khat_client{name = ClientName, socket = Socket} = State,
    case gen_tcp:send(Socket, Data) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            ?ERROR("Couldn't send data to ~p client: ~p", [ClientName, Reason]),
            {stop, Reason, State}
    end.

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
-spec(handle_info(Info :: timeout() | term(), State :: #khat_client{}) ->
    {noreply, NewState :: #khat_client{}} |
    {noreply, NewState :: #khat_client{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #khat_client{}}).
handle_info({tcp, _Socket, Data}, State) ->
    #khat_client{buffer = PrevData} = State,
    {ok, NewState} = process_data(State#khat_client{buffer = <<PrevData/binary, Data/binary>>}),
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, {shutdown, tcp_closed}, State};
handle_info(inactivity_timeout, State) ->
    {stop, {shutdown, inactivity_timeout}, State}.

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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #khat_client{}) -> term()).
terminate({shutdown, Reason}, State) when (Reason =:= tcp_closed) orelse (Reason =:= inactivity_timeout) ->
    #khat_client{name = ClientName} = State,
    ?INFO("Client ~s terminating with ~p reason", [ClientName, Reason]),
    ok;
terminate(Reason, State) ->
    #khat_client{name = ClientName} = State,
    ?WARN("Client ~s terminating with ~p reason", [ClientName, Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #khat_client{},
    Extra :: term()) ->
    {ok, NewState :: #khat_client{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec schedule_timeout(State :: #khat_client{}) -> {ok, #khat_client{}}.
schedule_timeout(#khat_client{timer_ref = TRef} = State) when TRef =/= undefined->
    _ = erlang:cancel_timer(TRef),
    schedule_timeout(State#khat_client{timer_ref = undefined});
schedule_timeout(State) ->
    #khat_client{inactivity_timeout = Timeout} = State,
    TRef = erlang:send_after(Timeout, self(), inactivity_timeout),
    NewState = State#khat_client{timer_ref = TRef},
    {ok, NewState}.

-spec broadcast_msg(GroupName :: khat_group_name(), ClientName :: khat_client_name(), Msg :: iolist()) -> ok.
broadcast_msg(GroupName, ClientName, Msg) ->
    {Date, Time} = lager_util:format_time(calendar:now_to_local_time(erlang:now())),
    khat_group:broadcast(GroupName, list_to_binary([Date, ",", Time, " ", ClientName, ": ", Msg, "\r\n"])).

-spec process_data(State :: #khat_client{}) -> {ok, #khat_client{}}.
process_data(State) when State#khat_client.buffer =:= <<>> ->
    {ok, State};
process_data(State) ->
    #khat_client{buffer = BufferData} = State,
    case binary:split(BufferData, <<"\r\n">>, []) of
        [BufferData] ->
            % wait for the next data in the buffer
            {ok, State};
        Msgs when is_list(Msgs) ->
            % last message can be incomplete
            {WholeMsgs, LastMsg} = lists:split(length(Msgs) - 1, Msgs),
            {ok, NewState} = process_messages(WholeMsgs, State),
            process_data(NewState#khat_client{buffer = list_to_binary(LastMsg)})
    end.

-spec process_messages(Msgs :: [binary()], State :: #khat_client{}) -> {ok, #khat_client{}}.
process_messages([], State) ->
    {ok, State};
process_messages([BinMsg | RestOfMsgs], State) ->
    #khat_client{name = ClientName, socket = Socket} = State,
    Msg = binary_to_list(BinMsg),
    {ok, NewState} =
        case khat_protocol:parse_msg(Msg) of
            {register, NewClientName} ->
                ?INFO("Register name ~s", [NewClientName]),
                try
                    true = register(khat_utils:list_to_atom(NewClientName), self()),
                    {ok, State#khat_client{name = NewClientName}}
                catch error:badarg when ClientName =:= undefined ->
                    ?WARN("Another client with name ~s is already registered", [NewClientName]),
                    _ = gen_tcp:send(Socket, <<"Already registered\r\n">>),
                    {ok, State};
                error:badarg ->
                    ?WARN("Client is already registered with name ~s", [ClientName]),
                    _ = gen_tcp:send(Socket, <<"Already registered\r\n">>),
                    {ok, State}
                end;
            {subscribe, GroupName} ->
                ok = khat_group:subscribe(GroupName),
                schedule_timeout(State);
            {unsubscribe, GroupName} ->
                ok = khat_group:unsubscribe(GroupName),
                schedule_timeout(State);
            {_, ""} when ClientName =:= undefined ->
                {ok, State};
            {_, _GroupMsg} when ClientName =:= undefined ->
                _ = gen_tcp:send(Socket, <<"Unregistered\r\n">>),
                {ok, State};
            {msg, ""} ->
                schedule_timeout(State);
            {msg, Msg} ->
                ok = broadcast_msg(?GLOBAL_GROUP, ClientName, Msg),
                schedule_timeout(State);
            {{group, Group}, GroupMsg} ->
                ok = broadcast_msg(Group, ClientName, GroupMsg),
                schedule_timeout(State);
            {alive, _} ->
                schedule_timeout(State);
            error ->
                ?ERROR("~s - received invalid message ~s", [ClientName, Msg]),
                _ = gen_tcp:send(Socket, <<"Invalid command\r\n">>),
                {ok, State}
        end,
    process_messages(RestOfMsgs, NewState).