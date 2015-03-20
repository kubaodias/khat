%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2015 17:05
%%%-------------------------------------------------------------------
-module(khat_group).

-behaviour(gen_server).

%% Includes
-include("../include/khat.hrl").
-include("../include/khat_logger.hrl").
-include("../include/khat_testing.hrl").

%% API
-export([
    start_link/1,
    stop/1,
    subscribe/1,
    unsubscribe/1,
    broadcast/2,
    client_count/1
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
-spec(start_link(GroupName :: khat_group_name()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(GroupName) ->
    ProcessName = group_name(GroupName),
    gen_server:start_link({local, ProcessName}, ?MODULE, [GroupName, ProcessName], []).

stop(GroupName) ->
    ProcessName = group_name(GroupName),
    gen_server:call(ProcessName, stop).

-spec subscribe(GroupName :: khat_group_name()) -> ok.
subscribe(GroupName) ->
    Pid = self(),
    {ok, GroupPid} = ensure_started(GroupName),
    gen_server:cast(GroupPid, {subscribe, Pid}).

-spec unsubscribe(GroupName :: khat_group_name()) -> ok.
unsubscribe(GroupName) ->
    Pid = self(),
    {ok, GroupPid} = ensure_started(GroupName),
    gen_server:cast(GroupPid, {unsubscribe, Pid}).

-spec broadcast(GroupName :: khat_group_name(), Data :: binary()) -> ok.
broadcast(GroupName, Data) ->
    {ok, GroupPid} = ensure_started(GroupName),
    gen_server:cast(GroupPid, {broadcast, Data}).

-spec client_count(GroupName :: khat_group_name()) -> non_neg_integer() | undefined.
client_count(GroupName) ->
    ProcessName = group_name(GroupName),
    ets:info(ProcessName, size).

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
    {ok, State :: #khat_group{}} | {ok, State :: #khat_group{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([GroupName, ProcessName]) ->
    ProcessName = ets:new(ProcessName, [protected, set, named_table]),
    ?INFO("Started group process for ~s", [GroupName]),
    {ok, #khat_group{name = GroupName, tab = ProcessName}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #khat_group{}) ->
    {reply, Reply :: term(), NewState :: #khat_group{}} |
    {reply, Reply :: term(), NewState :: #khat_group{}, timeout() | hibernate} |
    {noreply, NewState :: #khat_group{}} |
    {noreply, NewState :: #khat_group{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #khat_group{}} |
    {stop, Reason :: term(), NewState :: #khat_group{}}).
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #khat_group{}) ->
    {noreply, NewState :: #khat_group{}} |
    {noreply, NewState :: #khat_group{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #khat_group{}}).
handle_cast({subscribe, Pid}, State) ->
    #khat_group{name = GroupName, tab = Tid} = State,
    ?INFO("Group ~s - subscribe client ~p", [GroupName, Pid]),
    Ref = erlang:monitor(process, Pid),
    true = ets:insert(Tid, {Pid, Ref}),
    {noreply, State};
handle_cast({unsubscribe, Pid}, State) ->
    #khat_group{name = GroupName, tab = Tid} = State,
    ?INFO("Group ~s - unsubscribe client ~p", [GroupName, Pid]),
    true = ets:delete(Tid, Pid),
    {noreply, State};
handle_cast({broadcast, Data}, State) ->
    #khat_group{tab = Tid} = State,
    ok = broadcast_to_clients(Tid, ets:first(Tid), Data),
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
-spec(handle_info(Info :: timeout() | term(), State :: #khat_group{}) ->
    {noreply, NewState :: #khat_group{}} |
    {noreply, NewState :: #khat_group{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #khat_group{}}).
handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    #khat_group{name = GroupName, tab = Tid} = State,
    ?DEBUG("Group ~s - client ~p is down with ~p reason", [GroupName, Pid, Reason]),
    true = ets:delete_object(Tid, {Pid, Ref}),
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #khat_group{}) -> term()).
terminate(Reason, State) ->
    #khat_group{name = GroupName} = State,
    ?WARN("Group ~s terminating with ~p reason", [GroupName, Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #khat_group{},
    Extra :: term()) ->
    {ok, NewState :: #khat_group{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec group_name(GroupName :: khat_group_name()) -> atom().
group_name(GroupName) ->
    khat_utils:list_to_atom("group_" ++ GroupName).

-spec ensure_started(GroupName :: khat_group_name()) -> {ok, Pid :: pid()}.
ensure_started(GroupName) ->
    case start_link(GroupName) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid}
    end.

-spec broadcast_to_clients(Tid :: ets:tid(), Client :: {pid(), reference()} | '$end_of_table', Data :: binary()) -> ok.
broadcast_to_clients(_Tid, '$end_of_table', _Data) ->
    ok;
broadcast_to_clients(Tid, Pid, Data) ->
    ok = khat_client_worker:send(Pid, Data),
    broadcast_to_clients(Tid, ets:next(Tid, Pid), Data).