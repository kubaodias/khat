%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2015 17:05
%%%-------------------------------------------------------------------
-module(khat_channel).

-behaviour(gen_server).

%% Includes
-include("../include/logger.hrl").
-include("../include/khat.hrl").

%% API
-export([
    start_link/1,
    broadcast/2
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
-spec(start_link(ChannelName :: khat_channel_name()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ChannelName) ->
    ProcessName = process_name(ChannelName),
    gen_server:start_link({local, ProcessName}, ?MODULE, [ChannelName], []).

-spec broadcast(ChannelName :: khat_channel_name(), Data :: binary()) -> ok.
broadcast(ChannelName, Data) ->
    ProcessName = process_name(ChannelName),
    gen_server:cast(ProcessName, {broadcast, Data}).

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
    {ok, State :: #khat_channel{}} | {ok, State :: #khat_channel{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([ChannelName]) ->
    ?INFO("Started channel process for ~s", [ChannelName]),
    {ok, #khat_channel{name = ChannelName}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #khat_channel{}) ->
    {reply, Reply :: term(), NewState :: #khat_channel{}} |
    {reply, Reply :: term(), NewState :: #khat_channel{}, timeout() | hibernate} |
    {noreply, NewState :: #khat_channel{}} |
    {noreply, NewState :: #khat_channel{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #khat_channel{}} |
    {stop, Reason :: term(), NewState :: #khat_channel{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #khat_channel{}) ->
    {noreply, NewState :: #khat_channel{}} |
    {noreply, NewState :: #khat_channel{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #khat_channel{}}).
handle_cast({broadcast, Data}, State) ->
    #khat_channel{clients = Clients} = State,
    _ = [ok = khat_client_worker:send(Client, Data) || Client <- Clients],
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
-spec(handle_info(Info :: timeout() | term(), State :: #khat_channel{}) ->
    {noreply, NewState :: #khat_channel{}} |
    {noreply, NewState :: #khat_channel{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #khat_channel{}}).
handle_info({'DOWN', _Ref, monitor}, State) ->
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
    State :: #khat_channel{}) -> term()).
terminate(Reason, State) ->
    #khat_channel{name = ChannelName} = State,
    ?WARN("Channel ~s terminating with ~p reason", [ChannelName, Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #khat_channel{},
    Extra :: term()) ->
    {ok, NewState :: #khat_channel{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec process_name(ChannelName :: khat_channel_name()) -> atom().
process_name(ChannelName) ->
    khat_utils:list_to_atom("channel_" ++ ChannelName).