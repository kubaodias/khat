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
    start_link/1,
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
-spec(start_link(ClientName :: khat_client_name()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ClientName) ->
    ProcessName = process_name(ClientName),
    gen_server:start_link({local, ProcessName}, ?MODULE, [ClientName], []).

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
init([ClientName]) ->
    ?INFO("Started client process for ~s", [ClientName]),
    {ok, #khat_client{name = ClientName}}.

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
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
handle_info({tcp, Socket, Data}, State) when Socket =:= State#khat_client.socket ->
    {noreply, State};
handle_info({tcp_closed, Socket}, State) when Socket =:= State#khat_client.socket ->
    {stop, {shutdown, tcp_closed}, State}.

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

-spec process_name(ClientName :: khat_client_name()) -> atom().
process_name(ClientName) ->
    khat_utils:list_to_atom("client_" ++ ClientName).