%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2015 17:05
%%%-------------------------------------------------------------------
-module(khat_acceptor).

-behaviour(gen_server).

%% Includes
-include("../include/khat.hrl").
-include("../include/khat_logger.hrl").

%% API
-export([start_link/0]).

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
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link(?MODULE, [], []).

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
    {ok, State :: #khat_acceptor{}} | {ok, State :: #khat_acceptor{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, ListenSocket} = khat_listener:get_socket(),
    ok = accept_connections(),
    {ok, #khat_acceptor{listen_socket = ListenSocket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #khat_acceptor{}) ->
    {reply, Reply :: term(), NewState :: #khat_acceptor{}} |
    {reply, Reply :: term(), NewState :: #khat_acceptor{}, timeout() | hibernate} |
    {noreply, NewState :: #khat_acceptor{}} |
    {noreply, NewState :: #khat_acceptor{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #khat_acceptor{}} |
    {stop, Reason :: term(), NewState :: #khat_acceptor{}}).
handle_call(_Request, _From, State) ->
    {reply, error, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #khat_acceptor{}) ->
    {noreply, NewState :: #khat_acceptor{}} |
    {noreply, NewState :: #khat_acceptor{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #khat_acceptor{}}).
handle_cast(accept, State) ->
    #khat_acceptor{listen_socket = ListenSocket} = State,
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            {ok, PeerName} = inet:peername(Socket),
            ?DEBUG("Accepted connection from client ~p", [PeerName]),
            % it isn't necessary to start client worker under the supervisor - it isn't restarted
            %% {ok, Pid} = khat_client_sup:add_child(Socket),
            {ok, Pid} = khat_client_worker:start(Socket),
            ok = gen_tcp:controlling_process(Socket, Pid),
            ok = inet:setopts(Socket, [{active, true}]),
            ok = accept_connections(),
            {noreply, State};
        {error, Reason} ->
            ?ERROR("Couldn't accept connection on socket ~p: ~p", [ListenSocket, Reason]),
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
-spec(handle_info(Info :: timeout() | term(), State :: #khat_acceptor{}) ->
    {noreply, NewState :: #khat_acceptor{}} |
    {noreply, NewState :: #khat_acceptor{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #khat_acceptor{}}).
handle_info(Info, State) ->
    ?WARN("Received unexpected message ~p", [Info]),
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
    State :: #khat_acceptor{}) -> term()).
terminate(Reason, _State) ->
    ?WARN("TCP acceptor terminating with ~p reason", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #khat_acceptor{},
    Extra :: term()) ->
    {ok, NewState :: #khat_acceptor{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec accept_connections() -> ok.
accept_connections() ->
    gen_server:cast(self(), accept).