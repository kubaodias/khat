%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2015 17:05
%%%-------------------------------------------------------------------
-module(khat_listener).

-behaviour(gen_server).

%% Includes
-include("../include/khat.hrl").
-include("../include/khat_logger.hrl").

%% API
-export([
    start_link/0,
    get_socket/0
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
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_socket() -> {ok, ListenSocket :: port()}.
get_socket() ->
    gen_server:call(?MODULE, get_socket).

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
    {ok, State :: #khat_listener{}} | {ok, State :: #khat_listener{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    Port = khat_config:get_value(port, ?DEFAULT_PORT),
    Opts = [binary, {reuseaddr, true}, {keepalive, true}, {active, false}],
    case gen_tcp:listen(Port, Opts) of
        {ok, ListenSocket} ->
            ?INFO("Started listening on TCP port ~p", [Port]),
            {ok, #khat_listener{port = Port, listen_socket = ListenSocket}};
        {error, Reason} ->
            ?ERROR("Couldn't start TCP listener: ~p", [Reason]),
            {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #khat_listener{}) ->
    {reply, Reply :: term(), NewState :: #khat_listener{}} |
    {reply, Reply :: term(), NewState :: #khat_listener{}, timeout() | hibernate} |
    {noreply, NewState :: #khat_listener{}} |
    {noreply, NewState :: #khat_listener{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #khat_listener{}} |
    {stop, Reason :: term(), NewState :: #khat_listener{}}).
handle_call(get_socket, _From, State) ->
    #khat_listener{listen_socket = ListenSocket} = State,
    {reply, {ok, ListenSocket}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #khat_listener{}) ->
    {noreply, NewState :: #khat_listener{}} |
    {noreply, NewState :: #khat_listener{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #khat_listener{}}).
handle_cast(Request, State) ->
    ?WARN("Received unexpected request ~p", [Request]),
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
-spec(handle_info(Info :: timeout() | term(), State :: #khat_listener{}) ->
    {noreply, NewState :: #khat_listener{}} |
    {noreply, NewState :: #khat_listener{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #khat_listener{}}).
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
    State :: #khat_listener{}) -> term()).
terminate(Reason, _State) ->
    ?WARN("TCP listener terminating with ~p reason", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #khat_listener{},
    Extra :: term()) ->
    {ok, NewState :: #khat_listener{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
