%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2015 17:26
%%%-------------------------------------------------------------------
-module(khat_client_sup).

-behaviour(supervisor).

%% Includes
-include("../include/khat_logger.hrl").

%% API
-export([
    start_link/0,
    add_child/1
]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec add_child(Socket :: port()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
add_child(Socket) ->
    Id = khat_utils:get_timestamp(),
    supervisor:start_child(?MODULE, {Id, {khat_client_worker, start_link, [Socket]},
                                     transient, 10000, worker, [khat_client_worker]}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ?DEBUG("Starting khat client supervisor..."),
    {ok, { {one_for_one, 10, 60}, []} }.

