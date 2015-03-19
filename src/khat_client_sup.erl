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
-include("../include/logger.hrl").

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

-spec add_child(Name :: atom()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
add_child(Name) ->
    supervisor:start_child(?MODULE, {Name, {khat_client_worker, start_link, [Name]},
        transient, 10000, worker, [khat_client_worker]}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ?DEBUG("Starting khat client supervisor..."),
    {ok, { {one_for_one, 10, 60}, []} }.

