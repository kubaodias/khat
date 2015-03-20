%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2015 17:26
%%%-------------------------------------------------------------------
-module(khat_sup).

-behaviour(supervisor).

%% Includes
-include("../include/khat_logger.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SupFlags = {one_for_all, 5, 60},
    ?DEBUG("Starting khat supervisor..."),
    {ok, { SupFlags, [
        {khat_client_sup, {khat_client_sup, start_link, []}, permanent, infinity, supervisor, [khat_client_sup]},
        {khat_listener, {khat_listener, start_link, []}, permanent, 10000, worker, [khat_listener]}
    ]} }.

