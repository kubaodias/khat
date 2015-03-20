%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2015 17:26
%%%-------------------------------------------------------------------
-module(khat_acceptor_sup).

-behaviour(supervisor).

%% Includes
-include("../include/khat_logger.hrl").

%% Definitions
-define(DEFAULT_ACCEPTOR_COUNT, 15).

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
    SupFlags = {one_for_one, 10, 60},
    AcceptorCount = khat_config:get_value(acceptors, ?DEFAULT_ACCEPTOR_COUNT),
    ?DEBUG("Starting khat acceptor supervisor..."),
    {ok, { SupFlags, [
        {{khat_acceptor, N}, {khat_acceptor, start_link, []}, permanent, 10000, worker, [khat_acceptor]}
            || N <- lists:seq(1, AcceptorCount)
    ]} }.

