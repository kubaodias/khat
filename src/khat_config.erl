%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2015 17:10
%%%-------------------------------------------------------------------
-module(khat_config).

%% API
-export([
    get_value/1,
    get_value/2
]).

get_value(Key) ->
    get_value(Key, undefined).

get_value(Key, Default) ->
    AppEnv = application:get_all_env(khat),
    proplists:get_value(AppEnv, Key, Default).

