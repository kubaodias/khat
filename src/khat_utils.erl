%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2015 17:10
%%%-------------------------------------------------------------------
-module(khat_utils).

%% API
-export([
    list_to_atom/1
]).

-spec list_to_atom(List :: list()) -> atom().
list_to_atom(List) ->
    try
        list_to_existing_atom(List)
    catch error:badarg ->
        erlang:list_to_atom(List)
    end.