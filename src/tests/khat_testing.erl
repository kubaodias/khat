%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2015 17:10
%%%-------------------------------------------------------------------
-module(khat_testing).

-ifdef(TEST).

%% API
-export([
    looper/1,
    looper/3,
    mock/2,
    unmock/0
]).

-spec looper(Fun :: fun()) -> any().
looper(Fun) ->
    looper(Fun, 50, 5).

-spec looper(Fun :: fun(), Sleep :: pos_integer(), Count :: non_neg_integer()) -> any().
looper(Fun, _Sleep, 0) ->
    Fun();
looper(Fun, Sleep, Count) ->
    try
        Fun()
    catch error:{assertEqual_failed,_} ->
        timer:sleep(Sleep),
        looper(Fun, Sleep, Count - 1)
    end.

mock(Module, Functions) ->
    ok = meck:new(Module),
    mock_functions(Module, Functions).

unmock() ->
    _ = meck:unload(),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

mock_functions(_Module, []) ->
    ok;
mock_functions(Module, [{FunctionName, Result} | RestOfFunctions]) ->
    ok = meck:expect(Module, FunctionName, Result),
    mock_functions(Module, RestOfFunctions).

-endif.