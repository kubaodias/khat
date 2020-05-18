%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2015 17:38
%%%-------------------------------------------------------------------
-module(khat_protocol_tests).

-ifdef(TEST).

%% Includes
-include("../include/khat_logger.hrl").
-include("../include/khat_testing.hrl").

%% Definitions

%%%===================================================================
%%% Test functions
%%%===================================================================

parse_msg_test() ->
    ?assertEqual({msg, "hello world!"}, khat_protocol:parse_msg("hello world!")),
    ?assertEqual({register, "John Doe"}, khat_protocol:parse_msg("\\register\\John Doe")),
    ?assertEqual({subscribe, "News"}, khat_protocol:parse_msg("\\subscribe\\News")),
    ?assertEqual({unsubscribe, "Weather Channel"}, khat_protocol:parse_msg("\\unsubscribe\\Weather Channel")),
    ?assertEqual({{group, "News"}, "This is it!"}, khat_protocol:parse_msg("\\group=News\\This is it!")),
    ?assertEqual({alive, ""}, khat_protocol:parse_msg("\\alive\\Not important")),
    ?assertEqual(error, khat_protocol:parse_msg("\\group=Weather Channel")),
    ?assertEqual(error, khat_protocol:parse_msg("\\unregister\\user")).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-endif.