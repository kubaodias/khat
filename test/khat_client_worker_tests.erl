%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. mar 2015 17:38
%%%-------------------------------------------------------------------
-module(khat_client_worker_tests).

-ifdef(TEST).

%% Includes
-include("../include/khat.hrl").
-include("../include/khat_logger.hrl").
-include("../include/khat_testing.hrl").

%% Definitions
-define(CLIENT, client_name).

%%%===================================================================
%%% Test functions
%%%===================================================================
khat_client_worker_test_() ->
    {foreach,
        fun() ->
            ?MOCK(khat_group, [{subscribe, fun(_GroupName) -> ok end},
                               {broadcast, fun(_GroupName, _Data) -> ok end}]),
            ?MOCK(calendar, [{now_to_local_time, fun(_Now) -> calendar:now_to_universal_time({0,0,0}) end}]),
            {ok, PID} = khat_client_worker:start_link(socket, ?DEFAULT_INACTIVITY_TIMEOUT),
            ok = send(PID, <<"\\register\\client_name\r\n">>),
            ok = synchronize_client_worker(PID),
            PID
        end,
        fun(PID) ->
            true = erlang:unlink(PID),
            ok = khat_client_worker:stop(PID),
            ?UNMOCK()
        end,
        [
            {"Single message", fun() -> single_message_eunit() end},
            {"Multiple messages", fun() -> multiple_messages_eunit() end},
            {"Multiple messages streamed", fun() -> multiple_messages_streamed_eunit() end},
            {"Message split", fun() -> message_split_eunit() end},
            {"Next message split", fun() -> next_message_split_eunit() end},
            {"Empty message is not broadcasted", fun() -> empty_message_eunit() end}
        ]}.

single_message_eunit() ->
    ok = send(<<"hello\r\n">>),
    ok = assert_received([<<"hello\r\n">>]).

multiple_messages_eunit() ->
    ok = send(<<"hello\r\n">>),
    ok = send(<<"how are you?\r\n">>),
    ok = assert_received([<<"hello\r\n">>, <<"how are you?\r\n">>]).

multiple_messages_streamed_eunit() ->
    ok = send(<<"hello\r\nhow are you?\r\n">>),
    ok = assert_received([<<"hello\r\n">>, <<"how are you?\r\n">>]).

message_split_eunit() ->
    ok = send(<<"Houston, we have a prob">>),
    ok = assert_received([]),
    ok = send(<<"lem\r\n">>),
    ok = assert_received([<<"Houston, we have a problem\r\n">>]).

next_message_split_eunit() ->
    ok = send(<<"First line\r\nSecond">>),
    ok = assert_received([<<"First line\r\n">>]),
    ok = send(<<" line\r\n">>),
    ok = assert_received([<<"Second line\r\n">>]).

empty_message_eunit() ->
    ok = send(<<"\r\n">>),
    ok = assert_received([]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

send(Data) ->
    send(?CLIENT, Data).

send(Process, Data) ->
    Process ! {tcp, socket, Data},
    ok.

synchronize_client_worker() ->
    synchronize_client_worker(?CLIENT).

synchronize_client_worker(Process) ->
    _ = sys:get_status(Process),
    ok.

assert_received(ExpectedData) ->
    ok = synchronize_client_worker(),
    Data = [BroadcastData || {_Pid, {khat_group, broadcast, [?GLOBAL_GROUP, BroadcastData]}, ok} <- meck:history(khat_group)],
    ?assertEqual([<<"1970-01-01,00:00:00 client_name: ", ExpData/binary>> || ExpData <- ExpectedData], Data),
    ok = meck:reset(khat_group).

-endif.