%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2015 17:38
%%%-------------------------------------------------------------------
-module(khat_group_tests).

-ifdef(TEST).

%% Includes
-include("../../include/khat_logger.hrl").
-include("../../include/khat_testing.hrl").

%% Definitions
-define(GROUP_NAME, "test").

%%%===================================================================
%%% Test functions
%%%===================================================================
khat_group_test_() ->
    {foreach,
        fun() ->
            ?MOCK(khat_client_worker, [{send, fun(Pid, Data) -> Pid ! Data, ok end}]),
            {ok, PID} = khat_group:start(?GROUP_NAME),
            PID
        end,
        fun(PID) ->
            true = erlang:unlink(PID),
            ok = khat_group:stop(?GROUP_NAME),
            ?UNMOCK()
        end,
        [
            {"Subscribe test", fun() -> subscribe_eunit() end},
            {"Unsubscribe test", fun() -> unsubscribe_eunit() end},
            {"Subscriber terminated test", fun() -> subscriber_terminated_eunit() end},
            {"Broadcast test", fun() -> broadcast_eunit() end},
            {"Ensure group started test", fun() -> ensure_started_eunit() end}
        ]}.

subscribe_eunit() ->
    PID1 = start_subscriber(),
    PID2 = start_subscriber(),
    ok = assert_client_count(0, ?GROUP_NAME),
    ok = subscribe_to_group(PID1, ?GROUP_NAME),
    ok = assert_client_count(1, ?GROUP_NAME),
    % subscribe the same client again
    ok = subscribe_to_group(PID1, ?GROUP_NAME),
    ok = assert_client_count(1, ?GROUP_NAME),
    ok = subscribe_to_group(PID2, ?GROUP_NAME),
    ok = assert_client_count(2, ?GROUP_NAME).

unsubscribe_eunit() ->
    PID1 = start_subscriber(),
    PID2 = start_subscriber(),
    ok = subscribe_to_group(PID1, ?GROUP_NAME),
    ok = subscribe_to_group(PID2, ?GROUP_NAME),
    ok = assert_client_count(2, ?GROUP_NAME),
    ok = unsubscribe_from_group(PID1, ?GROUP_NAME),
    ok = assert_client_count(1, ?GROUP_NAME),
    % unsubscribe the same client again
    ok = unsubscribe_from_group(PID1, ?GROUP_NAME),
    ok = assert_client_count(1, ?GROUP_NAME),
    ok = unsubscribe_from_group(PID2, ?GROUP_NAME),
    ok = assert_client_count(0, ?GROUP_NAME),
    % subscribe one of the clients again
    ok = subscribe_to_group(PID1, ?GROUP_NAME),
    ok = assert_client_count(1, ?GROUP_NAME).

subscriber_terminated_eunit() ->
    PID1 = start_subscriber(),
    PID2 = start_subscriber(),
    ok = subscribe_to_group(PID1, ?GROUP_NAME),
    ok = subscribe_to_group(PID2, ?GROUP_NAME),
    ok = assert_client_count(2, ?GROUP_NAME),
    % stop one of the clients
    ok = stop_subscriber(PID2),
    ok = assert_client_count(1, ?GROUP_NAME).

broadcast_eunit() ->
    Data1 = <<"d1">>,
    Data2 = <<"d2">>,
    PID1 = start_subscriber(),
    PID2 = start_subscriber(),
    ok = subscribe_to_group(PID1, ?GROUP_NAME),
    ok = subscribe_to_group(PID2, ?GROUP_NAME),
    ok = assert_client_count(2, ?GROUP_NAME),
    ok = khat_group:broadcast(?GROUP_NAME, Data1),
    ok = assert_data_received(PID1, [Data1]),
    ok = assert_data_received(PID2, [Data1]),
    ok = unsubscribe_from_group(PID2, ?GROUP_NAME),
    ok = assert_client_count(1, ?GROUP_NAME),
    ok = khat_group:broadcast(?GROUP_NAME, Data2),
    ok = assert_data_received(PID1, [Data2, Data1]),
    ok = assert_data_received(PID2, [Data1]).

ensure_started_eunit() ->
    % group should be started when khat_group:subscribe/1 is called
    ExtraGroup = "extra",
    ok = assert_client_count(undefined, ExtraGroup),
    PID = start_subscriber(),
    ok = subscribe_to_group(PID, ExtraGroup),
    ok = assert_client_count(1, ExtraGroup).

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_subscriber() ->
    spawn_link(fun() -> subscriber_function([]) end).

stop_subscriber(PID) ->
    PID ! stop,
    ok.

subscribe_to_group(PID, GroupName) ->
    PID ! {subscribe, GroupName},
    ok.

unsubscribe_from_group(PID, GroupName) ->
    PID ! {unsubscribe, GroupName},
    ok.

subscriber_function(DataAcc) ->
    receive
        {subscribe, GroupName} ->
            ok = khat_group:subscribe(GroupName),
            subscriber_function(DataAcc);
        {unsubscribe, GroupName} ->
            ok = khat_group:unsubscribe(GroupName),
            subscriber_function(DataAcc);
        {get_data, Pid} ->
            Pid ! {data, DataAcc},
            subscriber_function(DataAcc);
        stop ->
            ok;
        Data ->
            NewDataAcc = [Data | DataAcc],
            subscriber_function(NewDataAcc)
    end.

assert_client_count(Expected, GroupName) ->
    ?LOOPER(fun() ->
                ?assertEqual(Expected, khat_group:client_count(GroupName))
            end).

assert_data_received(PID, ExpectedData) ->
    ?LOOPER(fun() ->
                PID ! {get_data, self()},
                receive {data, Data} ->
                    ?assertEqual(ExpectedData, Data)
                end
            end).

-endif.