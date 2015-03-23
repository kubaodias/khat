%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2015 17:10
%%%-------------------------------------------------------------------
-module(khat_protocol).

%% Include files
-include("../include/khat.hrl").

%% API
-export([
    parse_msg/1
]).

-spec parse_msg(Msg :: list()) -> {khat_msg_type(), Data :: list()} | error.
parse_msg([$\\,$r,$e,$g,$i,$s,$t,$e,$r,$\\ | ClientName]) ->
    {register, ClientName};
parse_msg([$\\,$s,$u,$b,$s,$c,$r,$i,$b,$e,$\\ | GroupName]) ->
    {subscribe, GroupName};
parse_msg([$\\,$u,$n,$s,$u,$b,$s,$c,$r,$i,$b,$e,$\\ | GroupName]) ->
    {unsubscribe, GroupName};
parse_msg([$\\,$g,$r,$o,$u,$p,$= | GroupNameAndData]) ->
    case string:chr(GroupNameAndData, $\\) of
        0 ->
            error;
        N ->
            GroupName = string:left(GroupNameAndData, N - 1),
            Data = string:right(GroupNameAndData, length(GroupNameAndData) - N),
            {{group, GroupName}, Data}
    end;
parse_msg([$\\,$a,$l,$i,$v,$e,$\\ | _]) ->
    {alive, ""};
parse_msg([$\\ | _InvalidType]) ->
    error;
parse_msg(Msg) ->
    {msg, Msg}.