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
parse_msg([$\\,$s,$u,$b,$s,$c,$r,$i,$b,$e,$\\ | ChannelName]) ->
    {subscribe, ChannelName};
parse_msg([$\\,$u,$n,$s,$u,$b,$s,$c,$r,$i,$b,$e,$\\ | ChannelName]) ->
    {unsubscribe, ChannelName};
parse_msg([$\\,$c,$h,$a,$n,$n,$e,$l,$\\ | ChannelNameAndData]) ->
    case string:chr(ChannelNameAndData, $\\) of
        0 ->
            error;
        N ->
            ChannelName = string:left(ChannelNameAndData, N - 1),
            Data = string:right(ChannelNameAndData, length(ChannelNameAndData) - N),
            {{channel, ChannelName}, Data}
    end;
parse_msg([$\\ | _InvalidType]) ->
    error;
parse_msg(Msg) ->
    {msg, Msg}.