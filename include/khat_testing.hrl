%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2015 17:07
%%%-------------------------------------------------------------------

-ifndef(khat_testing_hrl).
-define(khat_testing_hrl, true).

-ifdef(TEST).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(LOOPER, khat_testing:looper).
-define(MOCK,   khat_testing:mock).
-define(UNMOCK, khat_testing:unmock).

-endif.

-endif.