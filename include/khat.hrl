%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2015 17:07
%%%-------------------------------------------------------------------

-ifndef(khat_hrl).
-define(khat_hrl, true).

-define(DEFAULT_PORT, 6667).

-type khat_client_name()    :: nonempty_string().
-type khat_channel_name()   :: nonempty_string().

-record(khat_listener, {
    port    :: pos_integer(),
    socket  :: port()
}).

-record(khat_client, {
    name    :: khat_client_name(),
    socket  :: port()
}).

-record(khat_channel, {
    name    :: khat_channel_name(),
    clients = [] :: [pid()]
}).


-endif.