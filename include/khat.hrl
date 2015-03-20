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

-define(GLOBAL_GROUP, "global").

-type khat_client_name()    :: nonempty_string().
-type khat_group_name()     :: nonempty_string().
-type khat_msg_type()       :: msg | register | subscribe | unsubscribe.

-record(khat_listener, {
    port    :: pos_integer(),
    listen_socket :: port()
}).

-record(khat_acceptor, {
    listen_socket :: port()
}).

-record(khat_client, {
    name    :: khat_client_name() | undefined,
    socket  :: port()
}).

-record(khat_group, {
    name    :: khat_group_name(),
    tab     :: ets:tid()
}).


-endif.