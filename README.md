# khat
Simple chat server with group subscriptions and registration of the clients.

Overview
--------
Messages starting with a `\` character are special commands described in the commands section below. All other data sent from the client is broadcasted to all clients connected to the server.

Commands
--------
Register client with a new name. When user is unregistered he can only receive messages but can't send any.
```erlang
\register\Client Name
```

Subscribe to a given group. Any messages sent to this group by any of the clients will be sent to the subscribed client.
```erlang
\subscribe\Group Name
```

Unsubscribe from a given group. No more messages destined for this group will be sent to the client.
```erlang
\unsubscribe\Group Name
```

Send message to the given group. Only clients subscribed to this group will receive this message.
```erlang
\group=Group Name\Some message
```

Send a keepalive packet. If client isn't sending any messages he has to send keepalive packets in order to remain connected to the server.
```erlang
\alive\
```

Configuration
-------------
Configuration file is placed in rel/files/sys.config. There are following configuration options enabled:

 * port - TCP port used by khat server
 * acceptors - number of acceptor processes waiting for a TCP connection establishment
 * inactivity_timeout - timeout (in seconds) after which the TCP connection is closed if client hasn't sent any messages or alive packets
