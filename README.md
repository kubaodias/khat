# khat
[![Build Status](https://img.shields.io/github/workflow/status/kubaodias/khat/Erlang%20CI)](https://github.com/kubaodias/khat/actions?query=workflow%3A%22Erlang+CI%22)

Simple chat server with group subscriptions and registration of the clients.

## Developer guide

### Compile
```
make compile
```

### Run unit tests
```
make test
```

### Run application
```
make run
```

## Configuration
Configuration file is placed in `sys.config` file. There are following configuration options enabled:

 * port - TCP port used by khat server (default is 6667)
 * acceptors - number of acceptor processes waiting for a TCP connection establishment
 * inactivity_timeout - timeout (in seconds) after which the TCP connection is closed if client hasn't sent any messages or alive packets

## Usage

### Connect to the server
When server is running you can connect to it via telnet:
```
telnet 127.0.0.1 6667
```

### Commands
Messages starting with a `\` character are special commands described in the commands section below. All other data sent from the client is broadcasted to all clients connected to the server.

#### Register client with a new name
When user is unregistered then messages can only be received but not sent.
```erlang
\register\Client Name
```

#### Subscribe to a given group
Any messages sent to group `Group Name` in the example below by any of the clients will be sent to the subscribed client.
```erlang
\subscribe\Group Name
```

#### Unsubscribe from a given group
No more messages destined for this group will be sent to the client.
```erlang
\unsubscribe\Group Name
```

#### Send message to the given group
Only clients subscribed to this group will receive this message.
```erlang
\group=Group Name\Some message
```

#### Send a keepalive packet
If client isn't sending any messages then keepalive packets needs to be sent in order to remain connected to the server. If there's no data sent by the client within `inactivity_timeout` seconds then the client will be disconnected from the server.
```erlang
\alive\
```
