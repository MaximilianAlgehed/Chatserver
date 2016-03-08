-module(server).
-export([start/1]).


%Broker messages in the chat system
broker(Clients)  ->
    receive
        {disconnect, Pid} -> io:format("Client at ~p disconnected~n", [Pid]),
                             broker(Clients -- [Pid]);

        {connect, Pid}    -> io:format("Client at ~p connected~n", [Pid]),
                             broker([Pid|Clients]);

        {broadcast, Msg}  -> lists:map(fun(Pid) -> Pid ! {send, Msg} end, Clients),
                             broker(Clients)
    end.

%Connect a receiver to the broker
connect(Receiver, Broker) -> Broker ! {connect, Receiver}.

%Disconnect a receiver from the broker
disconnect(Receiver, Broker) -> Broker ! {disconnect, Receiver}.

%Send a message to all receivers on a broker
broadcast(Msg, Broker)    -> Broker ! {broadcast, Msg}.

%Local client message broker
%Handles:
%   socketListener -> broker
%   broker -> socketSend
client_broker(Sock, Broker) ->
    receive
        {send, Msg}     -> client_send(Sock, Msg);

        {fromSock, Msg} -> broadcast(Msg, Broker);

        disconnect      -> disconnect(self(), Broker),
                           exit(disconnect)

    end,
    client_broker(Sock, Broker).

%Send a message to the socket
client_send(Sock, Msg) -> gen_tcp:send(Sock, Msg).

%Get a message from the socket
client_recv(Msg, Broker) -> Broker ! {fromSock, Msg}.

%Disconnect from the server
client_disconnect(Broker) -> Broker ! disconnect.

%Listen for complete messages from the socket
client_listen(Sock, CBroker) -> 
    client_listen_loop(Sock, CBroker, []).

%Loop the listen
client_listen_loop(Sock, CBroker, Lst) ->
    case client_get_message(Sock, Lst) of
        {ok, Msg, Cont} -> client_recv(Msg, CBroker),
                           client_listen_loop(Sock, CBroker, Cont);

        error     -> client_disconnect(CBroker)
    end.

%Listen on a socket untill a 0 byte is received
client_get_message(Sock, Bytes) ->
    case lists:any(fun(X) -> X == 0 end, Bytes) of
        true -> get_message(Bytes);
        false -> case gen_tcp:recv(Sock, 0) of
                    {ok, Data} -> Msg = Bytes ++ binary:bin_to_list(Data),
                                  case lists:any(fun(X) -> X == 0 end, Msg) of
                                      true  -> get_message(Msg);
                                      false -> client_get_message(Sock, Msg)
                                  end;

                    {error, _} -> error
                  end
    end.

%Get message
get_message(Bytes) ->
    {ok,
     lists:takewhile(fun(X) -> X /= 0 end, Bytes) ++ [0],
     lists:dropwhile(
       fun(X) ->
               X == 0 end,
       (lists:dropwhile(fun(X) -> X /= 0 end, Bytes)))
    }. 

%Start a client process
start_client(Sock, Broker) ->
    {CBroker, _} = client_broker_listener(Sock, Broker),
    receive
        {'DOWN', _, _, CBroker, disconnect} -> ok;
        {'DOWN', _, _, CBroker, _} -> disconnect(CBroker, Broker)
    end.

%Spawn the client listener and broker
client_broker_listener(Sock, Broker) ->
    spawn_monitor(
        fun() -> MyPid = self(),
                 connect(MyPid, Broker),
                 spawn_link(fun() ->
                                    link(MyPid),
                                    client_listen(Sock, MyPid)
                            end),
                 client_broker(Sock, Broker)
        end
    ).

%Listen for a new client connecting
server_listen(LSock, Broker) ->
    {ok, CSock} = gen_tcp:accept(LSock),
    spawn(fun() -> start_client(CSock, Broker) end),
    server_listen(LSock, Broker).

%Start the server application
start(Port) ->
    Broker      = spawn(fun() -> broker([]) end),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
    Listener    = spawn(fun() -> server_listen(LSock, Broker) end),
    {{broker, Broker}, {listener, Listener}}.
