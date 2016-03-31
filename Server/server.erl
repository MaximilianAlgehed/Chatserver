-module(server).
-export([main/0, start/1, connect/2, disconnect/2, broadcast/2]).

main() -> start(5678).

%Broker messages in the chat system
broker(Clients)  ->
    receive
        {disconnect, Client} -> io:format("Client at ~p disconnected~n", [Client]),
                             broker(Clients -- [Client]);

        {connect, Client}    -> io:format("Client at ~p connected~n", [Client]),
                             broker([Client|Clients]);

        {broadcast, Msg}  -> io:format("Message: ~s~n", [Msg]),
                             lists:map(fun(Pid) -> Pid ! {send, Msg} end, Clients),
                             broker(Clients)
    end.

%Connect a receiver to the broker
connect(Receiver, Broker) -> Broker ! {connect, Receiver}.

%Disconnect a receiver from the broker
disconnect(Receiver, Broker) -> Broker ! {disconnect, Receiver}.

%Send a message to all receivers on a broker
broadcast(Msg, Broker)    -> Broker ! {broadcast, Msg}.

%Listen for a new client connecting
server_listen(PrivateKey, PublicKey, LSock, Broker) ->
    case gen_tcp:accept(LSock) of
        {ok, CSock} -> spawn(fun() -> client:start_client(PrivateKey, PublicKey, CSock, Broker) end);
        {error, Error} -> io:format("Error: ~p~n", [Error])
    end,
    server_listen(PrivateKey, PublicKey, LSock, Broker).

%Get the private key from file
read_key(KeyFile) ->
    {ok, PemBin} = file:read_file(KeyFile),
    [Entry]      = public_key:pem_decode(PemBin),
    public_key:pem_entry_decode(Entry).

%Start the server application
start(Port) ->
    PrivKey      = read_key("rsa_priv.pem"), 
    {ok, PubKey} = file:read_file("rsa_pub.pem"), 
    Broker       = spawn(fun() -> broker([]) end),
    {ok, LSock}  = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
    Listener     = spawn(fun() -> gen_tvp:constrolling_process(LSock, self()),
                                  server_listen(PrivKey, PubKey, LSock, Broker) end),
    {{broker, Broker}, {listener, Listener}}.
