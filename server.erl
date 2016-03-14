-module(server).
-export([start/1]).


%Broker messages in the chat system
broker(Clients)  ->
    receive
        {disconnect, Client} -> io:format("Client at ~p disconnected~n", [Client]),
                             broker(Clients -- [Client]);

        {connect, Client}    -> io:format("Client at ~p connected~n", [Client]),
                             broker([Client|Clients]);

        {broadcast, Msg}  -> io:format("Message: ~s~n", [Msg]),
                             lists:map(fun({Pid, _}) -> Pid ! {send, Msg} end, Clients),
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
client_broker(ClientKey, Sock, Broker) ->
    receive
        {send, Msg}     -> client_send(ClientKey, Sock, Msg);

        {fromSock, Msg} -> broadcast(Msg, Broker);

        disconnect      -> disconnect({self(), ClientKey}, Broker),
                           exit(disconnect)

    end,
    client_broker(ClientKey, Sock, Broker).

%Decrypt a message using the private key
decrypt_with(Msg, PrivateKey) ->
    public_key:decrypt_private(hex:hexstr_to_bin(Msg), PrivateKey).

%Encrypt a message using the public key
encrypt_with(Msg, PublicKey) ->
    hex:bin_to_hexstr(public_key:encrypt_public(Msg, PublicKey)).

split_n(N, Lst) -> lists:take(N, Lst)++split_n(N, lists:drop(Lst)).

%Send a message to the socket
client_send(ClientKey, Sock, Msg) -> 
    Messages = lists:map(fun(M) -> encrypt_with(M, ClientKey) end, split_n(10, Msg)),
    lists:map(fun(M) -> gen_tcp:send(Sock, M++[0]) end, Messages),
    gen_tcp:send(Sock, encrypt_with([0], ClientKey)++[0]).

%Get a message from the socket
client_recv(Msg, Broker) -> Broker ! {fromSock, Msg}.

%Disconnect from the server
client_disconnect(Broker) -> Broker ! disconnect.

%Listen for complete messages from the socket
client_listen(PrivateKey, Sock, CBroker) -> 
    client_listen_loop(PrivateKey, Sock, CBroker, [], []).

%Loop the listen
client_listen_loop(PrivateKey, Sock, CBroker, Lst, Msgs) ->
    case client_get_message(Sock, Lst) of
        {ok, Msg, Cont} -> Message = decrypt_with(Msg, PrivateKey),
                           case Message of 
                                [0] -> client_recv(Msgs, CBroker),
                                       client_listen_loop(PrivateKey, Sock, CBroker, Cont, []);
                                _   -> client_listen_loop(PrivateKey, Sock, CBroker, Cont, Msgs ++ Message)
                            end;

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
     lists:takewhile(fun(X) -> X /= 0 end, Bytes),
     lists:dropwhile(
       fun(X) ->
               X == 0 end,
       (lists:dropwhile(fun(X) -> X /= 0 end, Bytes)))
    }. 

%Start a client process
start_client(PrivateKey, PublicKey, Sock, Broker) ->
    {CBroker, _} = client_broker_listener(PrivateKey, PublicKey, Sock, Broker),
    receive
        {'DOWN', _, _, CBroker, disconnect} -> ok;
        {'DOWN', _, _, CBroker, _} -> disconnect(CBroker, Broker)
    end.

%Get the public key from the client
client_get_key(Sock) ->
    {ok, Message, []} = client_get_message(Sock, []),
    [Entry] = public_key:pem_decode(binary:list_to_bin(Message)),
    public_key:pem_entry_decode(Entry).

%Spawn the client listener and broker
client_broker_listener(PrivateKey, PublicKey, Sock, Broker) ->
    spawn_monitor(
        fun() -> MyPid = self(),
                 ClientKey = client_get_key(Sock),
                 gen_tcp:send(Sock, hex:bin_to_hexstr(PublicKey)++[0]),
                 connect({MyPid, ClientKey}, Broker),
                 spawn_link(fun() ->
                                    link(MyPid),
                                    client_listen(PrivateKey, Sock, MyPid)
                            end),
                 client_broker(ClientKey, Sock, Broker)
        end
    ).

%Listen for a new client connecting
server_listen(PrivateKey, PublicKey, LSock, Broker) ->
    {ok, CSock} = gen_tcp:accept(LSock),
    spawn(fun() -> start_client(PrivateKey, PublicKey, CSock, Broker) end),
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
    Listener     = spawn(fun() -> server_listen(PrivKey, PubKey, LSock, Broker) end),
    {{broker, Broker}, {listener, Listener}}.
