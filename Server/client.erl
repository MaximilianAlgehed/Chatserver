-module(client).
-export([start_client/4]).

%Decrypt a message using the private key
decrypt_with(Msg, PrivateKey) ->
    public_key:decrypt_private(base64:decode(Msg), PrivateKey).

%Encrypt a message using the public key
encrypt_with(Msg, PublicKey) ->
    base64:encode(public_key:encrypt_public(Msg, PublicKey)).

take(0, _) -> [];
take(_, []) -> [];
take(N, [H|T]) -> [H|take(N-1, T)].

drop(0, Lst) -> Lst;
drop(_, []) -> [];
drop(N, [_|T]) -> drop(N-1, T).

split_n(_, []) -> [];
split_n(N, Lst) -> [binary:list_to_bin(take(N, Lst))|split_n(N, drop(N, Lst))].

%Local client message broker
%Handles:
%   socketListener -> broker
%   broker -> socketSend
broker(ClientKey, Sock, Broker) ->
    receive
        {send, Msg}     -> send(ClientKey, Sock, Msg);

        {fromSock, Msg} -> server:broadcast(Msg, Broker);

        disconnect      -> server:disconnect(self(), Broker),
                           exit(disconnect)
    end,
    broker(ClientKey, Sock, Broker).

%Send a message to the socket
send(ClientKey, Sock, Msg) -> 
    Messages = lists:map(fun(M) -> encrypt_with(M, ClientKey) end, split_n(10, binary:bin_to_list(Msg))),
    lists:foreach(fun(M) -> gen_tcp:send(Sock, M++[0]) end, Messages),
    gen_tcp:send(Sock, encrypt_with(<<0>>, ClientKey)++[0]).

%Get a message from the socket
recv(Msg, Broker) -> Broker ! {fromSock, Msg}.

%Disconnect from the server
disconnect(Broker) -> Broker ! disconnect.

%Listen for complete messages from the socket
listen(PrivateKey, Sock, CBroker) -> 
    listen_loop(PrivateKey, Sock, CBroker, [], <<>>).

%Loop the listen
listen_loop(PrivateKey, Sock, CBroker, Lst, Msgs) ->
    case get_message(Sock, Lst) of
        {ok, Msg, Cont} -> Message = decrypt_with(Msg, PrivateKey),
                           case Message of 
                                <<0>> -> recv(Msgs, CBroker),
                                         listen_loop(PrivateKey, Sock, CBroker, Cont, <<>>);
                                _     -> listen_loop(PrivateKey, Sock, CBroker, Cont, <<Msgs/binary, Message/binary>>)
                            end;

        error     -> disconnect(CBroker)
    end.

%Listen on a socket untill a 0 byte is received
get_message(Sock, Bytes) ->
    case lists:any(fun(X) -> X == 0 end, Bytes) of
        true -> get_message(Bytes);
        false -> case gen_tcp:recv(Sock, 0) of
                    {ok, Data} -> Msg = Bytes ++ binary:bin_to_list(Data),
                                  case lists:any(fun(X) -> X == 0 end, Msg) of
                                      true  -> get_message(Msg);
                                      false -> get_message(Sock, Msg)
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
    {CBroker, _} = broker_listener(PrivateKey, PublicKey, Sock, Broker),
    receive
        {'DOWN', _, _, CBroker, disconnect} -> ok;
        {'DOWN', _, _, CBroker, _} -> server:disconnect(CBroker, Broker)
    end.

%Get the public key from the client
get_key(Sock) ->
    {ok, Message, []} = get_message(Sock, []),
    [Entry] = public_key:pem_decode(binary:list_to_bin(Message)),
    public_key:pem_entry_decode(Entry).

%Spawn the client listener and broker
broker_listener(PrivateKey, PublicKey, Sock, Broker) ->
    spawn_monitor(
        fun() -> MyPid = self(),
                 ClientKey = get_key(Sock),
                 gen_tcp:send(Sock, base64:encode(PublicKey)++[0]),
                 server:connect(MyPid, Broker),
                 spawn_link(fun() ->
                                    link(MyPid),
                                    listen(PrivateKey, Sock, MyPid)
                            end),
                 broker(ClientKey, Sock, Broker)
        end
    ).
