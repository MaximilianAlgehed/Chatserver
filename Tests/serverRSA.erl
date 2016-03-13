-module(serverRSA).
-export([run/0]).

run() ->
    {ok, PemBin} = file:read_file("rsa_pub.pem"),
    [Entry] = public_key:pem_decode(PemBin),
    Key = public_key:pem_entry_decode(Entry),
    Crypto = public_key:encrypt_public(<<"Well this is annoying!">>, Key),
    file:write_file("message", hex:bin_to_hexstr(Crypto)).
