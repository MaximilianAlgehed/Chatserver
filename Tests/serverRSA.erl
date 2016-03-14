-module(serverRSA).
-export([encrypt/2, decrypt/1]).

encrypt(Message, MessageFile) ->
    {ok, PemBin} = file:read_file("rsa_pub.pem"),
    [Entry] = public_key:pem_decode(PemBin),
    Key = public_key:pem_entry_decode(Entry),
    Crypto = public_key:encrypt_public(Message, Key),
    file:write_file(MessageFile, hex:bin_to_hexstr(Crypto)).

decrypt(MessageFile) ->
    {ok, PemBin} = file:read_file("rsa_priv.pem"),
    [Entry] = public_key:pem_decode(PemBin),
    Key = public_key:pem_entry_decode(Entry),
    {ok, Message} = file:read_file(MessageFile),
    public_key:decrypt_private(hex:hexstr_to_bin(binary:bin_to_list(Message)), Key).
