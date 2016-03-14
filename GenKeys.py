#!/usr/bin/python
from Crypto.PublicKey import RSA

#Generate the keypair
key = RSA.generate(2048)

#The public key
public =  key.publickey()
#The export, to save to file
public_exp = public.exportKey("PEM")
f = open("rsa_pub.pem", 'w')
f.write(public_exp)
f.close()

#The private key
private = key
#The export, to save to file
private_exp = private.exportKey("PEM")
f = open("rsa_priv.pem", 'w')
f.write(private_exp)
f.close()
