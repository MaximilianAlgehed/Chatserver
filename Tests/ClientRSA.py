from Crypto.Cipher import PKCS1_v1_5
from Crypto.PublicKey import RSA 

#Read private key from file
f = open("rsa_priv.pem", 'r')
private = RSA.importKey(f.read())
f.close()

#Read public key from file
f = open("rsa_pub.pem", 'r')
public = RSA.importKey(f.read())
f.close()

f = open("message", 'r')
msg = f.read()
f.close()

#Decrypt the message from the sender
print PKCS1_v1_5.new(private).decrypt(msg.decode("string-escape"), 0)

#Encrypt a message
message = "Hello, world!"
#Raw cypherstring
cypherstr = public.encrypt(message, None)[0]

#Hexified cypher string
cypher = "\\x"+('\\x'.join(x.encode('hex') for x in cypherstr))

#The cypher text
print cypher

#The decrypted text
print private.decrypt(cypher.decode("string-escape"))
