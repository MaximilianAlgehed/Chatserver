from Crypto.PublicKey import RSA

#Generate the keypair
key = RSA.generate(2048)

#The public key
public =  key.publickey()
#The export, to save to file
public_exp = public.exportKey("PEM")

#The private key
private = key
#The export, to save to file
private_exp = private.exportKey("PEM")

#Encrypt a message
message = "Hello, world!"
cypher = "%r"%public.encrypt(message, None)[0]

#The cypher text
print cypher

#The decrypted text
print private.decrypt(cypher.decode("string-escape")[1:-1])

#Check that there are no zeroes in the cypher texts that we send over the network
for c in cypher:
    if c == "\0":
        print "oops!"
