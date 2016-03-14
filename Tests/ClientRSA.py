from Crypto.Cipher import PKCS1_v1_5
from Crypto.PublicKey import RSA 

def decrypt(messageFile):
    
    #Read private key from file
    f = open("rsa_priv.pem", 'r')
    private = RSA.importKey(f.read())
    f.close() 

    #Read the message from the file
    f = open(messageFile, 'r')
    msg = f.read()
    f.close()

    #Decrypt the message from the sender
    print PKCS1_v1_5.new(private).decrypt(msg.decode("string-escape"), 0)

def encrypt(message, messageFile):

    #Read public key from file
    f = open("rsa_pub.pem", 'r')
    public = RSA.importKey(f.read())
    f.close()

    #Raw cypherstring
    cypherstr = PKCS1_v1_5.new(public).encrypt(message)

    #Hexified cypher string
    cypher = "\\x"+('\\x'.join(x.encode('hex') for x in cypherstr))

    f = open(messageFile, 'w')
    f.write(cypher)
    f.close()
