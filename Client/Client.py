#!/usr/bin/python
from Crypto.Cipher import PKCS1_v1_5
from Crypto.PublicKey import RSA 
import sys
import json
import curses
import socket
import locale
from threading import Lock, Thread

locale.setlocale(locale.LC_ALL, '')

prints_lock = Lock()

#Read the settings file
config_file = open("config", 'r')
config = json.loads(config_file.read());
config_file.close()

#Read public key from file
f = open("rsa_pub.pem", 'r')
public_key_pem = f.read()
f.close()

#Read private key from file
f = open("rsa_priv.pem", 'r')
private = RSA.importKey(f.read())
f.close()
private_key = PKCS1_v1_5.new(private)

server_pub = None

#Encrypt a message using the key
def encryptWith(message, key):
    cypherstr = key.encrypt(message)
    return "\\x"+('\\x'.join(x.encode('hex') for x in cypherstr))

def chunkstring(string, length):
    return (string[0+i:length+i] for i in range(0, len(string), length))

#Connect to the server
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((config["ServerIP"], config["ServerPort"]))

#Send the key to the server
s.send(public_key_pem)
s.send("\0")

recvd = ""
while True:
    ss = s.recv(1)
    ss = ss.decode('Latin-1')
    if ss == "\0".encode('Latin-1'):
        server_pub = PKCS1_v1_5.new(RSA.importKey(recvd.decode("string-escape")))
        break
    else:
        recvd = recvd + ss

screen = curses.initscr()

curses.start_color()

screen.timeout(10)
screen.keypad(1)

dims = screen.getmaxyx()

prints = []

msgS = ""

def sendMsg(msg):
    global s
    global server_pub
    msg = config["UserName"].encode('Latin-1')+"> ".encode('Latin-1')+msg.encode('Latin-1')
    map(lambda x: s.send(encryptWith(x, server_pub)+"\0"), chunkstring(msg, 10))
    s.send(encryptWith("\0", server_pub)+"\0")

def handleInput():
    global name
    global ch
    global screen
    global msgS

    printQ = False
    c = screen.getch()

    if c == curses.ERR:
        return False

    if c == curses.KEY_BACKSPACE and msgS != "":
        msgS = msgS[:len(msgS)-1]
        return True

    if unicode(unichr(c)) == u"\n":
        printQ = True
    else:
        msgS += unicode(unichr(c)) 

    if printQ:
        if not msgS == "":
            sendMsg(msgS)
            msgS = ""
                
    return True

update = False

def bgRead():
    global s
    global prints
    global prints_lock
    global update
    global private_key

    recvd = ""
    recvds = ""
    while True:
        ss = s.recv(1)
        ss = ss.decode('Latin-1')
        if ss == "\0".encode('Latin-1'):
            recvd = private_key.decrypt(recvd.decode("string-escape"), 0).encode('Latin-1')
            if recvd == "\0".encode('Latin-1'):
                prints_lock.acquire()
                prints = prints + [recvds]
                recvds = ""
                recvd = ""
                update = True
                prints_lock.release()
            else:
                recvds = recvds + recvd
                recvd = ""
        else:
            recvd = recvd + ss

t = Thread(target=bgRead)
t.daemon = True
t.start()

fst = True

while True:
    try:
        screen.refresh()
        up = handleInput()
        prints_lock.acquire()
        update = up or update
        prints_lock.release()

        posY = dims[0]-4
        prints_lock.acquire()
        msgIndex = len(prints)-1
        prints_lock.release()

        if msgIndex< posY:
            posY = msgIndex

        if update or fst:
            screen.clear()

            while posY >= 0:
                screen.addstr(posY, 0, prints[msgIndex].encode('Latin-1'))
                posY -= 1
                msgIndex -= 1

            screen.addstr(dims[0]-2, 0, msgS.encode('Latin-1'))
        update = False
        fst = False

    except KeyboardInterrupt:
        curses.endwin()
        sys.exit()
        break
