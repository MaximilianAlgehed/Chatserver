import sys
import curses
import socket
from threading import Lock, Thread

prints_lock = Lock()

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(("localhost", 5678))

screen = curses.initscr()

curses.start_color()

screen.timeout(10)
screen.keypad(1)

dims = screen.getmaxyx()

prints = []

msgS = ""

def sendMsg(msg):
    global s
    s.send(msg+"\0")

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

    if c not in range(128):
        return False

    if str(unichr(c)) == "\n":
        printQ = True
    else:
        msgS += str(unichr(c))

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

    try:
        recvd = ""
        while True:
            ss = s.recv(1)
            if ss == "\0":
                prints_lock.acquire()
                prints = prints + [recvd]
                recvd = ""
                update = True
                prints_lock.release()
            else:
                recvd = recvd + ss
    except e:
        thread.exit(1)

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
                screen.addstr(posY, 0, prints[msgIndex])
                posY -= 1
                msgIndex -= 1

            screen.addstr(dims[0]-2, 0, msgS)
        update = False
        fst = False

    except KeyboardInterrupt:
        curses.endwin()
        sys.exit()
        break
