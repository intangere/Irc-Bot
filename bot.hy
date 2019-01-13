#!/usr/local/bin/hy
(import [hymn.types.maybe [Just Nothing maybe]])
(import socket)

;IRC packet Delimiter
(setv delimiter "\r\n")

(defn log [info message]
  (print (.strip (.format "[{0}]: {1}" #* (, info message)))))

(defn build-user [username]
  (.format "USER {0} {0} {0} :{0}" #* (, username)))

(defn build-nick [nickname]
  (.format "NICK {0}" #* (, nickname)))

(defn build-join [channel]
  (.format "JOIN {0}" #* (, channel)))

(defn build-quit []
  (.format "QUIT :{0}" #* (, "Quitting..")))

(defn build-ping [hash]
  (.format "PING :{0}" #* (, hash)))

(setv build-version "VERSION :HyBOT")

(defn build-message [channel message]
  (.format "PRIVMSG {0} :{1}{2}" #* (, channel message delimiter)))

;Redefined functions that could fail using the maybe monad
(defn decode [data]
  (.decode data.value))

(defn encode [data]
  (.encode data.value))

(defn send [conn data]
  (conn.send (.encode (.format "{0}{1}" #* (, data delimiter)))))

(defn recieve [conn]
  (conn.recv 1024))

(defn connect [host port]
  (setv temp-conn (socket.socket socket.AF_INET socket.SOCK_STREAM))
  (temp-conn.connect (, host port))
  temp-conn)

(defn parse [conn username chan data]
  (if (in (+ " 376 " username) data.value)
    (do (send conn.value (build-join chan)))
  (if (.startswith data.value "PING :")
    (do (setv hash (.strip (get (.split data.value "PING :") 1))))
      (send conn.value (build-ping hash)))))

(setv safe-connect (maybe connect))
(setv safe-recv (maybe recieve))
(setv safe-send (maybe send))
(setv safe-parse (maybe parse))
(setv safe-encode (maybe encode))
(setv safe-decode (maybe decode))


(defclass Irc [object]
  (defn --init-- [self username host port]
    (setv self.username username)
    (setv self.host host)
    (setv self.port port)
    (setv self.conn Nothing))

  (defn send [self packet]
    (log "SENDING" (.encode (.format "{0}{1}" #* (, packet delimiter))))
    (self.conn.value.send (.encode (.format "{0}{1}" #* (, packet delimiter)))))

  (defn connect [self]
    (log "CONNECT" (.join " " ["Connecting to" self.host]))
    (setv self.conn (safe-connect self.host self.port))
    (if-not self.conn
      (log "ERROR" "Connection failed")
      (do 
        (setv connected True)
        (log "SUCCESS" "CONNECTED")
        (self.send (build-nick self.username))
        (self.send (build-user self.username))
        (while connected
          (setv data (safe-decode (safe-recv self.conn.value)))
          (if-not data
            (setv connected False)
            (do 
              (log "RECV" data.value)
              (safe-parse self.conn self.username "#bighugetesthy" data))))
          (log "CLOSED" "Connection closed by remote host")))))
    
(setv irc (Irc "HyBOT" "irc.freenode.net" 6667))
(irc.connect)   
