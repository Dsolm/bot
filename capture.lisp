(require :plokami)
(load "photon.lisp")

(defpackage :capture
  (:use :cl :plokami :photon))

(in-package :capture)

(defconstant +chat-request-operation-code+ 1)


;; 152 i 263 se envian solos sin hacer nada


;; 1 es movimiento??? Parece más alejarse de un sitio


;; Events code seem to be stored in the 252 slot in the property dictionary.
(defconstant +event-code-heartbeat?+ 1)
(defconstant +event-code-move+ 3)
(defconstant +event-code-chat+ 70)

(defvar *codes* (make-hash-table))
(defvar *events* (make-hash-table))

(setf photon:*on-event* (lambda (event)
	;;		  (setf (gethash (gethash 252 (parameters event)) *events*) t)
	;;		  (setf (gethash (code event) *codes*) t)
	;;		  (describe event))
      ))


(alexandria:hash-table-keys *codes*)

;; parameter 253
(setf photon:*on-request* (lambda (request)
			;    (print request)
			    (break)
			    (print (gethash 1 (parameters request)))
			  ))

;; parameter 253
(setf photon:*on-response* (lambda (response)
			;     (break)
			;     (print response)
			  ))

(defun start-capture ()
  (with-pcap-interface (pcap "enp8s0" :promisc t :snaplen 1500 :nbio t)
      (set-filter pcap "udp dst port 5056")
      (loop
	(capture pcap -1
		 (lambda (sec usec caplen len buffer)
;		   (format t "PACKET-SIZE: ~a~%" len)
		    (receive-packet (make-instance 'packet :payload buffer
						   ;; ethernet, ip, udp packet header size
						   :offset 42
						   :size caplen))
		   (finish-output)
		   ))
	;; Better to use select/epoll/kqueue on pcap-live-descriptor
	(sleep 0.01)
	)))

;; size=123
;;(receive-packet (photon::to-stream #(#x00 #x00 #x00 #x02 #x67 #x43 #xb5 #xde #x00 #xdf #x0c
;;  #x7d #x07 #x00 #x00 #x00 #x00 #x00 #x00 #x25 #x00 #x00
;;  #x00 #x63 #x00 #x00 #x02 #x69 #xf3 #x04 #x01 #x00 #x03
;;  #x00 #x69 #x00 #x49 #xba
;;  #x15 #x01 #x69 #x67 #x43 #xb5 #xcf #xfc #x6b #x00 #x98 #x07 #x00 #x00 #x00 #x00
;;  #x00 #x00 #x20 #x00 #x00 #x00 #x63 #x00 #x00 #x02 #x6a #xf3 #x04 #x01 #x00 #x03
;;  #x00 #x62 #x01 #x01 #x6b #xff #xff #xfc #x6b #x01 #x07)))

;; size = 66
;;00 00 00 01 67 43
;;   b5 81 00 df 0c 7d 05 ff 01 00 00 00 00 0c 00 00
;;   0a a9
;;
;;
;;;; size = 74
;; 00 00 00 01 67 43
;;b1 99 00 df 0c 7d 01 ff 00 00 00 00 00 14 00 00
;;00 00 00 00 0a 8a 00 2f 21 69
;;
;;;;



(defconstant +event-new-character+ 25)
(defconstant +event-move+ 3)
(defconstant +event-knocked-down+ 154)
(defconstant +event-update-fame+ 73)
(defconstant +event-character-equipment-changed+ 80)
(defconstant +event-party-invitation+ 214)
(defconstant +event-party-joined+ 215)
(defconstant +event-party-disbanded+ 216)
(defconstant +event-party-player-joined+ 217)
(defconstant +event-party-player-left+ 217)
(defconstant +event-party-changed-order+ 217)

;; startup package, can't decode for some reason
(receive-packet (make-instance 'packet :offset 42
				       :payload #(4 66 26 233 17 200 148 145 127 143 120 16 8 0 69 0 0 44 160 5 0 0 55 17 159
						  15 5 188 125 23 192 168 1 49 19 192 182 2 0 24 0 0 233 113 45 213 1 1 0 0 74
						  213 210 109 89 79 206 143 82 191 0 0 0 0 0 0)
			       ))
