(defpackage :photon
  (:use :cl)
  (:export #:receive-packet
	   #:packet
	   #:*on-request*
	   #:*on-response*
	   #:*on-event*
	   #:operation-request
	   #:operation-response
	   #:event-data
	   #:code
	   #:parameters
	   ))

(in-package :photon)

(require :babel) (require :ieee-floats)
(require :alexandria)

(defconstant +command-header-length+ 12)
(defconstant +proton-header-length+ 12)

;; Command types
(defconstant +command-type-disconnect+ 4)
(defconstant +command-type-send-reliable+ 6)
(defconstant +command-type-send-unreliable+ 7)
(defconstant +command-type-send-fragment+ 8)

(defconstant +message-type-operation-request+ 2)
(defconstant +message-type-operation-response+ 3)
(defconstant +message-type-event+ 4)

(defconstant +type-unknown+ 0)
(defconstant +type-null+ 42)
(defconstant +type-dictionary+ 68)
(defconstant +type-string-array+ 97)
(defconstant +type-byte+ 98)
(defconstant +type-double+ 100)
(defconstant +type-event-data+ 101)
(defconstant +type-float+ 102)
(defconstant +type-integer+ 105)
(defconstant +type-hash-table+ 104)
(defconstant +type-short+ 107)
(defconstant +type-long+ 108)
(defconstant +type-integer-array+ 110)
(defconstant +type-boolean+ 111)
(defconstant +type-operation-response+ 112)
(defconstant +type-operation-request+ 113)
(defconstant +type-string+ 115)
(defconstant +type-byte-array+ 120)
(defconstant +type-array+ 121)
(defconstant +type-object-array+ 122)
(defvar *pending-segments* (make-hash-table))

(defclass packet ()
  ((payload :initarg :payload
	    :accessor payload)
   (offset :initform 0
	   :initarg :offset
	   :accessor offset)
   (size :initarg :size)))


(defclass segmented-packet ()
  ((bytes-written :initarg :bytes-written
		  :accessor bytes-written
		  :initform 0)
   (total-payload :initarg :total-payload
		  :accessor total-payload
		  :accessor payload)))

(defun uint->int32 (uint)
  (if (logbitp 31 uint)
      (- uint 4294967296)
      uint))

(defmacro deserialize-bind (data var-method &body body)
  `(let (,@(mapcar
	     (lambda (x)
	       `(,(first x) (,(second x) ,data)))
	     var-method))
	 ,@body))

(defmethod i32 ((data packet))
  (with-slots (payload offset) data
	(let ((num 0))
	  (setf num (dpb (elt payload offset)        (byte 8 24) num))
	  (setf num (dpb (elt payload (incf offset)) (byte 8 16) num))
	  (setf num (dpb (elt payload (incf offset)) (byte 8 8)  num))
	  (setf num (dpb (elt payload (incf offset)) (byte 8 0)  num))
	  (incf offset)
	  (uint->int32 num))))

(defmethod u32 ((data packet))
  (with-slots (payload offset) data
	(let ((num 0))
	  (setf num (dpb (elt payload offset)        (byte 8 24) num))
	  (setf num (dpb (elt payload (+ 1 offset)) (byte 8 16) num))
	  (setf num (dpb (elt payload (+ 2 offset)) (byte 8 8)  num))
	  (setf num (dpb (elt payload (+ 3 offset)) (byte 8 0)  num))
	  (incf offset 4)
	  num)))

(defmethod i64 ((data packet))
  (with-slots (payload offset) data
    (let ((num 0))
      (setf num (dpb (elt payload offset)        (byte 8 56) num))
      (setf num (dpb (elt payload (incf offset))        (byte 8 48) num))
      (setf num (dpb (elt payload (incf offset))        (byte 8 40) num))
      (setf num (dpb (elt payload (incf offset))        (byte 8 32) num))
      (setf num (dpb (elt payload (incf offset))        (byte 8 24) num))
      (setf num (dpb (elt payload (incf offset)) (byte 8 16) num))
      (setf num (dpb (elt payload (incf offset)) (byte 8 8)  num))
      (setf num (dpb (elt payload (incf offset)) (byte 8 0)  num))
      (incf offset)
      num)))

(defmethod i16 ((data packet))
  (with-slots (payload offset) data
    (let ((num 0))
      (setf num (dpb (elt payload offset)        (byte 8 8) num))
      (setf num (dpb (elt payload (incf offset)) (byte 8 0) num))
      (incf offset)
      num)))


(defmethod deserialize-double ((input packet))
  (ieee-floats:decode-float64 (i64 input)))

(defmethod deserialize-float ((input packet))
  (ieee-floats:decode-float32 (u32 input)))

(defmethod deserialize-int-array ((input packet))
  (deserialize-bind  input
      ((array-size u32))
    (loop with array = (make-array array-size :element-type '(unsigned-byte 32))
	  for i from 0 below array-size
	  do (setf (elt array i) (i32 input))
	  finally (return array))))

(defmethod deserialize-hash-table ((input packet))
  (deserialize-bind input
      ((size i16))
    (deserialize-dictionary-elements input size
				     +type-unknown+ +type-unknown+)))

(defmethod deserialize-boolean ((input packet))
  (not (zerop (i8 input))))

(defmethod deserialize ((input packet))
  (deserialize-value input (i8 input)))

(defmethod deserialize-value ((input packet) type-code)
  (alexandria:switch (type-code)
    (+type-unknown+ nil)
    (+type-null+ nil)
    (+type-dictionary+ (deserialize-dictionary input))
    (+type-string-array+ (deserialize-string-array input))
    (+type-byte+ (i8 input))
    (+type-double+ (deserialize-double input))
    (+type-event-data+ (deserialize-event-data input))
    (+type-float+ (deserialize-float input))
    (+type-integer+ (i32 input))
    (+type-hash-table+ (deserialize-hash-table input))
    (+type-short+ (i16 input))
    (+type-long+ (i64 input))
    (+type-integer-array+ (deserialize-int-array input))
    (+type-boolean+ (deserialize-boolean input))
    (+type-operation-response+ (deserialize-operation-response input))
    (+type-operation-request+ (deserialize-operation-request input))
    (+type-string+ (deserialize-string input))
    (+type-byte-array+ (deserialize-byte-array input))
    (+type-array+ (deserialize-array input))
    (+type-object-array+ (deserialize-object-array input))
    (t (error (format nil "Type code: ~a not implemented." type-code)))))

(defun deserialize-dictionary-elements (input  dictionary-size key-type-code value-type-code)
  (loop
    with output = (make-hash-table :size dictionary-size :test #'equal)
    for i from 0 below dictionary-size
    do
       (let ((key (deserialize-value input (if (or (= key-type-code 0) (= key-type-code 42))
					       (i8 input)
					       key-type-code)))
	     (value (deserialize-value input (if (or (= value-type-code 0) (= value-type-code 42))
						 (i8 input)
						 value-type-code))))
	 (setf (gethash key output) value))
    finally (return output)))


(defclass operation-request ()
  ((operation-code :accessor operation-code
		   :initarg :operation-code)
   (parameters :accessor parameters
	       :initarg :parameters)))

(defclass operation-response ()
  ((operation-code :accessor operation-code
		   :initarg :operation-code)
   (return-code :accessor return-code
		:initarg :return-code)
   (debug-message :accessor debug-message
		  :initarg :debug-message)
   (parameters :accessor parameters
	       :initarg :parameters)))

(defmethod deserialize-operation-request ((input packet))
  (deserialize-bind  input
      ((operation-code i8))
    (make-instance 'operation-request
		   :operation-code operation-code
		   :parameters (deserialize-parameter-table input))))

(defmethod deserialize-operation-response ((input packet))
  (deserialize-bind  input
      ((operation-code i8)
       (return-code i16))
    (let ((debug-message (deserialize-value input (i8 input)))
	  (parameters (deserialize-parameter-table input)))
      (make-instance 'operation-response
		     :operation-code operation-code
		     :return-code return-code
		     :debug-message debug-message
		     :parameters parameters))))

(defclass event-data ()
  ((code :accessor code
	 :initarg :code)
   (parameters :accessor parameters
	       :initarg :parameters)))


(defmethod deserialize-event-data ((input packet))
  (deserialize-bind input
      ((code i8))
    (make-instance 'event-data
		   :code code
		   :parameters (deserialize-parameter-table input))))

(defmethod deserialize-parameter-table ((input packet))
  (deserialize-bind input
      ((dictionary-size i16))
    (let ((dictionary (make-hash-table :size dictionary-size :test #'equal)))
      (loop for i from 0 below dictionary-size
	    do
	       (deserialize-bind  input
		   ((key i8)
		    (value-type-code i8))
		 (setf (gethash key dictionary)
		       (deserialize-value input value-type-code))))
      dictionary)))

(defmethod deserialize-dictionary ((input packet))
  (deserialize-bind  input
      ((key-type-code i8)
       (value-type-code i8)
       (dictionary-size i16))
    (deserialize-dictionary-elements input dictionary-size key-type-code value-type-code)))


(defmacro collect-array (form size &optional (type t))
  `(loop
     with array = (make-array ,size :element-type ,type) 
     for i from 0 below ,size
     do  (setf (aref array i) ,form)
     finally (return array)))

(defmethod deserialize-object-array ((input packet))
  (deserialize-bind input
      ((array-size i16))
    (collect-array
	(deserialize-value input (i8 input))
	array-size)))


(defmethod deserialize-array ((input packet))
  (deserialize-bind input
      ((size i16)
       (type-code i8))
  (alexandria:switch (type-code)
    (+type-array+ 
     (collect-array (deserialize-array input) size))

    (+type-byte-array+
     (collect-array (deserialize-byte-array input) size))

    (+type-dictionary+
     (deserialize-dictionary-array input size))

    (t
     ;; TODO: MAP type-code TO LISP TYPE SPECIFIER.
     (collect-array (deserialize-value input type-code) size)))))

(defmethod deserialize-dictionary-array (input size)
  (deserialize-bind  input
      ((key-type-code i8)
       (value-type-code i8))
    (collect-array (let ((dictionary (make-hash-table :test 'equal)))
	    (deserialize-bind  input
		((array-size i16))
	      (loop for j from 0 below array-size
		    do
		       (let (key value)
			 (if (> key-type-code 0)
			     (setf key (deserialize-value input key-type-code))
			     ;; deserialize-val input  next-key-type-code
			     (setf key (deserialize-value input (i8 input))))
			 (if (> value-type-code 0)
			     (setf value (deserialize-value input value-type-code))
			     ;; deserialize-val input  next-value-type-code
			     (setf value (deserialize-value input (i8 input))))
			 (setf (gethash key dictionary) value))))
		     dictionary)
	size 'hash-table)))

(defmethod deserialize-byte-array ((data packet))
  (with-slots (payload offset) data
    (let ((length (u32 data)))
      (let ((bytes (subseq payload offset (+ offset length))))
	(incf offset length)
	bytes))))

(defmethod deserialize-string ((data packet))
  (with-slots (payload offset) data
    (deserialize-bind  data
	((length i16))
      (let ((string (babel:octets-to-string payload :start offset :end (+ offset length))))
	(incf offset length)
;;	(format t "GOT STRING: ~a ~%" string)
	string))))

(defmethod deserialize-string-array ((data packet))
  (deserialize-bind  data
      ((array-size i16))
    ;; REVIEW: This is a list, not an array.
    (loop for i from 0 below array-size
	  collect
	  (deserialize-string data))))

(defmethod i8 ((data packet))
  (with-slots (payload offset) data
    (let ((num (elt payload offset)))
      (incf offset)
      num)))

(defun get-segmented-package (start-sequence-number total-length)
  (let ((segmented-package (gethash start-sequence-number *pending-segments*)))
    (if segmented-package
	segmented-package
	(setf (gethash start-sequence-number *pending-segments*)
	      (make-instance 'segmented-packet
			     :total-payload (make-array total-length
							:element-type 'unsigned-byte
							:initial-element 0))))))

(defun block-copy (source src-offset destination dst-offset len)
  (loop for i from 0 below len do
    (setf (elt destination (+ i dst-offset))
	  (elt source (+ i src-offset)))))

(defun handle-finished-segmented-package (package)
  (handle-send-reliable package (length (total-payload package))))

(defun handle-segmented-payload (start-sequence-number
				 total-length
				 fragment-length
				 fragment-offset
				 data)
  (let ((segmented-package (get-segmented-package start-sequence-number total-length)))
    (with-slots (payload offset) data
      ;; TODO do not copy, use array slices instead
      (block-copy payload offset (total-payload segmented-package) fragment-offset fragment-length)
      (incf offset fragment-length)
      (incf (bytes-written segmented-package) fragment-length)
      (when (>= (bytes-written segmented-package) (length (total-payload segmented-package)))
	(remhash start-sequence-number *pending-segments*)
	(handle-finished-segmented-package segmented-package)))))

(defun handle-send-fragment (data command-length)
  (deserialize-bind data
      ((start-sequence-number u32)
       (fragment-count u32)
       (fragment-number u32)
       (total-length u32)
       (fragment-offset u32))

    ;; is also fragment-length
    (decf command-length 20)

    ;; payload field is command-length bytes

    (handle-segmented-payload
       start-sequence-number
       total-length
       command-length
       fragment-offset
       data)))

(defvar *on-request*
  (lambda (request)
    (print request)))
(defvar *on-response*
  (lambda (response)
    (print response)))
(defvar *on-event*
  (lambda (event)
    (print event)))

(defun handle-send-reliable (data command-length)
					;skip one byte
  (i8 data)
  (deserialize-bind  data
      ((message-type i8))
    ;; 2=message-header-length
    (decf command-length 2)
;	(format t "STARTING OFFSET: ~a ~%" (offset data))
	(cond
	  ((= message-type +message-type-operation-request+)
	   (funcall *on-request* (deserialize-operation-request data)))

	  ((= message-type +message-type-operation-response+)
	   (funcall *on-response* (deserialize-operation-response data)))

	  ((= message-type +message-type-event+)
	   (funcall *on-event* (deserialize-event-data data)))
	  (t
	   (with-slots (offset) data
	     (incf offset command-length)))

;	(format t "ENDING OFFSET: ~a ~%" (offset data))
;	(format t "COMMAND LENGTH: ~a ~%" command-length)

      ;; TODO Not needed
     ;;(incf offset command-length)


      )))

(defun handle-command (data)
  (let ((command-type (i8 data))
	(channel-id (i8 data))
	(command-flags (i8 data)))
    ; skip one byte
    (i8 data)
    (let ((command-length (u32 data))
	  (sequence-number (u32 data)))
      (decf command-length +command-header-length+)

      (cond
	((= command-type +command-type-disconnect+)
;;	 (print "COMMAND-TYPE-DISCONNECT")
	 )

	((= command-type +command-type-send-unreliable+)
	 ; skip 4 bytes
	 (u32 data)
	 (decf command-length 4)
	 (handle-send-reliable data command-length)
	 )

	((= command-type +command-type-send-reliable+)
	 (handle-send-reliable data command-length)
	 )

	((= command-type +command-type-send-fragment+)
	 (handle-send-fragment data command-length)
	 	 (print "COMMAND-TYPE-SEND-FRAGMENT")
	 )

	(t (with-slots (offset) data
	     (incf offset command-length)))))))

(defun receive-packet (data)
  (let ((peer-id (i16 data))
	(flags (i8 data))
	(command-count (i8 data))
	(timestamp (u32 data))
	(challenge (u32 data)))
    ;; is encrypted or crc
    (unless (or (= flags 1) (= flags #xCC))
      (loop for command from 0 below command-count
	    do
	       (handle-command data)))))



;; TESTS
(unless (eq (i32 (make-instance 'packet :payload #(1 0 0 0))) #x1000000)
  (print "TEST FAILED: i32"))
(unless (eq (i16 (make-instance 'packet :payload #(1 0 0 0))) #x100)
  (print "TEST FAILED: i16"))
(unless (eq (i8 (make-instance 'packet :payload #(1 0 0 0))) #x1)
  (print "TEST FAILED: i8"))

(defun to-stream (payload)
  (make-instance 'packet
		 :payload (coerce payload '(vector (unsigned-byte 8)))))

(unless (= (i16 (to-stream #(127 255))) 32767)
  (print "TEST FAILED i16"))

(let ((table (deserialize
	      (to-stream #(#x68 #x00 #x02 #x73 #x00 #x01 #x61 #x69
			   #x00 #x00 #x00 #x02 #x69 #x00 #x00 #x00
			   #x04 #x66 #x40 #x13 #x33 #x33))
	      )))
  (unless (and (eql (gethash "a" table) 2)
	       (eql (gethash 4 table) 2.3))
    (print "TEST FAILED: DESERIALIZE HASH TABLE")))


;; deserialize-dictionary
(let ((table (deserialize
	      (to-stream #(

			   #x44 #x73 #x73 #x00 #x02 #x00 #x08 #x74
			   #x65 #x73 #x74 #x4b #x65 #x79 #x31 #x00
			   #x0a #x74 #x65 #x73 #x74 #x56 #x61 #x6c
			   #x75 #x65 #x31 #x00 #x08 #x74 #x65 #x73
			   #x74 #x4b #x65 #x79 #x32 #x00 #x0a #x74
			   #x65 #x73 #x74 #x56 #x61 #x6c #x75 #x65
			   #x32


			   )))))
  (unless (and (equal (gethash "testKey1" table) "testValue1")
	       (equal (gethash "testKey2" table) "testValue2"))
    (print "TEST FAILED: DESERIALIZE HASH TABLE")))

;; deserialize-string-array
(unless (equalp #("test1" "test2") (deserialize (to-stream #(#x79 #x00 #x02 #x73 #x00 #x05 #x74 #x65
							   #x73 #x74 #x31 #x00 #x05 #x74 #x65 #x73
							   #x74 #x32))))
  (print "TEST FAILED: DESERIALIZE STRING ARRAY"))

;; deserialize-byte
(unless (equal (deserialize (to-stream #(#x62 #x06))) 6)
  (print "TEST FAILED: DESERIALIZE BYTE"))


;; deserialize-double
(unless (equal (deserialize
		(to-stream #(#x64 #x40 #x93 #x4a #x33 #x33 #x33 #x33 #x33)))
	       1234.55d0)
  (print "TEST FAILED: DESERIALIZE DOUBLE"))

;; deserialize-float
(unless (eql (deserialize (to-stream #(#x66 #x44 #x9A #x51 #x9A))) 1234.55)
  (print "TEST FAILED: DESERIALIZE FLOAT"))


;; deserialize-integer
(unless (eql (deserialize (to-stream #(#x69 #x00 #x00 #x04 #xD2))) 1234)
  (print "TEST FAILED: DESERIALIZE INTEGER"))


;; deserialize-short
(unless (eql (deserialize (to-stream #(#x6B #x04 #xD2))) 1234)
  (print "TEST FAILED: DESERIALIZE SHORT"))

;; deserialize-long
(unless (eql (deserialize (to-stream #(
				       #x6C #x01 #xBA #xD6 #x53 #xBE #xEC #x89
				       #x8E
				       )))
	     124647594879912334)
  (print "TEST FAILED: DESERIALIZE LONG"))


;; deserialize-integer-array
(unless (equalp (deserialize (to-stream #(
				       #x79 #x00 #x04 #x69 #x00 #x00 #x00 #x01
				       #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x05
				       #xFF #xFF #xFF #xFD)))
	     #(1 0 5 -3))
  (print "TEST FAILED: DESERIALIZE INTEGER ARRAY"))

;; deserialize-true

(unless (deserialize (to-stream #(#x6f #x01)))
  (print "TEST FAILED: DESERIALIZE TRUE"))

(when (deserialize (to-stream #(#x6f #x00)))
  (print "TEST FAILED: DESERIALIZE FALSE"))

;; deserialize-string
(unless (equal (deserialize
		(to-stream #(#x73 #x00 #x0C #x74 #x65 #x73 #x74 #x5F
			     #x6D #x65 #x73 #x73 #x61 #x67 #x65)))
	       "test_message")
  (print "TEST FAILED: DESERIALIZE STRING"))

;; deserialize-byte-array
(unless (equalp (deserialize (to-stream #(#x78 #x00 #x00 #x00 #x08 #x01 #x02 #x03
						  #x04 #x05 #x06 #x00 #xFF)))
			#(1 2 3 4 5 6 0 255))
  (print "TEST FAILED: DESERIALIZE BYTE ARRAY"))


;; deserialize-byte-2d-array
(unless (equalp (deserialize (to-stream
			      #(#x79 #x00 #x02 #x78 #x00 #x00 #x00 #x02
				#x01 #x03 #x00 #x00 #x00 #x02 #x04 #x03)))
		(coerce #(#(1 3) #(4 3)) 'vector))
  (print "TEST FAILED: DESERIALIZE 2D BYTE ARRAY"))

(unless (equalp (deserialize (to-stream
			      #(#x79 #x00 #x02 #x6E #x00 #x00 #x00 #x02
				#x00 #x00 #x00 #x01 #x00 #x00 #x00 #x03
				#x00 #x00 #x00 #x02 #x00 #x00 #x00 #x04
				#x00 #x00 #x00 #x02)))
		#(#(1 3) #(4 2)))
  (print "TEST FAILED: DESERIALIZE 2D INT ARRAY"))



(defmacro new-hashmap! (&rest key-values)
  (loop with hashmap = (make-hash-table :test #'equal)
	for (key value) in key-values do
	  (setf (gethash key hashmap) value)
	finally (return hashmap)))

;; deserialize-object-array
(let ((array
	(deserialize (to-stream #(#x7A #x00 #x03 #x69 #x00 #x00 #x00 #x01
				  #x73 #x00 #x01 #x41 #x44 #x73 #x73 #x00
				  #x01 #x00 #x03 #x61 #x62 #x63 #x00 #x03
				  #x64 #x65 #x66)))))
  (unless (equalp array (vector 1 "A" (new-hashmap! ("abc" "def"))))
    (print "TEST FAILED: DESERIALIZE OBJECT ARRAY")))

;; deserialize-dictionary-array
(unless (equalp (deserialize (to-stream

			      #(#x79 #x00 #x02 #x44 #x73 #x73 #x00 #x01
				#x00 #x03 #x61 #x62 #x63 #x00 #x03 #x64
				#x65 #x66 #x00 #x01 #x00 #x03 #x35 #x2B
				#x35 #x00 #x0C #x39 #x20 #x71 #x75 #x69
				#x63 #x6B #x20 #x6D #x61 #x74 #x68)

			      ))

		(vector (new-hashmap! ("abc" "def"))  (new-hashmap! ("5+5" "9 quick math"))))
  (print "TEST FAILED: DESERIALIZE DICTIONARY ARRAY"))


(let ((result (deserialize (to-stream #(
					#x65 #x64 #x00 #x02 #x00 #x73 #x00 #x05
					#x74 #x65 #x73 #x74 #x31 #x01 #x73 #x00
					#x05 #x74 #x65 #x73 #x74 #x32
					)))))
  (unless (and (equalp (code result) 100)
	       (equalp (parameters result) (new-hashmap! (0 "test1") (1 "test2"))))

    (print "TEST FAILED: DESERIALIZE EVENT DATA")))


;; deserialize operation response
(let ((result (deserialize (to-stream #(#x70 #x64 #x00 #x65 #x2a #x00 #x02 #x00
					#x73 #x00 #x05 #x74 #x65 #x73 #x74 #x31
					#x01 #x73 #x00 #x05 #x74 #x65 #x73 #x74
					#x32 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))))
  (unless (and (equalp (operation-code result) 100)
	       (equalp (return-code result) 101)
	       (equalp (parameters result) (new-hashmap! (0 "test1") (1 "test2"))))
    (print "TEST FAILED: DESERIALIZE OPERATION RESPONSE")))


;; deserialize-operation-response-with-debug-message
(let ((result (deserialize (to-stream #(
					#x70 #x64 #x00 #x66 #x73 #x00 #x11 #x53
					#x6F #x6D #x65 #x44 #x65 #x62 #x75 #x67
					#x20 #x6D #x65 #x73 #x73 #x61 #x67 #x65
					#x00 #x02 #x00 #x73 #x00 #x05 #x74 #x65
					#x73 #x74 #x31 #x01 #x69 #x00 #x00 #x00
					#x02)))))
  (unless (and (equalp (operation-code result) 100)
	       (equalp (return-code result) 102)
	       (equalp (debug-message result) "SomeDebug message")
	       (equalp (parameters result) (new-hashmap! (0 "test1") (1 2))))
    (print "TEST FAILED: DESERIALIZE OPERATION RESPONSE")))

(let ((result (deserialize (to-stream #(
					#x71 #x64 #x00 #x02 #x00 #x73 #x00 #x05
					#x74 #x65 #x73 #x74 #x31 #x01 #x73 #x00
					#x05 #x74 #x65 #x73 #x74 #x32
					)))))
  (unless (and (equalp (operation-code result) 100)
	       (equalp (parameters result) (new-hashmap! (0 "test1") (1 "test2"))))
    (print "TEST FAILED: DESERIALIZE OPERATION REQUEST")))


(defun test-single-event-message ()
  (deserialize (to-stream #(#x00 #x01 #x01 #x01 #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x01 #x06 #x01 #x01 #x04 #x00 #x00 #x00 #x1a #x00 #x00 #x00 #x01 #x01 #x04 #x64 #x00 #x02 #x00 #x73 #x00 #x05 #x74 #x65 #x73 #x74 #x31))))
