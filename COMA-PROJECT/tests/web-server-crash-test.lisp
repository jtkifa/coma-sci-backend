
#|

On at least one flavor of linux, starting this file as

  export RunBugServer=1 ; sbcl --script ./web-server-crash-test.lisp  

and in another sbcl simply loading this file and running

  (make-test-json-web-request *good-json*)  ==> OK response
and
  (make-test-json-web-request *bad-json*)  

will trigger a memory failure like the one that follows


This fails on CentOS Linux,
uname:
 Linux xxx.edu 3.10.0-1160.45.1.el7.x86_64 #1 SMP Wed Oct 13 17:20:51 UTC 2021 x86_64
x86_64 x86_64 GNU/Linux

SBCL 2.3.1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[2023-03-06 17:42:39 [error]] There is no applicable method for the generic function
                                #<standard-generic-function yason:encode (12)>
                              when called with arguments
                                (#<sb-kernel:case-failure expected-type:
                                                          (member #\, #\})
                                                          datum: #\C>
CORRUPTION WARNING in SBCL pid 111951 tid 111981:
Memory fault at 0x7f7a (pc=0x52c89037 [code 0x52c88e80+0x1B7 ID 0x40a5], fp=0x7f65344bd2b8, sp=0x7f65344bd2a8) tid 111981
The integrity of this image is possibly compromised.
Exiting.
   0: fp=0x7f65344bd2b8 pc=0x52c89037 SB-KERNEL::OUTPUT-UGLY-OBJECT
   1: fp=0x7f65344bd310 pc=0x52a24b03 (LABELS SB-IMPL::HANDLE-IT :IN SB-KERNEL::OUTPUT-OBJECT)
   2: fp=0x7f65344bd338 pc=0x52b1d1a4 (FLET "PPRINT-BLOCK" :IN PPRINT-FILL)
   3: fp=0x7f65344bd3f0 pc=0x52a1b2c0 (LABELS #:BODY-NAME-3 :IN SB-PRETTY::CALL-LOGICAL-BLOCK-PRINTER)
   4: fp=0x7f65344bd4b8 pc=0x52a1adae (FLET "WITH-PRETTY-STREAM0" :IN SB-PRETTY::CALL-LOGICAL-BLOCK-PRINTER)
   5: fp=0x7f65344bd570 pc=0x52a1ab8e SB-PRETTY::CALL-LOGICAL-BLOCK-PRINTER
   6: fp=0x7f65344bd598 pc=0x52b1d0f2 PPRINT-FILL
   7: fp=0x7f65344bd600 pc=0x52c8d919 SB-PRETTY::OUTPUT-PRETTY-OBJECT
   8: fp=0x7f65344bd658 pc=0x52a24b03 (LABELS SB-IMPL::HANDLE-IT :IN SB-KERNEL::OUTPUT-OBJECT)
   9: fp=0x7f65344bd678 pc=0x52a20f18 PRIN1
  10: fp=0x7f65344bd6e0 pc=0x5336a6e0 SB-FORMAT::S-INTERPRETER
  11: fp=0x7f65344bd710 pc=0x52af7c77 SB-FORMAT::INTERPRET-DIRECTIVE-LIST
  12: fp=0x7f65344bd7a8 pc=0x52ead27d (FLET "PPRINT-BLOCK" :IN SB-FORMAT::INTERPRET-FORMAT-LOGICAL-BLOCK)
  13: fp=0x7f65344bd860 pc=0x52a1b2c0 (LABELS #:BODY-NAME-3 :IN SB-PRETTY::CALL-LOGICAL-BLOCK-PRINTER)
  14: fp=0x7f65344bd928 pc=0x52a1adae (FLET "WITH-PRETTY-STREAM0" :IN SB-PRETTY::CALL-LOGICAL-BLOCK-PRINTER)
  15: fp=0x7f65344bd9e0 pc=0x52a1ac3c SB-PRETTY::CALL-LOGICAL-BLOCK-PRINTER
  16: fp=0x7f65344bdaa8 pc=0x52ead07e SB-FORMAT::INTERPRET-FORMAT-LOGICAL-BLOCK
  17: fp=0x7f65344bdb40 pc=0x533763ea SB-FORMAT::<-INTERPRETER
  18: fp=0x7f65344bdb70 pc=0x52af7c77 SB-FORMAT::INTERPRET-DIRECTIVE-LIST
  19: fp=0x7f65344bdbd8 pc=0x52b67865 SB-FORMAT::%FORMAT
...


================================================================


If run not as a --script but using  --load then the backtrace can be different

[2023-03-06 18:44:10 [error]] There is no applicable method for the generic function
                                #<standard-generic-function yason:encode (12)>
                              when called with arguments
                                (#<sb-kernel:case-failure expected-type:
                                                          (member #\, #\})
                                                          datum: #\C>
                                 #<sb-kernel:instance {7F569E66E293}>).
See also:
  The ANSI Standard, Section 7.6.6
Backtrace for: #<SB-THREAD:THREAD "hunchentoot-worker-127.0.0.1:53624" RUNNING {1001FFF033}>
0: (TRIVIAL-BACKTRACE:PRINT-BACKTRACE-TO-STREAM #<SB-IMPL::STRING-OUTPUT-STREAM {7F569E66DE43}>)
1: (HUNCHENTOOT::GET-BACKTRACE)
2: ((FLET "H0" :IN HUNCHENTOOT:HANDLE-REQUEST) #<SB-PCL::NO-APPLICABLE-METHOD-ERROR {1003DD1E43}>)
3: (SB-KERNEL::%SIGNAL #<SB-PCL::NO-APPLICABLE-METHOD-ERROR {1003DD1E43}>)
4: (ERROR SB-PCL::NO-APPLICABLE-METHOD-ERROR :GENERIC-FUNCTION #<STANDARD-GENERIC-FUNCTION YASON:ENCODE (12)> :ARGS (#<SB-KERNEL:CASE-FAILURE expected-type: (MEMBER #\, #\}) datum: #\C> #<SB-IMPL::STRING-OUTPUT-STREAM {7F569E66E293}>))
5: ((:METHOD NO-APPLICABLE-METHOD (T)) #<STANDARD-GENERIC-FUNCTION YASON:ENCODE (12)> #<SB-KERNEL:CASE-FAILURE expected-type: (MEMBER #\, #\}) datum: #\C> #<SB-IMPL::STRING-OUTPUT-STREAM {7F569E66E293}>) [fast-method]
6: (SB-PCL::CALL-NO-APPLICABLE-METHOD #<STANDARD-GENERIC-FUNCTION YASON:ENCODE (12)> (#<SB-KERNEL:CASE-FAILURE expected-type: (MEMBER #\, #\}) datum: #\C> #<SB-IMPL::STRING-OUTPUT-STREAM {7F569E66E293}>))
7: ((:METHOD YASON:ENCODE (HASH-TABLE)) #<HASH-TABLE :TEST EQUALP :COUNT 3 {1003DD1C03}> #<SB-IMPL::STRING-OUTPUT-STREAM {7F569E66E293}>) [fast-method]
8: ((:METHOD YASON:ENCODE (HASH-TABLE)) #<HASH-TABLE :TEST EQUAL :COUNT 3 {1003DD1B63}> #<SB-IMPL::STRING-OUTPUT-STREAM {7F569E66E293}>) [fast-method]
9: (WEB-SUBMIT-JSON :REQUEST NIL)
10: ((:METHOD HUNCHENTOOT:HANDLE-REQUEST (HUNCHENTOOT:ACCEPTOR HUNCHENTOOT:REQUEST)) #<HUNCHENTOOT:EASY-ACCEPTOR (host 127.0.0.1, port 9999)> #<HUNCHENTOOT:REQUEST {1003D95FA3}>) [fast-method]
11: ((:METHOD HUNCHENTOOT:PROCESS-REQUEST (T)) #<HUNCHENTOOT:REQUEST {1003D95FA3}>) [fast-method]
12: (HUNCHENTOOT::DO-WITH-ACCEPTOR-REQUEST-COUNT-INCREMENTED #<HUNCHENTOOT:EASY-ACCEPTOR (host 127.0.0.1, port 9999)> #<FUNCTION (LAMBDA NIL :IN HUNCHENTOOT:PROCESS-CONNECTION) {1003DD06DB}>)
13: ((:METHOD HUNCHENTOOT:PROCESS-CONNECTION (HUNCHENTOOT:ACCEPTOR T)) #<HUNCHENTOOT:EASY-ACCEPTOR (host 127.0.0.1, port 9999)> #<USOCKET:STREAM-USOCKET {1002003533}>) [fast-method]
14: ((:METHOD HUNCHENTOOT:PROCESS-CONNECTION :AROUND (HUNCHENTOOT:ACCEPTOR T)) #<HUNCHENTOOT:EASY-ACCEPTOR (host 127.0.0.1, port 9999)> #<USOCKET:STREAM-USOCKET {1002003533}>) [fast-method]
15: ((:METHOD HUNCHENTOOT::HANDLE-INCOMING-CONNECTION% (HUNCHENTOOT:ONE-THREAD-PER-CONNECTION-TASKMASTER T)) #<HUNCHENTOOT:ONE-THREAD-PER-CONNECTION-TASKMASTER {100175E7B3}> #<USOCKET:STREAM-USOCKET {1002003533}>) [fast-method]
16: ((LAMBDA NIL :IN BORDEAUX-THREADS::BINDING-DEFAULT-SPECIALS))
17: ((FLET SB-UNIX::BODY :IN SB-THREAD::RUN))
18: ((FLET "WITHOUT-INTERRUPTS-BODY-132" :IN SB-THREAD::RUN))
19: ((FLET SB-UNIX::BODY :IN SB-THREAD::RUN))
20: ((FLET "WITHOUT-INTERRUPTS-BODY-125" :IN SB-THREAD::RUN))
21: (SB-THREAD::RUN)
22: ("foreign function: call_into_lisp_")
23: ("foreign function: funcall1")


Note the presence of  #<SB-IMPL::STRING-OUTPUT-STREAM {7F569E66E293}> which has dynamic-extent


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

(require 'asdf)
(require 'sb-posix)
(load (sb-posix:getenv "SBCLRC"))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (asdf:load-system "yason")
  (asdf:load-system "alexandria")
  (asdf:load-system "drakma")
  (asdf:load-system "hunchentoot"))


(defvar *hunchentoot-server* nil)
(defvar *default-web-host* "127.0.0.1")
(defvar *default-web-port* 9999)



;; define good and bad json to parse
(defparameter *bad-json*
  "{\"TYPE\":\"REQUEST\",\"COMMAND\" : \"hello\", \"ID\":\"abc123\" C}")
;;                                                                 ^^^^^  bad JSON
 (defparameter *good-json*
  "{\"TYPE\":\"REQUEST\",\"COMMAND\" : \"hello\", \"ID\":\"abc123\" }")

;;(setf hunchentoot:*log-lisp-backtraces-p* nil)


;; a hunchentoot handler to parse input JSON
(hunchentoot:define-easy-handler (web-submit-json :uri "/submit-json")
    (request)
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (s)
    (yason:encode ;; encode a special hash
     (submit-json-web-request request)
     s)))

(defun launch-web-interface (&key (port *default-web-port*)
                                                (host *default-web-host*))
  (setf *hunchentoot-server*
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                          :address host
                                          ;; :document-root nil
                                          :access-log-destination nil
                                          :port port  )))

  ;;
  *hunchentoot-server*)


(defun build-error-hash-json (&key name desc)
  (alexandria:alist-hash-table
   `(("TYPE" . "ERROR")
     ("ERROR" . ,name)
     ("DESCRIPTION" . ,desc))
   :test 'equalp))


(defun read-json-request-object (stream)
  (ignore-errors
   (yason:parse stream)))

; launch a thread to carry out a json request, and return
;; a json object of type WEB-REQUEST-RESPONSE
(defun submit-json-web-request (req-string)
  (flet ((return-with-error (error-name error-desc)
           (let ((jhash (make-hash-table :test 'equal)))
             (setf (gethash "TYPE" jhash) "WEB-REQUEST-RESPONSE")
             (setf (gethash "STATUS" jhash) "ERROR")
             (setf (gethash "ERROR" jhash)
                   (build-error-hash-json :name error-name :desc error-desc))
             (return-from submit-json-web-request jhash)))
         (return-with-success (id)
           (let ((jhash (make-hash-table :test 'equal)))
             (setf (gethash "TYPE" jhash) "WEB-REQUEST-RESPONSE")
             (setf (gethash "STATUS" jhash) "OK")
             (setf (gethash "ID" jhash) id)
             (return-from submit-json-web-request jhash))))
    
    (when (not req-string)
      (return-with-error "NO-REQUEST" "No 'REQUEST' in web command"))
    (multiple-value-bind (json-obj error-obj)
        (with-input-from-string (s req-string)
          (read-json-request-object s))
      (when (not json-obj)
        (return-with-error "JSON-PARSE-ERROR"
			   error-obj
			   #+nil(format nil "~A" error-obj)))
      ;;

      ;;
      (bordeaux-threads:make-thread
       (lambda ()  ;; the thread we launch does nothing
	 (sleep 10)))
      (return-with-success "ID123"))))


;; running
;;  "BUGTEST=1" ; sbcl --script ./web-server-crash-test.lisp --eval "(launch)"
;; will start the server
(defun launch ()
  (launch-web-interface)
  ;; main loop just hangs around
  (loop
    do (sleep 1000)))

(when (equalp (posix-getenv "RunBugServer") "1")
  (format t "Launching Webserver~%~%")
  (force-output)
  (launch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a routine for submitting the web requests using drakma
(defun make-test-json-web-request (json-text &key (port *default-web-port*)
					       (host *default-web-host*))
  (multiple-value-bind (bytes puri stream res status)
      (drakma:http-request (format nil "http://~A:~A/submit-json" host port)
			   :method :post
			   :parameters
			   `(("request" . ,json-text)))
    (when (not (stringp bytes))
      (setf bytes (map 'string 'code-char bytes)))
    (values
     bytes puri stream res status)))
