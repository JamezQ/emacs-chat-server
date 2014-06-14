(defvar chat-server-clients '()
  "List of clients for the chat server")
(defvar chat-server-welcome-string "Hello, welcome to the emacs chat server."
  "Welcome message to send to all new clients of the chat-server")
(defun start-chat-server (port)
  "Start the chat server"
  (interactive "nEnter Port: ")
  (unless (process-status "chat-server")
    (when (make-network-process :name "chat-server"
				:buffer "*chat-server*"
				:service port
				:family 'ipv4
				:filter 'chat-server-filter
				:sentinel 'chat-server-sentinel
				:server 't)
      (chat-server-log "-- The chat server has started. --"))
    (setq chat-server-clients '())))
(defun stop-chat-server ()
  "Stop the chat server"
  (interactive)
  (while chat-server-clients
    (delete-process (car (car chat-server-clients))))
    ; This should be handled by the sentinel, strangly.
    ; Reason being is that way we don't duplicate code
    ; depending on if a client left or the server quit.
    ;(setq chat-server-clients (cdr chat-server-clients)))
  (chat-server-log "-- The chat server has stopped. --")
  (delete-process "chat-server"))
(defun chat-server-get-username (proc)
  "Extract the username of a client from a client process."
  (let ((proc-name (process-name proc)))
    (string-match "<.*>" proc-name)
    (match-string 0 proc-name)))

(defun chat-server-log (msg)
  "This function will log chat events to the buffer *chat-server*."
  (when (get-buffer "*chat-server*")
    (with-current-buffer "*chat-server*"
      (goto-char (point-max))
      (or (bolp) (newline))
      (insert msg)
      (or (bolp) (newline)))))
(defun chat-server-sentinel (proc event)
  "Will introduce the clients to the chat server, and handle adding
and removing them from `chat-server-clients'."
  (cond
   ((eq (process-status proc) 'open)
    (setq chat-server-clients (cons (cons proc "") chat-server-clients))
    (process-send-string proc (format "%s\r\nYour username is %s.\r\n"
				      chat-server-welcome-string
				      (chat-server-get-username proc)))
    (process-send-string proc (format "%s : "
				      (chat-server-get-username proc)))
    (chat-server-log (format "%s has joined the chat server."
			     (chat-server-get-username proc))))
   ((eq (process-status proc) 'closed)
    (when (assoc proc chat-server-clients)
      (setq chat-server-clients
	    (remq (assoc proc chat-server-clients) chat-server-clients))
      (chat-server-log (format "%s has quit the chat server."
			       (chat-server-get-username proc)))))
   (t (chat-server-log (format "MAJOR PROBLEM, UNRECOGNIZED EVENT: %s"
			       event)))))
    (defun chat-server-timestamp ()
  "The chat server timestamp, it looks like 12:55 PM or something"
  (format-time-string "%I:%M:%S %p"))
(defun chat-server-filter (proc string)
  "This event will trigger whenever a user sends data to the emacs chat server."
  (unless (assoc proc chat-server-clients)
    (setq chat-server-clients (cons (cons proc "") chat-server-clients)))
  ;; Only send data if there a full line sent.
  (let* ((cur-string (concat (cdr (assoc proc chat-server-clients)) string))
	 (full-line? (string-match "\n" cur-string))
	 (username (chat-server-get-username proc))
	 (to-send (if full-line?
		      (format "\r\n%s @ %s : %s\r\n"
			      username
			      (chat-server-timestamp)
			      (substring cur-string 0 full-line?))
		    'nil)))
    (when full-line?
      (mapcar '(lambda (client)
		 (unless (eq (car client) proc)
		   (process-send-string (car client) to-send)
		   (process-send-string (car client) (format "%s : "
						     (chat-server-get-username
						      (car client))))))
	      chat-server-clients)
      (process-send-string proc (format "%s : "
					username))
      (chat-server-log to-send)
      (setq cur-string (substring cur-string (1+ full-line?))))
    (setcdr (assoc proc chat-server-clients) cur-string)))
