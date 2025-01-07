;;; emacs-tunnelblick.el --  Tunnelblick

;; Copyright (C) 2023  Andrew Parisi

;; Author: Andrew Parisi <anparisi@cisco.com>
;; URL: https://github.com/andrewppar/emacs-tunnelblick
;; Keywords: elisp, tunnelblick, vpn
;; Package-Requires: ((emacs "25.2"))
;; Package-Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Manage Tunnelblick connections in Emacs.
;; The main interfaces for this are:
;;  1. tunnelblick-connect
;;  2. tunnelblick-disconnect-all
;;  3. tunnelblick-list-connections

;;; Code:
(require 'cl-macs)

(defconst tunnelblick-buffer "*tunnelblick*")

;;;###autoload
(defvar tunnelblick/cli
  ""
  "name or path of the tunnelblick command line tool to use.")

(defvar tunnelblick--cli
  (list :path nil :type nil)
  "internal representation of the tunnelblickcli.")

(defun tunnelblick--initialize-from-var ()
  "Initialize emacs-tunnelblick from *TUNNELBLICKCLI*"
  (let* ((path (executable-find tunnelblick/cli))
	 (tunnelblick-type nil))
    ;; try to guess type
    (cond ((string-match-p "tunnelblickctl" path)
	   (setq tunnelblick-type :tunnelblickctl))
	  ((string-match-p "barbara" path)
	   (setq tunnelblick-type :barbara))
	  (t
	   (error (format "%s is not a supported tunnelblick cli"
			  tunnelblick/cli))))
    (list :path path :type tunnelblick-type)))

(defun tunnelblick--initialize-cli ()
  (unless (plist-get tunnelblick--cli :path)
    (cond ((executable-find tunnelblick/cli)
	   (setq tunnelblick--cli
		 (tunnelblick--initialize-from-var)))
	  ;; try to guess tunnelblick controller
	  ((executable-find "tunnelblickctl")
	   (setq tunnelblick--cli
		 (list :path (executable-find "tunnelblickctl")
		       :type :tunnelblickctl)))
	  ((executable-find "barbara")
	   (setq tunnelblick--cli
		 (list :path (executable-find "barbara")
		       :type :barbara)))
	  (t (error "Cannot find tunnelblick CLI")))))

(defvar tunnelblick--current-layout nil)

(defun tunnelblick--execute-command-internal (command args)
  "Execute a tunnelblick COMMAND on ARGS without any error handling."
  (cl-destructuring-bind (&key path &allow-other-keys)
      tunnelblick--cli
    (save-window-excursion
      (let ((tmp-buffer (switch-to-buffer (make-temp-name "tunnelblick")))
	    (result nil))
	(apply #'call-process path  nil tmp-buffer nil command args)
	(setq result (buffer-substring-no-properties (point-min) (point-max)))
	(kill-buffer tmp-buffer)
	result))))

(defun tunnelblick--execute-command (command &rest args)
  "Execute a tunnelblick COMMAND with ARGS."
  (let ((result (tunnelblick--execute-command-internal command args))
	(not-running-message "Error: Tunnelblick is not running"))
    (when (string-match-p (regexp-quote not-running-message) result)
      (message "Tunnelblick is not running... attempting to start")
      (tunnelblick--execute-command-internal "launch" nil)
      (sleep-for 1)
      (setq result (tunnelblick--execute-command-internal command args)))
    result))

(defun tunnelblick-quit ()
  "Close the current tunnelblick buffer and return to *window-layout*."
  (interactive)
  (when tunnelblick--current-layout
    (set-window-configuration tunnelblick--current-layout))
  (kill-buffer tunnelblick-buffer))

(define-minor-mode tunnelblick-mode
    "A mode for displaying tunnelblick results."
  :init-value nil
  :lighter " tunnelblick"
  :keymap `((,(kbd "C-c C-q") . tunnelblick-quit)))

(defmacro with-tunnelblick-buffer (buffer-name &rest body)
  "Execute BODY in the context of BUFFER-NAME."
  `(progn
     (setq tunnelblick--current-layout (current-window-configuration))
     (split-window)
     (switch-to-buffer ,buffer-name)
     (when buffer-read-only
       (read-only-mode -1))
     ;; do we need this?
     ;; (kill-region (point-min) (point-max))
     (progn ,@body)
     (tunnelblick-mode 1)
     (evil-normalize-keymaps)
     (read-only-mode 1)))

(defun tunnelblick--list-profiles ()
  "List all profiles available for tunnelblick."
  (let ((result '()))
    (dolist (line (split-string (tunnelblick--execute-command "list")))
      (let ((profile (string-trim line)))
	(unless (equal profile "")
	  (push profile result))))
    result))

(defun tunnelblick--insert-profiles (profiles)
  "Insert PROFILES into the current buffer."
  (insert (propertize "PROFILES\n" 'face '(:foreground "green")))
  (insert (propertize "--------\n" 'face '(:foreground "green")))
  (dolist (profile profiles)
    (insert (format "%s\n" profile))))

(defun tunnelblick-connect-profile (profile)
  "Connect to a tunnelblick PROFILE."
  (cl-destructuring-bind (&key type &allow-other-keys)
      tunnelblick--cli
    (cl-case type
      (:tunnelblickctl
       (tunnelblick--execute-command "connect" profile))
      (:barbara
       (tunnelblick--execute-command "connect" "--configuration" profile)))))

;;;###autoload
(defun tunnelblick-connect ()
  "Interactively connect to a tunnelblick profile."
  (interactive)
  (tunnelblick--initialize-cli)
  (let ((profile (completing-read "Select Profile: "
				  (tunnelblick--list-profiles)
				  nil
				  t)))
    (tunnelblick-connect-profile profile)))

(defun tunnelblick--parse-status-line (line)
  "parse LINE from status output into plist."
  (cl-destructuring-bind (name state autoconnect tx rx &rest _ignore)
      (string-split line)
    (list :name name :state state :autoconnect autoconnect :tx tx :rx rx)))

(defun tunnelblick--connected-profiles ()
  "Get the tunnelblick profiles that are currently connected."
  (seq-reduce
   (lambda (acc line)
     (cl-destructuring-bind (&key name state &allow-other-keys)
	 (tunnelblick--parse-status-line line)
       (if (equal state "CONNECTED")
	   (cons name acc)
	 acc)))
   (cdr (string-lines (tunnelblick--execute-command "status")))
   '()))

;;;###autoload
(defun tunnelblick-disconnect  ()
  "Interactively disconnect from a tunnelblick profile."
  (interactive)
  (tunnelblick--initialize-cli)
  (if-let ((connected-profiles (tunnelblick--connected-profiles)))
      (let ((profile (completing-read "Select Profile: " connected-profiles nil t)))
	(cl-case (plist-get tunnelblick--cli :type)
	  (:tunnelblickctl
	   (tunnelblick--execute-command "disconnect" profile))
	  (:barbara
	   (tunnelblick--execute-command "disconnect" "--configuration" profile))))
    (message "No vpn profiles connected. Nothing to disconnect")))


;;; TODO: Create a way to delete a tunnelblick profile

;;;###autoload
(defun tunnelblick-disconnect-all ()
  "Disconnect from all tunnelblick profiles."
  (interactive)
  (tunnelblick--initialize-cli)
  (cl-case (plist-get tunnelblick--cli :type)
    (:tunnelblickctl (tunnelblick--execute-command "disconnect" "--all"))
    (:barbara (tunnelblick--execute-command "disconnect" "--all"))))

;;;###autoload
(defun tunnelblick-list-profiles ()
  "List all tunnelblick profiles."
  (interactive)
  (tunnelblick--initialize-cli)
  (let ((profiles (tunnelblick--list-profiles)))
    (with-tunnelblick-buffer tunnelblick-buffer
      (tunnelblick--insert-profiles profiles))))

(defun tunnelblick--key->max-val (keys maps)
  (let ((init (seq-reduce (lambda (acc key) (plist-put acc key 0)) keys '())))
    (seq-reduce
     (lambda (acc map)
       (seq-reduce
	(lambda (acc* key)
	  (let ((map-val (length (plist-get map key))))
	    (if (< (plist-get acc* key) map-val)
		(plist-put acc* key map-val)
	      acc*)))
	keys
	acc))
     maps init)))

(defun tunnelblick--format-statuses (status key->max-val)
  "Format STATUSES with KEY->MAX-VAL"
  (string-join
   (mapcar
    (lambda (status-key)
      (let* ((status-string (plist-get status status-key))
	     (max-val (plist-get key->max-val status-key))
	     (padding (make-string (+ (- max-val (length status-string)) 1) ?\ ))
	     (cell-value (format "%s%s" status-string padding)))
	(cond ((string= status-string "EXITING")
	       (propertize cell-value 'face '(:foreground "red")))
	      ((string= status-string "GET_CONFIG")
	       (propertize cell-value 'face '(:foreground "yellow")))
	      ((string= status-string "CONNECTED")
	       (propertize cell-value 'face '(:foreground "green")))
	      (t cell-value))))
    (list :name :state :autoconnect :tx :rx))))

;;;###autoload
(defun tunnelblick-status ()
  "Get the statuses of tunnelblick connections."
  (interactive)
  (tunnelblick--initialize-cli)
  (let* ((statuses (mapcar
		    #'tunnelblick--parse-status-line
		    (string-lines (tunnelblick--execute-command "status"))))
	 (key->max-val (tunnelblick--key->max-val
			(list :name :state :autoconnect :tx :rx) statuses)))
    (with-tunnelblick-buffer tunnelblick-buffer
      (mapc
       (lambda (status)
	 (insert
	  (format
	   "%s\n" (tunnelblick--format-statuses status key->max-val))))
       statuses))))

;;;###autoload
(defun tunnelblick-add-profile ()
  "Add a profile to tunnelblick."
  (interactive)
  (tunnelblick--initialize-cli)
  (cl-destructuring-bind (&key path &allow-other-keys) tunnelblick--cli
    (let ((new-profile (read-file-name "Select a profile configuration: " nil nil t)))
      (cl-case path
	(:tunnelblickctl (tunnelblick--execute-command "install" new-profile))
	(:barabara (error "profile installation not implemented for barbara"))))))

;;;###autoload
(defun tunnelblick-profile-set-credentials ()
  "Add credentials to a tunnelblick profile."
  (interactive)
  (tunnelblick--initialize-cli)
  (cl-destructuring-bind (&key path &allow-other-keys) tunnelblick--cli
    (let ((profile (completing-read
		    "Select Profile: " (tunnelblick--list-profiles) nil t))
	  (username (read-string "Username: "))
	  (password (read-string "Password: ")))
      (cl-case path
	(:tunnelblickctl
	 (error "credential setting not implemented for tunnelblickctl"))
	(:barabara
	 (let ((args '()))
	   (unless (equal username "")
	     (push username args)
	     (push "--username" args))
	   (unless (equal password "")
	     (push password args)
	     (push "--password" args))
	   (dolist (item (reverse (list "credentials" "set" "--profile" profile)))
	     (push item args))
	   (apply #'tunnelblick--execute-command args)))))))

;;;###autoload
(defun tunnelblick-kill ()
  "Kill the running tunnelblick process."
  (interactive)
  (tunnelblick--initialize-cli)
  (tunnelblick--execute-command "quit"))

(provide 'emacs-tunnelblick)
;;; emacs-tunnelblick.el ends here
