;;; tunnelblick.el --  Tunnelblick

;; Copyright (C) 2023  Andrew Parisi

;; Author: Andrew Parisi <anparisi@cisco.com>
;; URL: https://github.com/andrewppar/emacs-tunnelblick
;; Keywords: elisp, tunnelblick, vpn
;; Package-Requires: ((emacs "25.2") (transient "0.6.0"))
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
(require 'transient)

(defconst *tunnelblick/buffer* "*tunnelblick*")

(defconst *tunnelblickctl*
  (let ((which-response (string-trim
			 (shell-command-to-string "which tunnelblickctl"))))
    (if (string-match-p (regexp-quote "command not found") which-response)
	(error "Cannot use emacs-tunnelblick without installing tunnelblickctl")
      which-response)))

(defvar *tunnelblick--current-layout* nil)

(defun tunnelblick--execute-command-internal (command args)
  "Execute a tunnelblick COMMAND on ARGS without any error handling."
  (save-window-excursion
    (let ((tmp-buffer (switch-to-buffer (make-temp-name "tunnelblick")))
	  (result nil))
      (apply #'call-process *tunnelblickctl* nil tmp-buffer nil command args)
      (setq result (buffer-substring-no-properties (point-min) (point-max)))
      (kill-buffer tmp-buffer)
      result)))

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

(defun tunnelblick/quit ()
  "Close the current tunnelblick buffer and return to *window-layout*."
  (interactive)
  (when *tunnelblick--current-layout*
    (set-window-configuration *tunnelblick--current-layout*))
  (kill-buffer *tunnelblick/buffer*))

(define-minor-mode tunnelblick-mode
    "A mode for displaying tunnelblick results."
  :init-value nil)
(evil-define-minor-mode-key 'normal 'tunnelblick-mode "q" 'tunnelblick-quit)

(defmacro with-tunnelblick-buffer (buffer-name &rest body)
  "Execute BODY in the context of BUFFER-NAME."
  `(progn
     (setq *tunnelblick--current-layout* (current-window-configuration))
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
    (dolist (line (split-string (tunnelblick--execute-command "ls")))
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

;;;###autoload
(defun tunnelblick/connect ()
  "Interactively connect to a tunnelblick profile."
  (interactive)
  (let ((profile (completing-read "Select Profile: "
				  (tunnelblick--list-profiles)
				  nil
				  t)))
    (tunnelblick--execute-command "connect" profile)))

;;;###autoload
(defun tunnelblick-disconnect  ()
  "Interactively disconnect from a tunnelblick profile."
  (interactive)
  (let ((profile (completing-read "Select Profile: "
				  (tunnelblick--list-profiles)
				  nil
				  t)))
    (tunnelblick--execute-command "disconnect" profile)))

;;;###autoload
(defun tunnelblick-disconnect-all ()
  "Disconnect from all tunnelblick profiles."
  (interactive)
  (tunnelblick--execute-command "disconnect" "--all"))

(defun tunnelblick-list-profiles ()
  "List all tunnelblick profiles."
  (interactive)
  (let ((profiles (tunnelblick--list-profiles)))
    (with-tunnelblick-buffer *tunnelblick/buffer*
      (tunnelblick--insert-profiles profiles))))

(defun tunnelblick--insert-statuses (statuses)
  "Insert STATUSES into the current buffer."
  (dolist (status statuses)
    (let ((status-elements (string-split status " " nil))
	  (colored-line-items '()))
      (dolist (element status-elements)
	(cond ((string= element "EXITING")
	       (push (propertize element 'face '(:foreground "red"))
		     colored-line-items))
	      ((string= element "CONNECTED")
	       (push (propertize element 'face '(:foreground "green"))
		     colored-line-items))
	      (t
	       (push element colored-line-items))))
      (let ((line (string-join (reverse colored-line-items) " ")))
	(insert (format "%s\n" line))))))

;;;###autoload
(defun tunnelblick/status ()
  "Get the statuses of tunnelblick connections."
  (interactive)
  (let ((statuses (string-split
		   (tunnelblick--execute-command "status")
		   "\n")))
    (with-tunnelblick-buffer *tunnelblick/buffer*
      (tunnelblick--insert-statuses statuses))))

(defun tunnelblick/add-profile ()
  "Add a profile to tunnelblick."
  (interactive)
  (let ((new-profile (read-file-name "Select a profile configuration: " nil nil t)))
    (tunnelblick--execute-command "install" new-profile)))

;;; TODO: Create a way to delete a tunnelblick profile

(defun tunnelblick/kill ()
  "Kill the running tunnelblick process."
  (interactive)
  (tunnelblick--execute-command "quit"))

(transient-define-prefix tunnelblick-transient ()
    "Transient Command for Tunnelblick."
  ["Menu: Tunnelblick"
   ("c" "Connect"          tunnelblick/connect)
   ("d" "Disconnect All"   tunnelblick/disconnect-all)
   ("l" "List Connections" tunnelblick/list-profiles)
   ("i" "Add Profile"      tunnelblick/add-profile)
   ("s" "Status"           tunnelblick/status)])


(provide 'tunnelblick)
;;; tunnelblick.el ends here
