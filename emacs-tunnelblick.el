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

(defconst tbk/buffer "*tunnelblick*")

(defconst *tunnelblickctl*
  (let ((which-response (string-trim
			 (shell-command-to-string "which tunnelblickctl"))))
    (if (string-match-p (regexp-quote "command not found") which-response)
	(error "Cannot use emacs-tunnelblick without installing tunnelblickctl")
      which-response)))

(defvar tbk-current-layout nil)

(defun tbk-execute-command-internal (command args)
  "Execute a tunnelblick COMMAND on ARGS without any error handling."
  (save-window-excursion
    (let ((tmp-buffer (switch-to-buffer (make-temp-name "tunnelblick")))
	  (result nil))
      (apply #'call-process *tunnelblickctl* nil tmp-buffer nil command args)
      (setq result (buffer-substring-no-properties (point-min) (point-max)))
      (kill-buffer tmp-buffer)
      result)))

(defun tbk-execute-command (command &rest args)
  "Execute a tunnelblick COMMAND with ARGS."
  (let ((result (tbk-execute-command-internal command args))
	(not-running-message "Error: Tunnelblick is not running"))
    (when (string-match-p (regexp-quote not-running-message) result)
      (message "Tunnelblick is not running... attempting to start")
      (tbk-execute-command-internal "launch" nil)
      (sleep-for 1)
      (setq result (tbk-execute-command-internal command args)))
    result))

(defun tbk/quit ()
  "Close the current tunnelblick buffer and return to *window-layout*."
  (interactive)
  (when tbk-current-layout
    (set-window-configuration tbk-current-layout))
  (kill-buffer *tbk/buffer*))

(define-minor-mode tunnelblick-mode
    "A mode for displaying tunnelblick results."
  :init-value nil
  :lighter " tunnelblick"
  :keymap `((,(kbd "C-c C-q") . tbk/quit)))

(defmacro with-tunnelblick-buffer (buffer-name &rest body)
  "Execute BODY in the context of BUFFER-NAME."
  `(progn
     (setq tbk-current-layout (current-window-configuration))
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

(defun tbk-list-profiles ()
  "List all profiles available for tunnelblick."
  (let ((result '()))
    (dolist (line (split-string (tbk-execute-command "ls")))
      (let ((profile (string-trim line)))
	(unless (equal profile "")
	  (push profile result))))
    result))

(defun tbk-insert-profiles (profiles)
  "Insert PROFILES into the current buffer."
  (insert (propertize "PROFILES\n" 'face '(:foreground "green")))
  (insert (propertize "--------\n" 'face '(:foreground "green")))
  (dolist (profile profiles)
    (insert (format "%s\n" profile))))

;;;###autoload
(defun tbk/connect-profile (profile)
  "Connect to a tunnelblick PROFILE."
  (tbk-execute-command "connect" profile))

;;;###autoload
(defun tbk/connect ()
  "Interactively connect to a tunnelblick profile."
  (interactive)
  (let ((profile (completing-read "Select Profile: "
				  (tbk-list-profiles)
				  nil
				  t)))
    (tbk-execute-command "connect" profile)))

;; this could be generalized and used for all status lists...
(defun tbk-connected-profiles ()
  "Get the tunnelblick profiles that are currently connected."
  (mapcar
   #'car
   (seq-filter
    (lambda (status)
      (equal (cadr status) "CONNECTED"))
    (mapcar
     (lambda (line)
       (string-split line " " t))
     (string-split (tbk-execute-command "status") "\n")))))

;;;###autoload
(defun tbk/disconnect  ()
  "Interactively disconnect from a tunnelblick profile."
  (interactive)
  (if-let ((connected-profiles (tbk-connected-profiles)))
      (let ((profile (completing-read "Select Profile: " connected-profiles nil t)))
	(tbk-execute-command "disconnect" profile))
    (message "No vpn profiles connected. Nothing to disconnect")))

;;;###autoload
(defun tbk/disconnect-all ()
  "Disconnect from all tunnelblick profiles."
  (interactive)
  (tbk-execute-command "disconnect" "--all"))

;;;###autoload
(defun tbk/list-profiles ()
  "List all tunnelblick profiles."
  (interactive)
  (let ((profiles (tbk-list-profiles)))
    (with-tunnelblick-buffer tbk/buffer
      (tbk-insert-profiles profiles))))

(defun tbk-insert-statuses (statuses)
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
(defun tbk/status ()
  "Get the statuses of tunnelblick connections."
  (interactive)
  (let ((statuses (string-split
		   (tbk-execute-command "status")
		   "\n")))
    (with-tunnelblick-buffer tbk/buffer
      (tbk-insert-statuses statuses))))

;;;###autoload
(defun tbk/add-profile ()
  "Add a profile to tunnelblick."
  (interactive)
  (let ((new-profile (read-file-name "Select a profile configuration: " nil nil t)))
    (tbk-execute-command "install" new-profile)))

;;; TODO: Create a way to delete a tunnelblick profile

;;;###autoload
(defun tbk/kill ()
  "Kill the running tunnelblick process."
  (interactive)
  (tbk-execute-command "quit"))

(provide 'emacs-tunnelblick)
;;; emacs-tunnelblick.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("tbk-" . "tunnelblick--") ("tbk/" . "tunnelblick/"))
;; End:
