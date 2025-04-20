;;; rqbit.el --- Frontend for the rqbit bittorrent client -*- lexical-binding: t -*-

;; Copyright (C) 2024 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/rqbit.el
;; Keywords: processes, tools
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs frontend for the rqbit bittorrent client.
;; See: <https://github.com/ikatson/rqbit>.
;;
;; Requires rqbit≥8

;; -------------------------------------------------------------------
;; Israel is committing genocide of the Palestinian people.
;;
;; The population in Gaza is facing starvation, displacement and
;; annihilation amid relentless bombardment and suffocating
;; restrictions on life-saving humanitarian aid.
;;
;; As of March 2025, Israel has killed over 50,000 Palestinians in the
;; Gaza Strip – including 15,600 children – targeting homes,
;; hospitals, schools, and refugee camps.  However, the true death
;; toll in Gaza may be at least around 41% higher than official
;; records suggest.
;;
;; The website <https://databasesforpalestine.org/> records extensive
;; digital evidence of Israel's genocidal acts against Palestinians.
;; Save it to your bookmarks and let more people know about it.
;;
;; Silence is complicity.
;; Protest and boycott the genocidal apartheid state of Israel.
;;
;;
;;                  From the river to the sea, Palestine will be free.
;; -------------------------------------------------------------------

;;; Code:
(eval-when-compile
  (defvar url-http-end-of-headers)
  (defvar url-http-response-status))

(require 'dired)
(require 'tabulated-list)
(require 'url-http)

(autoload 'url-expand-file-name "url-expand")

(define-error 'rqbit-error "Unknown rqbit error")
(define-error 'rqbit-not-found "rqbit not found" 'rqbit-error)
(define-error 'rqbit-bad-request "rqbit bad request" 'rqbit-error)

(defgroup rqbit nil
  "Integration for rqbit."
  :group 'processes)

(defface rqbit-complete '((t :inherit success :inverse-video t))
  "Face for completed torrents."
  :group 'rqbit)

(defface rqbit-in-progress '((t :inherit warning :inverse-video t))
  "Face for in-progress torrents."
  :group 'rqbit)

(defcustom rqbit-host "http://127.0.0.1:3030"
  "Rqbit host to use."
  :type 'string
  :group 'rqbit)

(defcustom rqbit-playlist-command '("mpv" "--playlist=-")
  "External tool used to reproduce a playlist.
This is a non-empty list of strings: the player tool possibly
followed by required arguments.  Once launched it will receive
the playlist source as its standard input."
  :type '(repeat string)
  :group 'rqbit)

(defvar rqbit--refresh-progress-timer nil)

(defun rqbit--set-customvar-timer (symbol value)
  "Set rqbit-variable SYMBOL value to VALUE.
Calls all actions which are necessary in order to make the new
value effective."
  (if (or (not (boundp symbol))
          (equal (symbol-value symbol) value))
      (set symbol value)
    ;; something must have changed -- restart timer
    (when (timerp rqbit--refresh-progress-timer)
      (message "Restarting timer")
      (cancel-timer rqbit--refresh-progress-timer)
      (setq rqbit--refresh-progress-timer nil)
      (rqbit-start-progress-timer)
      (message ""))))

(defcustom rqbit-progress-timer-interval 10
  "Time interval for refreshing the torrents progress (seconds)."
  :type 'number
  :set #'rqbit--set-customvar-timer
  :group 'rqbit)

(defalias 'rqbit--json-read
  (if (fboundp 'json-parse-buffer)
      (lambda ()
        (json-parse-buffer :null-object nil
                           :false-object nil
                           :object-type 'plist))
    (require 'json)
    (defvar json-object-type)
    (declare-function json-read "json" ())
    (lambda ()
      (let ((json-null nil)
            (json-false nil)
            (json-object-type 'plist))
        (json-read))))
  "Read JSON object in buffer, move point to end of buffer.")

(cl-defun rqbit-request (endpoint
                         &key data
                         (method "GET")
                         (reader 'json)
                         &aux
                         (parser (pcase reader
                                   (`json #'rqbit--json-read)
                                   (`raw  (lambda () (buffer-substring (point) (point-max))))
                                   ((pred functionp) (lambda () (funcall reader (point) (point-max))))
                                   (_ (user-error "Unsupported reader: %s" reader)))))
  "Do something with ENDPOINT and METHOD and DATA."
  (let ((url-request-data data)
        (url-request-method method)
        (url (url-expand-file-name endpoint rqbit-host)))
    (with-current-buffer (url-retrieve-synchronously url 'silent)
      (goto-char (1+ url-http-end-of-headers))
      (pcase url-http-response-status
        (200 (funcall parser))
        (400 (signal 'rqbit-bad-request (list "Bad Request" (rqbit--json-read))))
        (404 (signal 'rqbit-not-found (list "Not Found" (rqbit--json-read))))
        (_   (signal 'rqbit-error (list (buffer-substring-no-properties (point) (point-max)))))))))

(defun rqbit--dht-stats ()
  "DHT stats."
  (rqbit-request "/dht/stats"))

(defun rqbit--dht-table ()
  "DHT routing table."
  (rqbit-request "/dht/table"))

(defun rqbit--stats ()
  "Global session stats."
  (rqbit-request "/stats"))

(defun rqbit--torrent-info (id)
  "Fetch the information of a torrent by ID."
  (rqbit-request (format "/torrents/%s" id)))

(defun rqbit--torrent-playlist (id)
  "Fetch the information of a torrent by ID."
  (rqbit-request (format "/torrents/%s/playlist" id) :reader 'raw))

(defun rqbit--torrent-delete (id)
  "Forget about the torrent by ID, remove the files."
  (rqbit-request (format "/torrents/%s/delete" id) :method "POST"))

(defun rqbit--torrent-forget (id)
  "Forget about the torrent by ID, keep the files."
  (rqbit-request (format "/torrents/%s/forget" id) :method "POST"))

(defun rqbit--torrent-pause (id)
  "Pause torrent by ID."
  (rqbit-request (format "/torrents/%s/pause" id) :method "POST"))

(defun rqbit--torrent-start (id)
  "Resume torrent by ID."
  (rqbit-request (format "/torrents/%s/start" id) :method "POST"))

(defun rqbit--torrent-stats-v1 (id)
  "Fetch the stats of a torrent by ID."
  (rqbit-request (format "/torrents/%s/stats/v1" id)))

(defun rqbit--torrent-stats (id)
  "Fetch the stats of a torrent by ID."
  (rqbit-request (format "/torrents/%s/stats" id)))

(defvar rqbit-menu--marks nil
  "Alist of entries marked for copying and duplication.")

(defun rqbit-menu--refresh ()
  "Refresh the list of rqbit torrents."
  (setq tabulated-list-entries
        (mapcar (lambda (torrent)
                  (cl-destructuring-bind (&key id info_hash name &allow-other-keys) torrent
                    (cl-multiple-value-bind (progress paused) (rqbit--get-progress id)
                      (list info_hash (vector paused progress name)))))
                (plist-get (rqbit-request "/torrents") :torrents))
        rqbit-menu--marks nil)
  (tabulated-list-clear-all-tags))

(defun rqbit--cancel-refresh-timer ()
  "Cancel rqbit refresh timer."
  (when (timerp rqbit--refresh-progress-timer)
    (cancel-timer rqbit--refresh-progress-timer)
    (setq rqbit--refresh-progress-timer nil)))

(defun rqbit--speed-message-short (live)
  "Return a short description from the torrent LIVE property.
Used to build the tool tip message."
  (if live
      (format "%s↓ / %s↑"
              (plist-get (plist-get live :download_speed) :human_readable)
              (plist-get (plist-get live :upload_speed) :human_readable))
    "(Paused)"))

(defun rqbit--speed-message (live)
  "Return a description from the torrent LIVE property.
Used to build the eldoc message."
  (cl-labels ((build-message (speed peers remaining)
                (concat
                 (propertize "Speed: " 'face 'bold) speed "    "
                 (propertize "Peers: " 'face 'bold) peers "    "
                 (propertize "Time Remaining: " 'face 'bold) remaining)))
    (if live
        (cl-destructuring-bind (&key snapshot download_speed upload_speed time_remaining &allow-other-keys) live
          (let ((peer-stats (plist-get snapshot :peer_stats)))
            (build-message (format "%s↓ / %s↑" (plist-get download_speed :human_readable) (plist-get upload_speed :human_readable))
                           (format "%s/%s" (plist-get peer-stats :live) (plist-get peer-stats :seen))
                           (if time_remaining (plist-get time_remaining :human_readable) "N/A"))))
      (build-message "N/A" "N/A" (propertize "(Paused)" 'face 'font-lock-constant-face)))))

(defun rqbit-eldoc-function (&rest _ignored)
  "`eldoc-documentation-function' for rqbit."
  (when-let* ((entry (tabulated-list-get-entry)))
    (get-text-property 0 'eldoc-message (elt entry 1))))

(defun rqbit--center-string (length string)
  "Center a STRING into LENGTH."
  (declare (pure t) (side-effect-free t))
  (let ((pad (max 0 (- length (length string)))))
    (concat (make-string (ceiling pad 2) ?\s) string (make-string (floor pad 2) ?\s))))

(defun rqbit--progress-string (progress total)
  "Build a string showing the PROGRESS from the TOTAL downloaded."
  (let* ((percentage (/ (float progress) total))
         (string (rqbit--center-string 10 (format "%2.2f%%" (* percentage 100.)))))
    (put-text-property 0 (ceiling (* percentage 10)) 'face 'rqbit-in-progress string)
    string))

(defun rqbit--get-progress (id)
  "Set the progress bar of a Torrent by ID."
  (cl-destructuring-bind (&key state total_bytes progress_bytes live &allow-other-keys) (rqbit--torrent-stats-v1 id)
    (let ((paused (string-equal state "paused"))
          (progress (if (= progress_bytes total_bytes)
                        (propertize (rqbit--center-string 10 "100%") 'face 'rqbit-complete)
                      (rqbit--progress-string progress_bytes total_bytes))))
      (list (propertize progress
                        'help-echo (rqbit--speed-message-short live)
                        'eldoc-message (rqbit--speed-message live))
            (buttonize (if paused " ⏸ " " ⏵ ")
                       (lambda (_)
                         (if paused (rqbit--torrent-start id) (rqbit--torrent-pause id))
                         (tabulated-list-revert))
                       nil
                       (concat "Click to " (if paused "start" "pause")))))))

(defun rqbit--refresh-progress ()
  "Refresh rqbit progress column."
  (when (or (eq major-mode 'rqbit-menu-mode) (get-buffer-window "*rqbit*" 'visible))
    (message "Refreshing rqbit progress...")
    (save-excursion
      (goto-char (point-min))
      (while-let ((entry-id (tabulated-list-get-id)))
        (tabulated-list-set-col 1 (car (rqbit--get-progress entry-id)))
        (forward-line 1)))))

(defun rqbit-start-progress-timer ()
  "Start rqbit progress refresher."
  (when (> rqbit-progress-timer-interval 0)
    (or (timerp rqbit--refresh-progress-timer)
        (setq rqbit--refresh-progress-timer
              (run-at-time nil
                           rqbit-progress-timer-interval
                           #'rqbit--refresh-progress)))))

(defun rqbit-menu--propertize-keys (face)
  "Redisplay the torrent name on the current line with FACE."
  (tabulated-list-set-col 2 (propertize (aref (tabulated-list-get-entry) 2) 'face face)))

(defun rqbit-menu-mark ()
  "Mark torrents in the region or on the current line."
  (declare (modes rqbit-menu-mode))
  (interactive nil rqbit-menu-mode)
  (rqbit-menu--do-region
   (lambda (id)
     (push id rqbit-menu--marks)
     (rqbit-menu--propertize-keys 'dired-marked)
     (tabulated-list-put-tag #("*" 0 1 (face dired-mark))))))

(defun rqbit-menu-unmark ()
  "Mark torrents in the region or on the current line."
  (declare (modes rqbit-menu-mode))
  (interactive nil rqbit-menu-mode)
  (rqbit-menu--do-region
   (lambda (id)
     (setq rqbit-menu--marks (remove id rqbit-menu--marks))
     (rqbit-menu--propertize-keys 'default)
     (tabulated-list-put-tag " "))))

(defun rqbit-menu-unmark-all ()
  "Unmark and unflag all listed torrents."
  (declare (modes rqbit-menu-mode))
  (interactive nil rqbit-menu-mode)
  (setq-local rqbit-menu--marks nil)
  (save-excursion
    (goto-char (point-min))
    (while (tabulated-list-get-id)
      (rqbit-menu--propertize-keys 'default)
      (forward-line 1))
    (tabulated-list-clear-all-tags)))

(defun rqbit-jump-dired (&optional other-window)
  "Jump to Dired buffer corresponding to the torrent.
When OTHER-WINDOW is non-nil, jump to Dired buffer in other window."
  (declare (modes rqbit-menu-mode))
  (interactive "P" rqbit-menu-mode)
  (let ((id (or (tabulated-list-get-id) (user-error "No torrent on this line"))))
    (cl-destructuring-bind (&key name output_folder files &allow-other-keys) (rqbit--torrent-info id)
      (dired-jump other-window (if (length= files 1)
                                   (expand-file-name name output_folder)
                                 output_folder)))))

(defun rqbit-menu-play ()
  "Play torrent."
  (declare (modes rqbit-menu-mode))
  (interactive nil rqbit-menu-mode)
  (let ((id (or (tabulated-list-get-id) (user-error "No torrent on this line"))))
    (unless (executable-find (car rqbit-playlist-command))
      (error "Cannot find the rqbit playlist program: %s" (car rqbit-playlist-command)))
    (rqbit-request
     (format "/torrents/%s/playlist" id)
     :reader (lambda (start end)
               (let ((proc (make-process
                            :name "rqbit-playlist" :noquery t :connection-type 'pipe
                            :buffer (generate-new-buffer " *rqbit-playlist*")
                            :command rqbit-playlist-command
                            :sentinel
                            (lambda (proc _event)
                              (when (eq 'exit (process-status proc))
                                (kill-buffer (process-buffer proc)))))))
                 (process-send-region proc start end)
                 (process-send-eof proc))))))

(defun rqbit-menu-start ()
  "Start selected torrents."
  (declare (modes rqbit-menu-mode))
  (interactive nil rqbit-menu-mode)
  (dolist-with-progress-reporter (id (car (rqbit--get-marked-torrents))) "Starting torrents..."
    (condition-case-unless-debug nil
        (rqbit--torrent-start id)
      (rqbit-bad-request (message "Torrent already started")))))

(defun rqbit-menu-pause ()
  "Pause selected torrents."
  (declare (modes rqbit-menu-mode))
  (interactive nil rqbit-menu-mode)
  (dolist-with-progress-reporter (id (car (rqbit--get-marked-torrents)) (tabulated-list-revert)) "Pausing torrents..."
    (condition-case-unless-debug nil
        (rqbit--torrent-pause id)
      (rqbit-bad-request (message "Torrent already paused")))))

(defun rqbit-menu-remove ()
  "Forget selected torrent, keeping files from rqbit."
  (declare (modes rqbit-menu-mode))
  (interactive nil rqbit-menu-mode)
  (cl-multiple-value-bind (ids names) (rqbit--get-marked-torrents)
    (if (dired-mark-pop-up " *Forget*" 'delete names dired-deletion-confirmer (format "Forget %s " (dired-mark-prompt nil names)))
        (dolist-with-progress-reporter (id ids (tabulated-list-revert)) "Forgetting torrents..."
          (rqbit--torrent-forget id))
      (message "(No operations performed)"))))

(defun rqbit-menu-remove-with-files ()
  "Remove selected torrent with their files from rqbit."
  (declare (modes rqbit-menu-mode))
  (interactive nil rqbit-menu-mode)
  (cl-multiple-value-bind (ids names) (rqbit--get-marked-torrents)
    (if (dired-mark-pop-up " *Remove*" 'delete names dired-deletion-confirmer (format "Remove %s " (dired-mark-prompt nil names)))
        (dolist-with-progress-reporter (id ids (tabulated-list-revert)) "Removing torrents..."
          (rqbit--torrent-delete id))
      (message "(No operations performed)"))))

(defun rqbit--get-marked-torrents ()
  "Return a alist of marked torrents."
  (let* ((ids (or rqbit-menu--marks (list (or (tabulated-list-get-id) (user-error "No torrent on this line")))))
         (names (mapcar (lambda (id) (elt (cadr (assoc id tabulated-list-entries)) 2)) ids)))
    (list ids names)))

(defun rqbit-menu--do-region (function)
  "Run FUNCTION on torrents in the region or on the current line at the line start."
  (let ((id (or (tabulated-list-get-id) (user-error "Not on a table row"))))
    (goto-char (pos-bol))
    (funcall function id)
    (forward-line 1)))

(defvar-keymap rqbit-menu-mode-map
  :doc "Keymap for `rqbit-menu-mode'."
  :parent tabulated-list-mode-map
  "m" #'rqbit-menu-mark
  "d" #'rqbit-menu-pause
  "s" #'rqbit-menu-start
  "P" #'rqbit-menu-play
  "j" #'rqbit-jump-dired

  "D" #'rqbit-menu-remove
  "K" #'rqbit-menu-remove-with-files

  "u" #'rqbit-menu-unmark
  "U" #'rqbit-menu-unmark-all
  "+" #'rqbit-add-torrent)

(define-derived-mode rqbit-menu-mode tabulated-list-mode "rqbit"
  "A mode for rqbit menu."
  (setq tabulated-list-format [("" 3 t)
                               ("Progress" 10 nil)
                               ("Name" 30 t)]
        tabulated-list-padding 2)
  (make-local-variable 'rqbit-menu--marks)
  (add-hook 'kill-buffer-hook #'rqbit--cancel-refresh-timer nil t)
  (add-hook 'tabulated-list-revert-hook #'rqbit-menu--refresh nil t)
  (add-hook 'eldoc-documentation-functions 'rqbit-eldoc-function nil t)
  (tabulated-list-init-header)
  (when (zerop (length tabulated-list-entries))
    (rqbit-menu--refresh)
    (tabulated-list-print)))

;;;###autoload
(defun rqbit ()
  "Show the rqbit buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*rqbit*")
    (rqbit-menu-mode)
    (rqbit-start-progress-timer)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun rqbit-add-torrent (torrent)
  "Add TORRENT to rqbit."
  (interactive "sTorrent: ")
  (message "Adding torrent...")
  (rqbit-request "/torrents" :method "POST" :data torrent)
  (when (eq major-mode 'rqbit-menu-mode)
    (tabulated-list-revert)))

(provide 'rqbit)
;;; rqbit.el ends here
