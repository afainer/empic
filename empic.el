;;; empic.el --- Empic minor mode                    -*- lexical-binding: t; -*-

;; Copyright (C) 2018

;; Author:  Andrey Fainer <fandrey@gmx.com>
;; Keywords: tools

;; This file is part of Empic.

;; Empic is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; Empic is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with Empic.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defcustom empic-program "empic"
  "Empic program."
  :group 'empic)

(defvar-local empic-mode nil
  "Empic minor mode.
Don't set this variable directly.  Use the function
`empic-mode'.")
(or (assq 'empic-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(empic-mode " Empic") minor-mode-alist)))

(defvar empic-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [kp-add] 'empic-zoom-in)
    (define-key map [kp-subtract] 'empic-zoom-out)
    map)
  "Local keymap for Empic minor mode.")

(or (assq 'empic-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'empic-mode empic-mode-map) minor-mode-map-alist)))

(defvar-local empic-mode-buffer nil
  "A dired buffer with enabled `empic-mode'")

(defun empic-filter (proc string)
  "Empic mode filter function for PROC.
Read keys from STRING, lookup bindings and run commands."
  (let ((b (process-buffer proc))
        (m (process-mark proc))
        (p))
    (when (buffer-live-p b)
      (with-current-buffer b
        (save-excursion
          (setq p (goto-char (marker-position m)))
          (insert string)
          (set-marker m (point))
          (goto-char p)
          (beginning-of-line)
          (while (and (/= (point) m) (not (eobp)))
            (setq p (point))
            (end-of-line)
            (let ((k (lookup-key
                      empic-mode-map
                      (kbd (buffer-substring-no-properties p (point))))))
	      (if k (funcall k)))
            (forward-line)))))))

(defun empic-sentinel (proc event)
  "Empic mode sentinel for PROC.
Disable the Empic mode in its Dired buffer."
  (unless (process-live-p proc)
    (let ((s (process-exit-status proc)))
      (when (/= s 0)
        (message "Empic failed with code %d" s)))
    (delete-process proc)
    ;; TODO Kill *empic- and *empic-errors- buffers
    (with-current-buffer (process-buffer proc)
      (when (buffer-name empic-mode-buffer)
        (with-current-buffer empic-mode-buffer
          (setq empic-mode nil))))))

(defun empic-enable-mode ()
  "Enable the Empic minor mode.
Don't use this function, use `empic-mode' instead."
  (unless (eq major-mode 'dired-mode)
    (error "Not a Dired buffer"))
  (setq empic-mode
        (make-process :name "empic"
                      :buffer (get-buffer-create
                               (concat "*empic-" (buffer-name) "*"))
                      :stderr (get-buffer-create
                               (concat "*empic-errors-" (buffer-name) "*"))
                      :command (list empic-program "-e"
                                     (dired-get-filename t))
                      :filter 'empic-filter
                      :sentinel 'empic-sentinel))
  (let ((b (current-buffer)))
    (with-current-buffer (process-buffer empic-mode)
      (erase-buffer)
      (setq empic-mode-buffer b))))

;;; TODO Kill an Empic process if its Dired buffer is killed.

(defun empic-mode (&optional arg)
  "Toggle Empic minor mode in a Dired buffer.
With a prefix argument ARG, enable Empic mode if ARG is positive,
and disable it otherwise."
  (interactive (list (or current-prefix-arg 'toggle)))
  (let ((enable (if (eq arg 'toggle)
                    (not empic-mode)
                  (> (prefix-numeric-value arg) 0))))
    (if enable
        (empic-enable-mode)
      (if empic-mode
          (empic-quit)))))

(defun empic-send (str)
  "Send STR to an Empic process."
  (when empic-mode
    (process-send-string empic-mode str)))

(defun empic-zoom-in ()
  "Empic 'zoom in' command."
  (interactive)
  (send-string empic-mode "zoom in\n"))

(defun empic-zoom-out ()
  "Empic 'zoom out' command."
  (interactive)
  (send-string empic-mode "zoom out\n"))

(defun empic-quit ()
  "Quit Empic.
When Empic is exited disable Empic mode."
  (interactive)
  (send-string empic-mode "quit\n"))

(provide 'empic)
;;; empic.el ends here
