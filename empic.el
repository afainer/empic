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
    (define-key map [?1] 'empic-zoom-1)
    (define-key map [kp-1] 'empic-zoom-1)
    (define-key map [kp-end] 'empic-zoom-1)
    (define-key map [?0] 'empic-zoom-fit-big)
    (define-key map [kp-0] 'empic-zoom-fit-big)
    (define-key map [kp-insert] 'empic-zoom-fit-big)
    (define-key map [?9] 'empic-zoom-fit-small)
    (define-key map [kp-9] 'empic-zoom-fit-small)
    (define-key map [kp-prior] 'empic-zoom-fit-small)
    (define-key map [?/] 'empic-rotate-top)
    (define-key map [?,] 'empic-rotate-left)
    (define-key map [?.] 'empic-rotate-right)
    (define-key map [?\;] 'empic-rotate-delta-left)
    (define-key map [?']  'empic-rotate-delta-right)
    (define-key map [kp-4] 'empic-move-left)
    (define-key map [kp-6] 'empic-move-right)
    (define-key map [kp-8] 'empic-move-up)
    (define-key map [kp-2] 'empic-move-down)
    (define-key map " " 'empic-load-next)
    (define-key map [space] 'empic-load-next)
    (define-key map [backspace] 'empic-load-prev)
    (define-key map [?m] 'empic-mark)
    (define-key map [?q] 'empic-quit)
    map)
  "Local keymap for Empic minor mode.")

(or (assq 'empic-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'empic-mode empic-mode-map) minor-mode-map-alist)))

(defvar empic-mode-internal-map
  (let ((map (make-sparse-keymap)))
    (define-key map [left] 'empic-move-left)
    (define-key map [right] 'empic-move-right)
    (define-key map [up] 'empic-move-up)
    (define-key map [down] 'empic-move-down)
    map)
  "The keymap which is active in the Empic window.")

(defvar-local empic-mode-buffer nil
  "A Dired buffer with enabled `empic-mode'")

(defvar-local empic-current-file nil
  "Currently loaded file.")

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
            (let* ((key (kbd (buffer-substring-no-properties p (point))))
                   (def (or (lookup-key empic-mode-map key)
                            (lookup-key empic-mode-internal-map key))))
	      (if def (funcall def)))
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
                                     (setq empic-current-file
                                           (dired-get-filename t)))
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

(defmacro define-empic-command (command arg &optional optarg)
  (let ((arg (if (consp arg) (car arg) arg))
        (val (if (consp arg) (cdr arg) arg))
        (optarg (if (consp optarg) (car optarg) optarg))
        (optval (if (consp optarg) (cdr optarg) optarg))
        (cmd))
    (if optarg (setq arg (concat arg "-" optarg)))
    (setq cmd (concat command " " val (if optval (concat " " optval) "")))
    `(defun ,(intern (concat "empic-" command "-" arg)) ()
       ,(concat "Empic '" cmd "' command.")
       (interactive)
       (send-string empic-mode ,(concat cmd "\n")))))

(define-empic-command "zoom" "1")
(define-empic-command "zoom" "in")
(define-empic-command "zoom" "out")
(define-empic-command "zoom" "fit-big")
(define-empic-command "zoom" "fit-small")

(define-empic-command "rotate" "top")
(define-empic-command "rotate" "left")
(define-empic-command "rotate" "right")
(define-empic-command "rotate" "bottom")
(define-empic-command "rotate" "delta" ("left" . "-10"))
(define-empic-command "rotate" "delta" ("right" . "10"))

(define-empic-command "move" ("left"  . "-10 0"))
(define-empic-command "move" ("right" . "10 0"))
(define-empic-command "move" ("up" . "0 10"))
(define-empic-command "move" ("down" . "0 -10"))

(defmacro with-empic-dired (&rest body)
  "Evaluate the forms of BODY with the Empic Dired buffer."
  `(with-current-buffer (if empic-mode (current-buffer) empic-mode-buffer)
     (save-excursion
       ,@body)))

(put 'with-empic-dired 'lisp-indent-function 0)

(defun empic-load-next (&optional arg)
  "Load the next file."
  (interactive "p")
  (with-empic-dired
    (dired-goto-file (expand-file-name (concat default-directory
                                               empic-current-file)))
    (dired-next-line (or arg 1))
    (when (dired-move-to-filename)
      (let ((file (dired-get-filename t t)))
        (unless (member file '("." ".."))
          (setq empic-current-file (dired-get-filename t t))
          (send-string empic-mode (concat "load " empic-current-file "\n"))
          (empic-zoom-fit-big))))))

(defun empic-load-prev (&optional arg)
  "Load the previous file."
  (interactive "p")
  (empic-load-next (- (or arg 1))))

(defun empic-mark ()
  "Mark the current file in a Dired buffer."
  (interactive)
  (with-empic-dired
    (dired-goto-file (expand-file-name (concat default-directory
                                               empic-current-file)))
    (dired-mark 1))
  (empic-load-next))

(defun empic-quit ()
  "Quit Empic.
When Empic is exited disable Empic mode."
  (interactive)
  (send-string empic-mode "quit\n"))

(provide 'empic)
;;; empic.el ends here
