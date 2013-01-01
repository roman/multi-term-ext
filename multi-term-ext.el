;;; multi-term-ext.el --- Managing remote and persistent terminal
;;; buffers in Emacs with multi-term.

;; Author: Roman Gonzalez <romanandreg@gmail.com>
;; Copyright (C) 2012, 2013 Roman Gonzalez, all rights reserved.
;; Created: <2012-12-31>
;; Version: 0.1.0
;; Last-Updated: <2012-12-31>
;; URL: http://github.com/roman/multi-term-ext
;; Keywords: term, terminal, multiple buffer
;; Compatibility: GNU Emacs 24.1.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Features that might be required by this library:
;;
;; `multi-term'
;;

;;; Commentary:
;;
;; This package is an extension of the well known `multi-term.el', its
;; purpose is to add easy ways to create remote terms and persistent
;; terms using GNU Screen
;;

;;; Installation:
;;
;; Install via el-get
;;
;; (require 'multi-term)
;; (require 'multi-term-ext)
;;
;; Below are the commands you can use:
;;
;; `multi-term-persistent' Creates a new local term buffer using a session on GNU screen.
;; `multi-term-remote' Creates a new local term buffer using a session on GNU screen.
;; `multi-term-remote-persistent' Creates a new local term buffer using a session on GNU screen.
;;

;;; Change log:
;;
;; 2012/12/31
;; * First released.
;; * `multi-term-remote' function
;; * `multi-term-persistent' function
;; * `multi-term-remote-persistent' function
;;

;;; Acknowledgments:
;;
;; Mark Triggs <mst@dishevelled.net>
;; For create multi-shell.el
;; Andy Stewart <lazycat.manatee@gmail.com>
;; For mantaining it.
;;

;;; Bug
;;
;;

;;; TODO
;;
;;
;;

;;; Require:
(require 'multi-term)

;;; Code:

(defun -multi-term-get-buffer-ext (&optional special-shell dedicated-window)
  "Get term buffer.
If option SPECIAL-SHELL is `non-nil', will use shell from user input.
If option DEDICATED-WINDOW is `non-nil' will create dedicated `multi-term' window ."
  (with-temp-buffer
    (let ((shell-name (or multi-term-program ;shell name
                          (getenv "SHELL")
                          (getenv "ESHELL")
                          "/bin/sh"))
          term-list-length ;get length of term list
          index ;setup new term index
          term-name) ;term name
      (if dedicated-window
          (setq term-name multi-term-dedicated-buffer-name)
        ;; Compute index.
        (setq term-list-length (length (multi-term-list)))
        (setq index (if term-list-length (1+ term-list-length) 1))
        ;; switch to current local directory,
        ;; if in-existence, switch to `multi-term-default-dir'.
        (cd (or default-directory (expand-file-name multi-term-default-dir)))
        ;; adjust value N when max index of term buffer is less than length of term list
        (while (buffer-live-p (get-buffer (format "*%s<%s>*" multi-term-buffer-name index)))
          (setq index (1+ index)))
        (setq term-name (format "%s<%s>" multi-term-buffer-name index)))
      ;; Try get other shell name if `special-shell' is non-nil.
      (if special-shell
          (setq shell-name (read-from-minibuffer "Run program: " shell-name)))
      ;; Make term, details to see function `make-term' in `term.el'.
      (if multi-term-program-switches
          (apply #'make-term term-name shell-name nil multi-term-program-switches)
        (make-term term-name shell-name)))))

(defun multi-term-remote (user+host)
  "Creates a multi-term in a remote host. A user + host (e.g
user@host) value will be required to perform the connection."
  (interactive "sSSH addeess (e.g user@host): ")
  (let* ((term-list-length (length (multi-term-list)))
         (index (if term-list-length (1+ term-list-length) 1))

         (multi-term-program "ssh")
         (multi-term-buffer-name "remote-term")
         (multi-term-program-switches (append
                                       multi-term-program-switches
                                       (list user+host)))
         term-buffer)
    (setq term-buffer (-multi-term-get-buffer-ext))
    (set-buffer term-buffer)
    (while (buffer-live-p (get-buffer (format "*%s<%s>*" multi-term-buffer-name index)))
      (setq index (1+ index)))
    (setq term-name (format "%s<%s>" multi-term-buffer-name index))
    (rename-buffer (format "*%s*" term-name))
    (multi-term-internal)
    (switch-to-buffer term-buffer)
    term-buffer))

(defun multi-term-persistent (session-name &optional screen-shell)
  "Creates a multi-term inside a GNU screen session. A screen
session name is required."
  (interactive "sSession name: ")
  (let* ((session-name  (or session-name
                            multi-term-screen-session-name))
         (screen-shell (or screen-shell multi-term-program))
         (multi-term-program "screen")
         (multi-term-program-switches (append multi-term-program-switches
                                              (list "-x" "-R"
                                                    "-S" session-name
                                                    "-s" screen-shell)))
         term-buffer)
    (setq term-buffer (-multi-term-get-buffer-ext))
    (set-buffer term-buffer)
    (rename-buffer (format "*%s*" session-name))
    (multi-term-internal)
    (switch-to-buffer term-buffer)
    term-buffer))

(defun multi-term-remote-persistent (user+host session-name &optional screen-shell)
  "Creates multi-term buffer on a GNU screen session in a remote
host. A user + host (e.g user@host) value is required as well as
a GNU screen session name."
  (interactive "sSSH addeess (e.g user@host): \nsSession name: ")
  (let* ((session-name (or session-name
                           multi-term-screen-session-name))
         (screen-shell (or screen-shell multi-term-program))
         (multi-term-program "ssh")
         (multi-term-program-switches (append
                                       multi-term-program-switches
                                       (list user+host
                                             "-t"
                                             "screen"
                                             "-x"
                                             "-R"
                                             "-S"
                                             session-name
                                             "-s"
                                             screen-shell)))
         term-buffer)
    (setq term-buffer (-multi-term-get-buffer-ext))
    (set-buffer term-buffer)
    (rename-buffer (format "*%s*" session-name))
    (multi-term-internal)
    (switch-to-buffer term-buffer)
    term-buffer))

;; End:
(provide 'multi-term-ext)
;;; multi-term-ext.el ends here
