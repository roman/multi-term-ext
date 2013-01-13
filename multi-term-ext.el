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
;; Variables that you can set:
;;
;; `multi-term-ext-screen-session-name' Default screen session name (default: nil)
;; `multi-term-ext-remote-host' Default remote host address (default: nil)
;;

;;; Change log:
;; 2013/01/09
;; * `multi-term-ext-remote-ssh-port' variable
;; * `multi-term-ext-profiles' variable
;; * `multi-term-ext-remote-host' variable
;; * `multi-term-ext-setup-tramp-on-remote' variable
;; * `multi-term-setup-tramp' function
;; * `multi-term-profile' function
;;
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

(defcustom multi-term-ext-screen-session-name nil
  "Default GNU screen session name."
  :type 'string)

(defcustom multi-term-ext-remote-host nil
  "Default remote host SSH address (e.g user@host)."
  :type 'string)

(defcustom multi-term-ext-setup-tramp-on-remote nil
  "Indicates if you want TRAMP to work on remote terminals."
  :type 'bool)

(defcustom multi-term-ext-remote-ssh-port nil
  "Default SSH port."
  :type 'string)

(defcustom multi-term-ext-profiles nil
  "List of known terminal profiles to connect to.

  e.g

  (setq multi-term-ext-profiles
      '((\"python\" . ((multi-term-program \"/usr/bin/python\" )
                       (multi-term-buffer-name \"python\")
                       (multi-term-ext-screen-session-name \"python\")))

        (\"vagrant+server\" . ((multi-term-ext-remote-host  \"vagrant@127.0.0.1\")
                        (multi-term-ext-remote-ssh-port  \"2222\")
                        (multi-term-buffer-name  \"vagrant-server\")
                        (multi-term-ext-screen-session-name  \"\")))))"
  :type '(alist :key-type string
                :value-type (list (list string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun -multi-term-ext-buffer-name (buffer-name &optional index)
  (if index
      (format "%s<%s>"
              buffer-name
              current-index)
    (format "%s" buffer-name)))

(defun -multi-term-ext-next-buffer-name (&optional buffer-name)
  (let* ((term-count        (length (multi-term-list)))
         (current-index     nil)
         (buffer-name       (or buffer-name
                                multi-term-buffer-name)))
    (while (buffer-live-p
            (get-buffer
             (format "*%s*"
                     (-multi-term-ext-buffer-name
                      buffer-name
                      current-index))))
      (setq current-index (if term-count (1+ term-count) 1)))
    (-multi-term-ext-buffer-name buffer-name current-index)))

(defun -multi-term-ext-get-buffer ()
  "Get term buffer."
  (with-temp-buffer
    (let* ((shell-name (or multi-term-program ;shell name
                           (getenv "SHELL")
                           (getenv "ESHELL")
                           "/bin/sh"))
           (buffer-name (-multi-term-ext-next-buffer-name))
           (current-directory (or default-directory
                                  (expand-file-name multi-term-default-dir)))
           (term-buffer (if multi-term-program-switches
                            (apply #'make-term
                                   buffer-name shell-name nil multi-term-program-switches)
                          (make-term buffer-name shell-name))))
      (with-current-buffer term-buffer
        (multi-term-internal))
      (switch-to-buffer term-buffer)
      term-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; derived from http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
;;; stolen from http://github.com/tavisrudd/emacs.d
(defun multi-term-ext-setup-tramp ()
  "Setup ansi-term/tramp remote directory tracking
   NOTE:  this appears to have some sort of timing bug in it and doesn't always work"
  (interactive)
  (term-send-raw-string
   (concat "
function eterm_set_variables {
    local emacs_host=\"" (car (split-string (system-name) "\\.")) "\"
    local tramp_hostname=${TRAMP_HOSTNAME-$(hostname)}
    if [[ $TERM == \"eterm-color\" ]]; then
        if [[ $tramp_hostname != \"$emacs_host\" ]]; then
            echo -e \"\\033AnSiTu\" ${TRAMP_USERNAME-$(whoami)}
            echo -e \"\\033AnSiTh\" $tramp_hostname
        fi
        echo -e \"\\033AnSiTc\" $(pwd)
    elif [[ $TERM == \"screen\" || $TERM  == \"screen-256color\" ]]; then
        if [[ $tramp_hostname != \"$emacs_host\" ]]; then
            echo -e \"\\033P\\033AnSiTu\\033\\\\\" ${TRAMP_USERNAME-$(whoami)}
            echo -e \"\\033P\\033AnSiTh\\033\\\\\" $tramp_hostname
        fi
        echo -e \"\\033P\\033AnSiTc\\033\\\\\" $(pwd)
    fi
}
function eterm_tramp_init {
    for temp in cd pushd popd; do
        alias $temp=\"eterm_set_cwd $temp\"
    done

    # set hostname, user, and cwd now
    eterm_set_variables
}
function eterm_set_cwd {
    $@
    eterm_set_variables
}
eterm_tramp_init
export -f eterm_tramp_init
export -f eterm_set_variables
export -f eterm_set_cwd
clear
echo \"tramp initialized\"
")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multi-term-ext ()
  "Wrapper around normal multi-term that returns a buffer."
  (-multi-term-ext-get-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multi-term-remote (&optional user+host buffer-name)
  "Creates a multi-term in a remote host. A user + host (e.g
user@host) value will be required to perform the connection."
  (interactive)
  (let* ((user+host (or user+host
                        multi-term-ext-remote-host
                        (read-from-minibuffer "SSH address (e.g user@host): ")))
         (remote-port (if multi-term-ext-remote-ssh-port
                          (list "-p" multi-term-ext-remote-ssh-port)))
         (multi-term-program "ssh")
         (multi-term-buffer-name (or buffer-name
                                     multi-term-buffer-name
                                     "remote-terminal"))
         (multi-term-program-switches (append
                                       multi-term-program-switches
                                       remote-port
                                       (list user+host)))
         (term-buffer (-multi-term-ext-get-buffer)))
    (if multi-term-ext-setup-tramp-on-remote
        (with-current-buffer term-buffer
          (multi-term-ext-setup-tramp)))
    term-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multi-term-persistent (&optional session-name buffer-name screen-shell)
  "Creates a multi-term inside a GNU screen session. A screen
session name is required."
  (interactive)
  (let* ((session-name (or session-name
                           multi-term-ext-screen-session-name
                           (read-from-minibuffer "Session name: ")))
         (screen-shell (or screen-shell
                           multi-term-program
                           (read-from-minibuffer "Program name: ")))
         (multi-term-buffer-name (or buffer-name
                                     multi-term-buffer-name
                                     session-name))
         (multi-term-program "screen")
         (multi-term-program-switches (append multi-term-program-switches
                                              (list "-x" "-R"
                                                    "-S" session-name
                                                    "-s" screen-shell))))
    (-multi-term-ext-get-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multi-term-remote-persistent (&optional user+host session-name buffer-name screen-shell)
  "Creates multi-term buffer on a GNU screen session in a remote
host. A user + host (e.g user@host) value is required as well as
a GNU screen session name."
  (interactive)
  (let* ((session-name (or session-name
                           multi-term-ext-screen-session-name
                           (read-from-minibuffer "Session name: ")))
         (screen-shell (or screen-shell
                           multi-term-program
                           (read-from-minibuffer "Program path: ")))
         (user+host (or user+host
                        multi-term-ext-remote-host
                        (read-from-minibuffer "SSH address (e.g user@host): ")))
         (remote-port (if multi-term-ext-remote-ssh-port
                          (list "-p" multi-term-ext-remote-ssh-port)))
         (multi-term-buffer-name (or buffer-name
                                     multi-term-buffer-name
                                     session-name))
         (multi-term-program "ssh")
         (multi-term-program-switches (append
                                       multi-term-program-switches
                                       remote-port
                                       (list user+host
                                             "-t"
                                             "screen"
                                             "-x"
                                             "-R"
                                             "-S"
                                             session-name
                                             "-s"
                                             screen-shell)))
         (term-buffer (-multi-term-ext-get-buffer)))
    (if multi-term-ext-setup-tramp-on-remote
        (with-current-buffer (-multi-term-ext-get-buffer)
          (multi-term-ext-setup-tramp)))
    term-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multi-term-open-terminal ()
  "Opens a new terminal (either normal, remote, persistent or
remote+persistent) depending on your global multi-term variable
configuration."
  (cond
   (multi-term-ext-remote-host
    (if multi-term-ext-screen-session-name
        (multi-term-remote-persistent)
      (multi-term-remote)))
   (t
    (if multi-term-ext-screen-session-name
        (multi-term-persistent)
      (multi-term-ext)))))

(defun multi-term-ext-start-profile (let-options)
  "Starts a terminal session by providing a group of let options
with `multi-term-*' variable settings"
  (eval `(let ,let-options
           (multi-term-open-terminal))))

(defun multi-term-profile (profile-name)
  "Starts one of the terminal profiles specified in `multi-term-ext-profiles'"
  (interactive
   (list (ido-completing-read "Profile: "
                              (mapcar #'car multi-term-ext-profiles))))
  (let* ((let-options (cdr (assoc profile-name
                                  multi-term-ext-profiles))))
    (multi-term-ext-start-profile let-options)))


;; End:
(provide 'multi-term-ext)
;;; multi-term-ext.el ends here
