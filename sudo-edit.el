;;; sudo-edit.el --- Utilities for opening files with sudo

;; Copyright (C) 2014 Nathaniel Flath <flat0103@gmail.com>

;; Author: Nathaniel Flath <flat0103@gmail.com>
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides several utility functions for opening buffers
;; as root using 'sudo'.  They are:

;; sudo-edit
;; sudo-edit-current-file

;; Suggested keybinding:
;; (global-set-key (kbd "C-c C-r") 'sudo-edit-current-file)

;;; Installation

;; To use this mode, put the following in your init.el:
;; (require 'sudo-edit)

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(eval-when-compile
  (require 'subr-x))

(require 'tramp)

(defun sudo-edit-filename (file-name)
  "Return a sudo edit name for a FILE-NAME."
  (if (file-remote-p file-name)
      (let* ((vec (tramp-dissect-file-name file-name))
             (hop (tramp-make-tramp-file-name
                   (tramp-file-name-method vec)
                   (tramp-file-name-user vec)
                   (tramp-file-name-host vec)
                   ""
                   (tramp-file-name-hop vec))))
        (setq hop (string-remove-prefix tramp-prefix-format hop))
        (setq hop (string-remove-suffix tramp-postfix-host-format hop))
        (setq hop (concat hop tramp-postfix-hop-format))
        (tramp-make-tramp-file-name
         "sudo"
         ""
         (tramp-file-name-host vec)
         (tramp-file-name-localname vec)
         hop))
    (concat "/sudo::" file-name)))

;;;###autoload
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.  Will also prompt
for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (sudo-edit-filename (read-file-name "Find file(as root): ")))
    (let ((position (point)))
      (find-alternate-file (sudo-edit-filename buffer-file-name))
      (goto-char position))))

(provide 'sudo-edit)
;;; sudo-edit.el ends here
