;;; sudo-edit-test.el --- sudo-edit: Unit test suite  -*- lexical-binding: t -*-

;; Copyright (c) 2016 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
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

;; The unit test suite of sudo-edit

;;; Code:

(require 'sudo-edit)
(require 'undercover nil t)

(when (fboundp 'undercover)
  (undercover "sudo-edit.el"))

(ert-deftest sudo-edit-basic-test ()
  ;; local files
  (should (equal (sudo-edit-filename "example.txt")
                 "/sudo::example.txt"))
  ;; remote files
  (should (equal (sudo-edit-filename "/ssh:machine:example.txt")
                 "/ssh:machine|sudo:machine:example.txt"))
  ;; tramp multi-hops files
  (should (equal (sudo-edit-filename "/ssh:machine1|ssh:machine2:example.txt")
                 "/ssh:machine1|ssh:machine2|sudo:machine2:example.txt")))

(provide 'sudo-edit-test)

;;; sudo-edit-test.el ends here
