;;; sudo-edit-test.el --- sudo-edit: Unit test suite  -*- lexical-binding: t -*-

;; Copyright (c) 2016 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/nflath/sudo-edit
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

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
(declare-function undercover "undercover")

(when (require 'undercover nil 'no-error)
  (undercover "sudo-edit.el"))

(require 'sudo-edit)

(ert-deftest sudo-edit-filename-test ()
  ;; local files
  (should (equal (sudo-edit-filename "/path/to/example.txt" "root")
                 "/sudo:root@localhost:/path/to/example.txt"))
  ;; remote files
  (should (equal (sudo-edit-filename "/ssh:remotehost:/path/to/example.txt" "root")
                 "/ssh:remotehost|sudo:root@remotehost:/path/to/example.txt"))
  (should (equal (sudo-edit-filename "/ssh:remotehost|sudo:root@remotehost:/path/to/example.txt" "www-data")
                 "/ssh:remotehost|sudo:root@remotehost|sudo:www-data@remotehost:/path/to/example.txt"))

  ;; Change method from scp to ssh in ssh related methods
  (should (equal (sudo-edit-filename "/scp:remotehost:/path/to/example.txt" "root")
                 "/ssh:remotehost|sudo:root@remotehost:/path/to/example.txt"))

  ;; tramp multi-hops files
  (should (equal (sudo-edit-filename "/ssh:machine1|ssh:machine2:/path/to/example.txt" "root")
                 "/ssh:machine1|ssh:machine2|sudo:root@machine2:/path/to/example.txt"))
  (should (equal (sudo-edit-filename "/ssh:machine1|ssh:machine2|sudo:root@machine2:/path/to/example.txt" "www-data")
                 "/ssh:machine1|ssh:machine2|sudo:root@machine2|sudo:www-data@machine2:/path/to/example.txt")))

(ert-deftest sudo-edit-make-tramp-file-name-test ()
  (should (equal (sudo-edit-make-tramp-file-name
                  "ssh" "user" "domain" "remotehost" "2222" "/tmp/file")
                 "/ssh:user%domain@remotehost#2222:/tmp/file"))
  ;; Hops must be preserved even when TRAMP hides ad-hoc proxies normally.
  (let ((tramp-show-ad-hoc-proxies nil))
    (should (equal (sudo-edit-make-tramp-file-name
                    "sudo" "root" nil "remotehost" nil "/tmp/file"
                    "ssh:remotehost|")
                   "/ssh:remotehost|sudo:root@remotehost:/tmp/file"))))

(ert-deftest sudo-edit-custom-methods-test ()
  (let ((sudo-edit-local-method "doas")
        (sudo-edit-remote-method "su"))
    (should (equal (sudo-edit-filename "/tmp/file" "root")
                   "/doas:root@localhost:/tmp/file"))
    (should (equal (sudo-edit-filename "/ssh:remotehost:/tmp/file" "root")
                   "/ssh:remotehost|su:root@remotehost:/tmp/file"))))

(ert-deftest sudo-edit-sudo-edit-out-of-band-ssh-p-test ()
  (should (sudo-edit-out-of-band-ssh-p "/scp:remotehost:/path/to/example.txt"))
  (should (sudo-edit-out-of-band-ssh-p "/sftp:remotehost:/path/to/example.txt"))
  (should-not (sudo-edit-out-of-band-ssh-p "/telnet:remotehost:/path/to/example.txt")))

(provide 'sudo-edit-test)

;;; sudo-edit-test.el ends here
