;;; yasnippet-radical-snippets.el --- Collection of radical yasnippet snippets

;; Copyright (C) 2021 Gustaf Waldemarson

;; Author: Gustaf Waldemarson <gustaf.waldemarson@gmail.com>
;; Version: 0.1
;; Package-Requires: ((yasnippet "0.8.0"))
;; Keywords: convenience, snippets
;; Homepage: https://github.com/Xaldew/yasnippet-radical-snippets

;;; Commentary:

;; This repository contains a small collection of *radical* snippets for the
;; yasnippet package.

;;; License:

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

;;; Code:

(require 'yasnippet)


(defconst yasnippet-radical-snippets-dir
  (expand-file-name
   "snippets"
   (file-name-directory
    ;; Copied from ‘f-this-file’ from f.el.
    (cond
     (load-in-progress load-file-name)
     ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
      byte-compile-current-file)
     (:else (buffer-file-name))))))


;;;###autoload
(defun yasnippet-radical-snippets-initialize ()
  "Load the `yasnippet-snippets' snippets directory."
  ;; NOTE: we add the symbol `yasnippet-radical-snippets-dir' rather than its
  ;; value, so that yasnippet will automatically find the directory
  ;; after this package is updated (i.e., moves directory).
  (add-to-list 'yas-snippet-dirs 'yasnippet-radical-snippets-dir t)
  (yas-load-directory yasnippet-radical-snippets-dir t))


(defgroup yasnippet-radical-snippets nil
  "Options for yasnippet setups.

This is useful for customizing options declared in
“.yas-setup.el” files.  For example, you could declare a
customizable variable used for a snippet expansion.  See Info
node `(elisp)Customization Types'."
  :group 'yasnippet)

(provide 'yasnippet-radical-snippets)

;;; yasnippet-radical-snippets.el ends here
