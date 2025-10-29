;;; cabal-mode.el --- Support for Cabal packages -*- lexical-binding: t -*-

;; Copyright © 2007, 2008  Stefan Monnier
;;             2016 Arthur Fayzrakhmanov
;;             2025 August Johansson
;; URL: https://github.com/webdevred/cabal-mode
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))
;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Todo:

;; - distinguish continued lines from indented lines.
;; - indent-line-function.
;; - outline-minor-mode.

;;; Code:

;; (defun cabal-extract-fields-from-doc ()
;;   (require 'xml)
;;   (let ((section (completing-read
;;                   "Section: "
;;                   '("general-fields" "library" "executable" "buildinfo"))))
;;     (goto-char (point-min))
;;     (search-forward (concat "<sect3 id=\"" section "\">")))
;;   (let* ((xml (xml-parse-region
;;                (progn (search-forward "<variablelist>") (match-beginning 0))
;;                (progn (search-forward "</variablelist>") (point))))
;;          (varlist (cl-remove-if-not 'consp (cl-cddar xml)))
;;          (syms (mapcar (lambda (entry) (cl-caddr (assq 'literal (assq 'term entry))))
;;                        varlist))
;;          (fields (mapcar (lambda (sym) (substring-no-properties sym 0 -1)) syms)))
;;     fields))

(require 'cl-lib)

(defvar cabal-mode-interactive-prompt-state nil
  "Special variable indicating a state of user input waiting.")

(defun cabal-mode-toggle-interactive-prompt-state (&optional disabled)
  "Set `cabal-mode-interactive-prompt-state' to t.
If given DISABLED argument sets variable value to nil, otherwise to t."
  (setq cabal-mode-interactive-prompt-state (not disabled)))

(defconst cabal-general-fields
  ;; Extracted with (cabal-extract-fields-from-doc "general-fields")
  '("name" "version" "cabal-version" "license" "license-file" "copyright"
    "author" "maintainer" "stability" "homepage" "package-url" "synopsis"
    "description" "category" "tested-with" "build-depends" "data-files"
    "extra-source-files" "extra-tmp-files" "import"))

(defconst cabal-library-fields
  ;; Extracted with (cabal-extract-fields-from-doc "library")
  '("exposed-modules"))

(defconst cabal-executable-fields
  ;; Extracted with (cabal-extract-fields-from-doc "executable")
  '("executable" "main-is"))

(defconst cabal-buildinfo-fields
  ;; Extracted with (cabal-extract-fields-from-doc "buildinfo")
  '("buildable" "other-modules" "hs-source-dirs" "extensions" "ghc-options"
    "ghc-prof-options" "hugs-options" "nhc-options" "includes"
    "install-includes" "include-dirs" "c-sources" "extra-libraries"
    "extra-lib-dirs" "cc-options" "ld-options" "frameworks"))

(defvar cabal-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; The comment syntax can't be described simply in syntax-table.
    ;; We could use font-lock-syntactic-keywords, but is it worth it?
    ;; (modify-syntax-entry ?- ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?- "w" st)
    st))

(defvar cabal-font-lock-keywords
  ;; The comment syntax can't be described simply in syntax-table.
  ;; We could use font-lock-syntactic-keywords, but is it worth it?
  '(;; comments
    ("^[ \t]*--.*" . font-lock-comment-face)
    ;; fields ending in colon
    ("^ *\\([^ \t:]+\\):\\( +\\|$\\)" (1 font-lock-keyword-face))
    ;; stanzas that start a line, followed by an identifier
    ("^\\(Library\\|Executable\\|Test-Suite\\|Benchmark\\|Common\\|Package\\|Flag\\|Repository\\)[ \t]+\\([^\n \t]*\\)"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    ;; stanzas that start a line, followed by a constant
    ("^\\(Source-Repository\\)[ \t]+\\(head\\|this\\)" (1 font-lock-keyword-face) (2 font-lock-constant-face))
    ;; stanzas that start a line, followed by a constant in cabal config
    ("^\\(install-dirs\\)[ \t]+\\(global\\|user\\)" (1 font-lock-keyword-face) (2 font-lock-constant-face))
    ;; stanzas that start a line
    ("^\\(Library\\|Custom-Setup\\|source-repository-package\\)[ \t]*$" (1 font-lock-keyword-face))
    ;; stanzas that start a line in cabal config
    ("^\\(haddock\\|init\\|program-locations\\|program-default-options\\)[ \t]*$" (1 font-lock-keyword-face))
    ;; stanzas that can live inside if-blocks
    ("^[ \t]*\\(program-options\\)$" (1 font-lock-keyword-face))
    ;; if clause
    ("^ *\\(if\\|elif\\)[ \t]+.*$" (1 font-lock-keyword-face))
    ;; else clause
    ("^ *\\(else\\)[ \t]*$" (1 font-lock-keyword-face))
    ;; True/False
    ("\\<\\(?:True\\|False\\)\\>" (0 font-lock-constant-face))))

(defvar cabal-buffers nil
  "List of Cabal buffers.")

(defun cabal-buffers-clean (&optional buffer)
  "Refresh list of known cabal buffers.

Check each buffer in variable `cabal-buffers' and remove
it from list if one of the following conditions are hold:
+ buffer is killed;
+ buffer's mode is not derived from `cabal-mode';
+ buffer is a BUFFER (if given)."
  (let ((bufs ()))
    (dolist (buf cabal-buffers)
      (if (and (buffer-live-p buf)
               (not (eq buf buffer))
               (with-current-buffer buf (derived-mode-p 'cabal-mode)))
          (push buf bufs)))
    (setq cabal-buffers bufs)))

(defun cabal-unregister-buffer ()
  "Exclude current buffer from global list of known cabal buffers."
  (cabal-buffers-clean (current-buffer)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cabal\\'\\|/cabal\\.project\\|/\\.cabal/config\\'" . cabal-mode))

(defvar cabal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'cabal-subsection-arrange-lines)
    (define-key map (kbd "C-M-n") 'cabal-next-section)
    (define-key map (kbd "C-M-p") 'cabal-previous-section)
    (define-key map (kbd "M-n") 'cabal-next-subsection)
    (define-key map (kbd "M-p") 'cabal-previous-subsection)
    (define-key map (kbd "C-<down>") 'cabal-next-subsection)
    (define-key map (kbd "C-<up>") 'cabal-previous-subsection)
    (define-key map (kbd "C-c C-f") 'cabal-find-or-create-source-file)
    (define-key map (kbd "M-g l") 'cabal-goto-library-section)
    (define-key map (kbd "M-g e") 'cabal-goto-executable-section)
    (define-key map (kbd "M-g b") 'cabal-goto-benchmark-section)
    (define-key map (kbd "M-g o") 'cabal-goto-common-section)
    (define-key map (kbd "M-g t") 'cabal-goto-test-suite-section)
    map))

;;;###autoload
(define-derived-mode cabal-mode text-mode "Cabal"
  "Major mode for Cabal package description files."
  (setq-local font-lock-defaults
              '(cabal-font-lock-keywords t t nil nil))
  (add-to-list 'cabal-buffers (current-buffer))
  (add-hook 'change-major-mode-hook 'cabal-unregister-buffer nil 'local)
  (add-hook 'kill-buffer-hook 'cabal-unregister-buffer nil 'local)
  (setq-local comment-start "--")
  (setq-local comment-start-skip "--[ \t]*")
  (setq-local comment-end "")
  (setq-local comment-end-skip "[ \t]*\\(\\s>\\|\n\\)")
  (setq-local indent-line-function 'cabal-indent-line)
  (setq indent-tabs-mode nil))

(make-obsolete 'cabal-get-setting
               'cabal--get-field
               "March 14, 2016")
(defalias 'cabal-get-setting 'cabal--get-field
  "Try to read value of field with NAME from current buffer.
Obsolete function.  Defined for backward compatibility.  Use
`cabal--get-field' instead.")

(defun cabal--get-field (name)
  "Try to read value of field with NAME from current buffer."
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^[ \t]*" (regexp-quote name)
                     ":[ \t]*\\(.*\\(\n[ \t]+[ \t\n].*\\)*\\)")
             nil t)
        (let ((val (match-string 1))
              (start 1))
          (when (match-end 2)             ;Multiple lines.
            ;; The documentation is not very precise about what to do about
            ;; the \n and the indentation: are they part of the value or
            ;; the encoding?  I take the point of view that \n is part of
            ;; the value (so that values can span multiple lines as well),
            ;; and that only the first char in the indentation is part of
            ;; the encoding, the rest is part of the value (otherwise, lines
            ;; in the value cannot start with spaces or tabs).
            (while (string-match "^[ \t]\\(?:\\.$\\)?" val start)
              (setq start (1+ (match-beginning 0)))
              (setq val (replace-match "" t t val))))
          val)))))


(make-obsolete 'cabal-guess-setting
               'cabal-get-field
               "March 14, 2016")
(defalias 'cabal-guess-setting 'cabal-get-field
  "Read the value of field with NAME from project's cabal file.
Obsolete function.  Defined for backward compatibility.  Use
`cabal-get-field' instead.")

;;;###autoload
(defun cabal-get-field (name)
  "Read the value of field with NAME from project's cabal file.
If there is no valid .cabal file to get the setting from (or
there is no corresponding setting with that name in the .cabal
file), then this function returns nil."
  (interactive)
  (when (and name buffer-file-name)
    (let ((cabal-file (cabal-find-file)))
      (when (and cabal-file (file-readable-p cabal-file))
        (with-temp-buffer
          (insert-file-contents cabal-file)
          (cabal--get-field name))))))

(defun cabal-compute-checksum (dir)
  "Compute MD5 checksum of package description file in DIR.
Return nil if no Cabal description file could be located via
`cabal-find-pkg-desc'."
  (let ((cabal-file (cabal-find-pkg-desc dir)))
    (when cabal-file
      (with-temp-buffer
        (insert-file-contents cabal-file)
        (md5 (buffer-string))))))

(defun cabal-find-file (&optional dir)
  "Search for package description file upwards starting from DIR.
If DIR is nil, `default-directory' is used as starting point for
directory traversal.  Upward traversal is aborted if file owner
changes.  Uses `cabal-find-pkg-desc' internally."
  (let ((use-dir (or dir default-directory)))
    (while (and use-dir (not (file-directory-p use-dir)))
      (setq use-dir (file-name-directory (directory-file-name use-dir))))
    (when use-dir
      (catch 'found
        (let ((user (nth 2 (file-attributes use-dir)))
              ;; Abbreviate, so as to stop when we cross ~/.
              (root (abbreviate-file-name use-dir)))
          ;; traverse current dir up to root as long as file owner doesn't change
          (while (and root (equal user (nth 2 (file-attributes root))))
            (let ((cabal-file (cabal-find-pkg-desc root)))
              (when cabal-file
                (throw 'found cabal-file)))

            (let ((proot (file-name-directory (directory-file-name root))))
              (if (equal proot root) ;; fix-point reached?
                  (throw 'found nil)
                (setq root proot))))
          nil)))))

(defun cabal-find-pkg-desc (dir &optional allow-multiple)
  "Find a package description file in the directory DIR.
Returns nil if none or multiple \".cabal\" files were found.  If
ALLOW-MULTIPLE is non nil, in case of multiple \".cabal\" files,
a list is returned instead of failing with a nil result."
  ;; This is basically a port of Cabal's
  ;; Distribution.Simple.Utils.findPackageDesc function
  ;;  http://hackage.haskell.org/packages/archive/Cabal/1.16.0.3/doc/html/Distribution-Simple-Utils.html
  ;; but without the exception throwing.
  (let* ((cabal-files
          (cl-remove-if 'file-directory-p
                        (cl-remove-if-not 'file-exists-p
                                          (directory-files dir t ".\\.cabal\\'")))))
    (cond
     ((= (length cabal-files) 1) (car cabal-files)) ;; exactly one candidate found
     (allow-multiple cabal-files) ;; pass-thru multiple candidates
     (t nil))))

(defun cabal-find-dir (&optional dir)
  "Like `cabal-find-file' but return directory instead.
See `cabal-find-file' for meaning of DIR argument."
  (let ((cabal-file (cabal-find-file dir)))
    (when cabal-file
      (file-name-directory cabal-file))))

;;;###autoload
(defun cabal-visit-file (other-window)
  "Locate and visit package description file for file visited by current buffer.
This uses `cabal-find-file' to locate the closest
\".cabal\" file and open it.  This command assumes a common Cabal
project structure where the \".cabal\" file is in the top-folder
of the project, and all files related to the project are in or
below the top-folder.  If called with non-nil prefix argument
OTHER-WINDOW use `find-file-other-window'."
  (interactive "P")
  ;; Note: We aren't allowed to rely on haskell-session here (which,
  ;; in pathological cases, can have a different .cabal file
  ;; associated with the current buffer)
  (if buffer-file-name
      (let ((cabal-file (cabal-find-file (file-name-directory buffer-file-name))))
        (if cabal-file
            (if other-window
                (find-file-other-window cabal-file)
              (find-file cabal-file))
          (error "Could not locate \".cabal\" file for %S" buffer-file-name)))
    (error "Cannot locate \".cabal\" file for buffers not visiting any file")))

(defvar cabal-commands
  '("install"
    "update"
    "list"
    "info"
    "upgrade"
    "fetch"
    "unpack"
    "check"
    "sdist"
    "upload"
    "report"
    "init"
    "configure"
    "build"
    "copy"
    "haddock"
    "clean"
    "hscolour"
    "register"
    "test"
    "help"
    "run"))

(defgroup cabal nil
  "Haskell cabal files."
  :group 'haskell)

(defconst cabal-section-header-regexp "^[[:alnum:]]" )
(defconst cabal-subsection-header-regexp "^[ \t]*[[:alnum:]]\\w*:")
(defconst cabal-comment-regexp "^[ \t]*--")
(defconst cabal-empty-regexp "^[ \t]*$")
(defconst cabal-conditional-regexp "^[ \t]*\\(\\if\\|else\\|}\\)")

(defun cabal-classify-line ()
  "Classify the current line's type.
Possible results are \\='section-header \\='subsection-header \\='section-data
\\='comment and \\='empty"
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at cabal-subsection-header-regexp ) 'subsection-header)
     ((looking-at cabal-section-header-regexp) 'section-header)
     ((looking-at cabal-comment-regexp) 'comment)
     ((looking-at cabal-empty-regexp ) 'empty)
     ((looking-at cabal-conditional-regexp ) 'conditional)
     (t 'section-data))))

(defun cabal-header-p ()
  "Is the current line a section or subsection header?"
  (cl-case (cabal-classify-line)
    ((section-header subsection-header) t)))

(defun cabal-section-header-p ()
  "Is the current line a section or subsection header?"
  (cl-case (cabal-classify-line)
    ((section-header) t)))


(defun cabal-section-beginning ()
  "Find the beginning of the current section."
  (save-excursion
    (while (not (or (bobp) (cabal-section-header-p)))
      (forward-line -1))
    (point)))

(defun cabal-beginning-of-section ()
  "Go to the beginning of the section."
  (interactive)
  (goto-char (cabal-section-beginning)))

(defun cabal-section-end ()
  "Find the end of the current section."
  (interactive)
  (save-excursion
    (if (re-search-forward "\n\\([ \t]*\n\\)*[[:alnum:]]" nil t)
        (match-beginning 0)
      (point-max))))

(defun cabal-end-of-section ()
  "Go to the end of the section."
  (interactive)
  (goto-char (cabal-section-end)))

(defun cabal-next-section ()
  "Go to the next section."
  (interactive)
  (when (cabal-section-header-p) (forward-line))
  (while (not (or (eobp) (cabal-section-header-p)))
    (forward-line)))

(defun cabal-previous-section ()
  "Go to the next section."
  (interactive)
  (when (cabal-section-header-p) (forward-line -1))
  (while (not (or (bobp) (cabal-section-header-p)))
    (forward-line -1)))

(defun cabal-subsection-end ()
  "Find the end of the current subsection."
  (save-excursion
    (cabal-beginning-of-subsection)
    (forward-line)
    (while (and (not (eobp))
                (member (cabal-classify-line) '(empty section-data)))
      (forward-line))
    (unless (eobp) (forward-line -1))
    (while (and (equal (cabal-classify-line) 'empty)
                (not (bobp)))
      (forward-line -1))
    (end-of-line)
    (point)))

(defun cabal-end-of-subsection ()
  "Go to the end of the current subsection."
  (interactive)
  (goto-char (cabal-subsection-end)))

(defun cabal-section ()
  "Get the name and data of the associated section."
  (save-excursion
    (cabal-beginning-of-section)
    (when (and (cabal-section-header-p)
               (looking-at "^\\(\\w+\\)[ \t]*\\(.*\\)$"))
      (list :name (match-string-no-properties 1)
            :value (match-string-no-properties 2)
            :beginning (match-beginning 0)
            :end (cabal-section-end)))))


(defun cabal-subsection ()
  "Get the name and bounds of of the current subsection."
  (save-excursion
    (cabal-beginning-of-subsection)
    (when (looking-at "\\([ \t]*\\(\\w*\\):\\)[ \t]*")
      (list :name (match-string-no-properties 2)
            :beginning (match-end 0)
            :end (save-match-data (cabal-subsection-end))
            :data-start-column (save-excursion (goto-char (match-end 0))
                                               (current-column))
            :data-indent-column (save-excursion (goto-char (match-end 0))
                                                (when (looking-at "\n  +\\(\\w*\\)") (goto-char (match-beginning 1)))
                                                (current-column))))))


(defun cabal-section-name (section)
  "Return the name of SECTION.
Fetches the value associated with the :name key from the SECTION plist."
  (plist-get section :name))

(defun cabal-section-value (section)
  "Return the value of SECTION.
Fetches the value associated with the :value key from the SECTION plist."
  (plist-get section :value))

(defun cabal-section-start (section)
  "Return the buffer position where SECTION begins.
Fetches the value associated with the :beginning key from the SECTION plist."
  (plist-get section :beginning))

(defun cabal-section-data-start-column (section)
  "Return the column where the data part of SECTION start.
Fetches the value associated with the :data-start-column key from the SECTION plist."
  (plist-get section :data-start-column))

(defun cabal-section-data-indent-column (section)
  "Return the indentation column used for data in SECTION.
Fetches the value associated with the :data-indent-column key from the SECTION plist."
  (plist-get section :data-indent-column))

(defun cabal-map-component-type (component-type)
  "Map from cabal file COMPONENT-TYPE to build command component-type."
  (let ((component-type (downcase component-type)))
    (cond ((equal component-type "executable") "exe")
          ((equal component-type "test-suite") "test")
          ((equal component-type "benchmark")  "bench"))))

(defun cabal-enum-targets (&optional process-type)
  "Enumerate .cabal targets.
PROCESS-TYPE determines the format of the returned target."
  (let ((cabal-file (cabal-find-file))
        (process-type (if process-type process-type 'ghci)))
    (when (and cabal-file (file-readable-p cabal-file))
      (with-temp-buffer
        (insert-file-contents cabal-file)
        (cabal-mode)
        (goto-char (point-min))
        (let ((matches)
              (package-name (cabal--get-field "name")))
          (cabal-next-section)
          (while (not (eobp))
            (if (cabal-source-section-p (cabal-section))
                (let* ((section (cabal-section))
                       (component-type (cabal-section-name section))
                       (val (car (split-string
                                  (cabal-section-value section)))))
                  (if (equal (downcase component-type) "library")
                      (let ((lib-target (if (eq 'stack-ghci process-type)
                                            (concat package-name ":lib")
                                          (concat "lib:" package-name))))
                        (push lib-target matches))
                    (push (concat  (when (eq 'stack-ghci process-type)
                                     (concat package-name ":"))
                                   (cabal-map-component-type component-type)
                                   ":"
                                   val)
                          matches))))
            (cabal-next-section))
          (reverse matches))))))

(defmacro cabal-with-subsection (subsection replace &rest funs)
  "Copy SUBSECTION data into a temporary buffer, save indentation and execute FUNS.

If REPLACE is non-nil the subsection data is replaced with the
resulting buffer-content."
  (let ((section (make-symbol "section"))
        (beg (make-symbol "beg"))
        (end (make-symbol "end"))
        (start-col (make-symbol "start-col"))
        (section-data (make-symbol "section-data")))
    `(let* ((,section ,subsection)
            (,beg (plist-get ,section :beginning))
            (,end (plist-get ,section :end))
            (,start-col (plist-get ,section :data-start-column))
            (,section-data (buffer-substring ,beg ,end)))
       (save-excursion
         (prog1
             (with-temp-buffer
               (setq indent-tabs-mode nil)
               (indent-to ,start-col)
               (insert ,section-data)
               (goto-char (point-min))
               (prog1
                   (progn (cabal-save-indentation ,@funs))
                 (goto-char (point-min))
                 (when (looking-at (format "[ ]\\{0,%d\\}" (1+ ,start-col)))
                   (replace-match ""))

                 (setq ,section-data (buffer-substring (point-min) (point-max)))))
           ,@(when replace
               `((delete-region ,beg ,end)
                 (goto-char ,beg)
                 (insert ,section-data))))))))

(defmacro cabal-each-line (&rest forms)
  "Execute FORMS on each line."
  `(save-excursion
     (while (< (point) (point-max))
       ,@forms
       (forward-line))))

(defun cabal-chomp-line ()
  "Remove leading and trailing whitespaces from current line."
  (beginning-of-line)
  (when (looking-at "^[ \t]*\\([^ \t]\\|\\(?:[^ \t].*[^ \t]\\)\\)[ \t]*$")
    (replace-match (match-string 1) nil t)
    t))


(defun cabal-min-indentation (&optional beg end)
  "Compute largest common whitespace prefix of each line in between BEG and END."
  (save-excursion
    (goto-char (or beg (point-min)))
    (let ((min-indent nil))
      (while (< (point) (or end (point-max)))
        (let ((indent (current-indentation)))
          (if (and (not (cabal-ignore-line-p))
                   (or (not min-indent)
                       (< indent min-indent)))
              (setq min-indent indent)))
        (forward-line))
      min-indent)))

(defun cabal-ignore-line-p ()
  "Does line only contain whitespaces and comments?"
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*\\(?:--.*\\)?$")))

(defun cabal-kill-indentation ()
  "Remove longest common whitespace prefix from each line."
  (goto-char (point-min))
  (let ((indent (cabal-min-indentation)))
    (cabal-each-line (unless (cabal-ignore-line-p)
                       (delete-char indent)) )
    indent))

(defun cabal-add-indentation (indent)
  (goto-char (point-min))
  (cabal-each-line
   (unless (cabal-ignore-line-p)
     (indent-to indent))))


(defmacro cabal-save-indentation (&rest funs)
  "Strip indentation from each line, execute FUNS and reinstate indentation
   so that the indentation of the FIRST LINE matches."
  (let ((old-l1-indent (make-symbol "new-l1-indent"))
        (new-l1-indent (make-symbol "old-l1-indent")))
    `(let ( (,old-l1-indent (save-excursion
                              (goto-char (point-min))
                              (current-indentation))))
       (unwind-protect
           (progn
             (cabal-kill-indentation)
             ,@funs)
         (progn
           (goto-char (point-min))
           (let ((,new-l1-indent (current-indentation)))
             (cabal-add-indentation (- ,old-l1-indent
                                       ,new-l1-indent))))))))

(defun cabal-comma-separatorp (pos)
  "Return non-nil when the char at POS is a comma separator.
Characters that are not a comma, or commas inside a commment or
string, are not comma separators."
  (when (eq (char-after pos) ?,)
    (let ((ss (syntax-ppss pos)))
      (not
       (or
        ;; inside a string
        (nth 3 ss)
        ;; inside a comment
        (nth 4 ss))))))

(defun cabal-strip-list-and-detect-style ()
  "Strip commas from a comma-separated list.
Detect and return the comma style.  The possible options are:

before: a comma at the start of each line (except the first), e.g.
    Foo
  , Bar

after: a comma at the end of each line (except the last), e.g.
    Foo,
    Bar

single: everything on a single line, but comma-separated, e.g.
    Foo, Bar

nil: no commas, e.g.
    Foo Bar

If the styles are mixed, the position of the first comma
determines the style.  If there is only one element then `after'
style is assumed."
  (let (comma-style)
    ;; split list items on single line
    (goto-char (point-min))
    (while (re-search-forward
            "\\([^ \t,\n]\\)[ \t]*\\(,\\)[ \t]*\\([^ \t,\n]\\)" nil t)
      (when (cabal-comma-separatorp (match-beginning 2))
        (setq comma-style 'single)
        (replace-match "\\1\n\\3" nil nil)))
    ;; remove commas before
    (goto-char (point-min))
    (while (re-search-forward "^\\([ \t]*\\),\\([ \t]*\\)" nil t)
      (setq comma-style 'before)
      (replace-match "" nil nil))
    ;; remove trailing commas
    (goto-char (point-min))
    (while (re-search-forward ",[ \t]*$" nil t)
      (unless (eq comma-style 'before)
        (setq comma-style 'after))
      (replace-match "" nil nil))

    ;; if there is just one line then set default as 'after
    (unless comma-style
      (goto-char (point-min))
      (forward-line)
      (when (eobp)
        (setq comma-style 'after)))
    (goto-char (point-min))

    (cabal-each-line (cabal-chomp-line))
    comma-style))

(defun cabal-listify (comma-style)
  "Add commas so that the buffer contain a comma-separated list.
Respect the COMMA-STYLE, see
`cabal-strip-list-and-detect-style' for the possible
styles."
  (cl-case comma-style
    (before
     (goto-char (point-min))
     (while (cabal-ignore-line-p) (forward-line))
     (indent-to 2)
     (forward-line)
     (cabal-each-line
      (unless (cabal-ignore-line-p)
        (insert ", "))))
    (after
     (goto-char (point-max))
     (while (equal 0 (forward-line -1))
       (unless (cabal-ignore-line-p)
         (end-of-line)
         (insert ",")
         (beginning-of-line))))
    (single
     (goto-char (point-min))
     (while (not (eobp))
       (end-of-line)
       (unless (eobp)
         (insert ", ")
         (delete-char 1)
         (just-one-space))))))

(defmacro cabal-with-cs-list (&rest funs)
  "Format the buffer so that each line contains a list element.
Respect the comma style."
  (let ((comma-style (make-symbol "comma-style")))
    `(let ((,comma-style
            (save-excursion
              (cabal-strip-list-and-detect-style))))
       (unwind-protect (progn ,@funs)
         (cabal-listify ,comma-style)))))


(defun cabal-sort-lines-key-fun ()
  (when (looking-at "[ \t]*--[ \t,]*")
    (goto-char (match-end 0)))
  nil)

(defmacro cabal-save-position (&rest forms)
  "Save position as mark, execute FORMS and go back to mark."
  `(prog2
       (cabal-mark)
       (progn ,@forms)
     (cabal-goto-mark)
     (cabal-remove-mark)))

(defun cabal-sort-lines-depends-compare (key1 key2)
    "Compare two dependency keys, KEY1 and KEY2, for sorting.
Return t if KEY1 should come before KEY2.

Each key is a cons cell (START . END) representing a region in the buffer.
The function extracts the corresponding text strings and sorts them
lexicographically, but ensures that the package \"base\" always comes first."
  (let* ((key1str (buffer-substring (car key1) (cdr key1)))
         (key2str (buffer-substring (car key2) (cdr key2)))
         (base-regex "^[ \t]*base\\($\\|[^[:alnum:]-]\\)"))
    (cond
     ((string-match base-regex key1str) t)
     ((string-match base-regex key2str) nil)
     (t (string< key1str key2str)))))

(defun cabal-subsection-arrange-lines ()
  "Sort lines of current subsection."
  (interactive)
  (cabal-save-position
   (let* ((subsection (cabal-section-name (cabal-subsection)))
          (compare-lines (if (string= (downcase subsection) "build-depends")
                             'cabal-sort-lines-depends-compare
                           nil)))
     (cabal-with-subsection
      (cabal-subsection) t
      (cabal-with-cs-list
       (sort-subr nil 'forward-line 'end-of-line
                  'cabal-sort-lines-key-fun
                  'end-of-line
                  compare-lines))))))

(defun cabal-subsection-beginning ()
  "Find the beginning of the current subsection."
  (save-excursion
    (while (and (not (bobp))
                (not (cabal-header-p)))
      (forward-line -1))
    (back-to-indentation)
    (point)))

(defun cabal-beginning-of-subsection ()
  "Go to the beginning of the current subsection."
  (interactive)
  (goto-char (cabal-subsection-beginning)))

(defun cabal-next-subsection ()
  "Go to the next subsection."
  (interactive)
  (if (cabal-header-p) (forward-line))
  (while (and (not (eobp))
              (not (cabal-header-p)))
    (forward-line))
  (cabal-forward-to-line-entry))

(defun cabal-previous-subsection ()
  "Go to the previous subsection."
  (interactive)
  (if (cabal-header-p) (forward-line -1))
  (while (and (not (bobp))
              (not (cabal-header-p)))
    (forward-line -1))
  (cabal-forward-to-line-entry))


(defun cabal-find-subsection-by (section pred)
  "Find SECTION where PRED is t."
  (save-excursion
    (when section (goto-char (cabal-section-start section)))
    (let* ((end (if section (cabal-section-end) (point-max)))
           (found nil))
      (while (and (< (point) end)
                  (not found))
        (let ((subsection (cabal-subsection)))
          (when (and subsection (funcall pred subsection))
            (setq found subsection)))
        (cabal-next-subsection))
      found)))

(defun cabal-find-subsection (section name)
  "Find SECTION with name NAME."
  (let ((downcase-name (downcase name)))
    (cabal-find-subsection-by
     section
     `(lambda (subsection)
        (string= (downcase (cabal-section-name subsection))
                 ,downcase-name)))))

(defun cabal-goto-subsection (name)
  (let ((subsection (cabal-find-subsection (cabal-section) name)))
    (when subsection
      (goto-char (cabal-section-start subsection)))))

(defun cabal-goto-exposed-modules ()
  (interactive)
  (cabal-goto-subsection "exposed-modules"))

(defun cabal-subsection-entry-list (section name)
  "Get the data of a SECTION named NAME as a list."
  (let ((subsection (cabal-find-subsection section name)))
    (when subsection
      (cabal-with-subsection
       subsection nil
       (cabal-with-cs-list
        (delete-matching-lines
         (format "\\(?:%s\\)\\|\\(?:%s\\)"
                 cabal-comment-regexp
                 cabal-empty-regexp)
         (point-min) (point-max))
        (split-string (buffer-substring-no-properties (point-min) (point-max))
                      "\n" t))))))

(defun cabal-remove-mark ()
  (remove-list-of-text-properties (point-min) (point-max)
                                  '(cabal-marker)))


(defun cabal-mark ()
  "Mark the current position with the text property cabal-marker."
  (cabal-remove-mark)
  (put-text-property (line-beginning-position) (line-end-position)
                     'cabal-marker 'marked-line)
  (put-text-property (point) (1+ (point))
                     'cabal-marker 'marked))


(defun cabal-goto-mark ()
  "Go to marked line."
  (let ((marked-pos (text-property-any (point-min) (point-max)
                                       'cabal-marker
                                       'marked))
        (marked-line (text-property-any (point-min) (point-max)
                                        'cabal-marker
                                        'marked-line) ))
    (cond (marked-pos (goto-char marked-pos))
          (marked-line (goto-char marked-line)))))

(defmacro cabal-with-subsection-line (replace &rest forms)
  "Mark line, copy subsection data into a temporary buffer, save indentation
and execute FORMS at the marked line.

If REPLACE is non-nil the subsection data is replaced with the
resulting buffer-content.  Unmark line at the end."
  `(progn
     (cabal-mark)
     (unwind-protect
         (cabal-with-subsection (cabal-subsection) ,replace
                                (cabal-goto-mark)
                                ,@forms)
       (cabal-remove-mark))))


(defun cabal-get-line-content ()
  (cabal-with-subsection-line
   nil
   (cabal-with-cs-list
    (cabal-goto-mark)
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position)))))

(defun cabal-module-to-filename (module)
  (concat (replace-regexp-in-string "[.]" "/" module ) ".hs"))

(defconst cabal-module-sections '("exposed-modules" "other-modules")
  "List of sections that contain module names.")

(defconst cabal-file-sections
  '("main-is" "c-sources" "data-files" "extra-source-files"
    "extra-doc-files" "extra-tmp-files" )
  "List of subsections that contain filenames.")

(defconst cabal-source-bearing-sections
  '("library" "executable" "test-suite" "benchmark"))

(defun cabal-source-section-p (section)
  "Wheter given SECTION is a source section, so not a common section."
  (not (not (member (downcase (cabal-section-name section))
                    cabal-source-bearing-sections))))

(defun cabal-line-filename ()
  "Expand filename in current line according to the subsection type.

Module names in exposed-modules and other-modules are expanded by replacing each
dot (.) in the module name with a forward slash (/) and appending \".hs\"

Example: Foo.Bar.Quux ==> Foo/Bar/Quux.hs

Source names from main-is and c-sources sections are left untouched."
  (let ((entry (cabal-get-line-content))
        (subsection (downcase (cabal-section-name
                               (cabal-subsection)))))
    (cond ((member subsection cabal-module-sections)
           (cabal-module-to-filename entry))
          ((member subsection cabal-file-sections) entry))))

(defun cabal-join-paths (&rest args)
  "Crude hack to replace f-join."
  (mapconcat 'identity args "/"))

(defun cabal-find-or-create-source-file ()
  "Open the source file this line refers to."
  (interactive)
  (let* ((src-dirs (append (cabal-subsection-entry-list
                            (cabal-section) "hs-source-dirs")
                           '("")))
         (base-dir (file-name-directory (buffer-file-name)))
         (filename (cabal-line-filename)))
    (when filename
      (let ((candidates
             (delq nil (mapcar
                        (lambda (dir)
                          (let ((file (cabal-join-paths base-dir
                                                        dir
                                                        filename)))
                            (when (and (file-readable-p file)
                                       (not (file-directory-p file)))
                              file)))
                        src-dirs))))
        (if (null candidates)
            (unwind-protect
                (progn
                  (cabal-mode-toggle-interactive-prompt-state)
                  (let* ((src-dir
                          (cabal-join-paths base-dir
                                            (or (car src-dirs) "")))
                         (newfile (cabal-join-paths src-dir filename))
                         (do-create-p (y-or-n-p (format "Create file %s ?" newfile))))
                    (when do-create-p
                      (find-file-other-window newfile ))))
              (cabal-mode-toggle-interactive-prompt-state t))
          (find-file-other-window (car candidates)))))))


(defun cabal-find-section-type (type &optional wrap)
  "Find a section section-type TYPE.

Restart WRAP is t."
  (save-excursion
    (cabal-next-section)
    (while
        (not
         (or
          (eobp)
          (string=
           (downcase type)
           (downcase (cabal-section-name (cabal-section))))))
      (cabal-next-section))
    (if (eobp)
        (if wrap (progn
                   (goto-char (point-min))
                   (cabal-find-section-type type nil) )
          nil)
      (point))))

(defun cabal-goto-section-type (type)
  (let ((section (cabal-find-section-type type t)))
    (if section (goto-char section)
      (message "No %s section found" type))))

(defun cabal-goto-library-section ()
  (interactive)
  (cabal-goto-section-type "library"))

(defun cabal-goto-test-suite-section ()
  (interactive)
  (cabal-goto-section-type "test-suite"))

(defun cabal-goto-executable-section ()
  (interactive)
  (cabal-goto-section-type "executable"))

(defun cabal-goto-benchmark-section ()
  (interactive)
  (cabal-goto-section-type "benchmark"))

(defun cabal-goto-common-section ()
  (interactive)
  (cabal-goto-section-type "common"))


(defun cabal-line-entry-column ()
  "Column at which the line entry start."
  (save-excursion
    (cl-case (cabal-classify-line)
      (section-data (beginning-of-line)
                    (when (looking-at "[ ]*\\(?:,[ ]*\\)?")
                      (goto-char (match-end 0))
                      (current-column)))
      (subsection-header
       (cabal-section-data-start-column (cabal-subsection))))))

(defun cabal-forward-to-line-entry ()
  "Go forward to the beginning of the line entry (but never move backwards)."
  (let ((col (cabal-line-entry-column)))
    (when (and col (< (current-column) col))
      (beginning-of-line)
      (forward-char col))))

(defun cabal-indent-line ()
  "Indent current line according to subsection."
  (interactive)
  (cl-case (cabal-classify-line)
    (section-data
     (save-excursion
       (let ((indent (cabal-section-data-indent-column
                      (cabal-subsection))))
         (indent-line-to indent)
         (beginning-of-line)
         (when (looking-at "[ ]*\\([ ]\\{2\\},[ ]*\\)")
           (replace-match ", " t t nil 1)))))
    (empty
     (indent-relative)))
  (cabal-forward-to-line-entry))

(defun cabal-map-sections (fun)
  "Execute FUN over each section, collecting the result."
  (save-excursion
    (goto-char (point-min))
    (let ((results nil))
      (while (not (eobp))
        (let* ((section (cabal-section))
               (result (and section (funcall fun (cabal-section)))))
          (when section (setq results (cons result results))))
        (cabal-next-section))
      (nreverse results))))

(defun cabal-section-add-build-dependency (dependency &optional sort sec)
  "Add a build DEPENDENCY to the build-depends section."
  (let* ((section (or sec (cabal-section)))
         (subsection (and section
                          (cabal-find-subsection section "build-depends"))))
    (when subsection
      (cabal-with-subsection
       subsection t
       (cabal-with-cs-list
        (insert dependency)
        (insert "\n")
        (when sort
          (goto-char (point-min))
          (sort-subr nil 'forward-line 'end-of-line
                     'cabal-sort-lines-key-fun)))))))

(defun cabal-add-build-dependency (dependency &optional sort silent)
  "Add the given DEPENDENCY to every section in cabal file.
If SORT argument is given sort dependencies in section after update.
Pass SILENT argument to update all sections without asking user."
  (cabal-map-sections
   (lambda (section)
     (when (cabal-source-section-p section)
       (unwind-protect
           (progn
             (when
                 (or silent
                     (y-or-n-p (format "Add dependency %s to %s section %s?"
                                       dependency
                                       (cabal-section-name section)
                                       (cabal-section-value section))))
               (cabal-section-add-build-dependency dependency
                                                   sort
                                                   section))
             nil)
         (cabal-mode-toggle-interactive-prompt-state t))))))

(defun cabal-add-dependency
    (package &optional version no-prompt sort silent)
  "Add PACKAGE to the cabal file.
If VERSION is non-nil it will be appended as a minimum version.
If NO-PROMPT is nil the minimum package version is read from the
minibuffer.  When SORT is non-nil the package entries are sorted
afterwards.  If SILENT is non-nil the user is prompted for each
source-section."
  (interactive
   (list (read-from-minibuffer "Package entry: ") nil t t nil))
  (cabal-mode-toggle-interactive-prompt-state)
  (unwind-protect
      (save-window-excursion
        (find-file-other-window (cabal-find-file))
        (let ((entry (if no-prompt package
                       (read-from-minibuffer
                        "Package entry: "
                        (concat package
                                (if version (concat " >= " version) ""))))))
          (cabal-add-build-dependency entry sort silent)
          (when (or silent (y-or-n-p "Save cabal file? "))
            (save-buffer))))
    ;; unwind
    (cabal-mode-toggle-interactive-prompt-state t)))


(defun cabal--find-tags-dir ()
  "Return a directory where TAGS file will be generated.
Tries to find cabal file first and if succeeds uses its location.
If cabal file not found uses current file directory.  If current
buffer not visiting a file returns nil."
  (or (cabal-find-dir)
      (when buffer-file-name
        (file-name-directory buffer-file-name))))

(provide 'cabal-mode)
;;; cabal-mode.el ends here
