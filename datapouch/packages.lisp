;;;; packages.lisp


(in-package :cl-user)


(defpackage :datapouch.regex-support
  (:use #:cl #:cl-ppcre)
  (:nicknames :d.regex)
  (:export #:allow-named-registers
           #:regex #:expr #:groups
           #:wrap-in-noncapturing-group
           #:make-named-group
           #:concat-two
           #:concat
           #:combine
           #:interchange
           #:interchange-three
           #:scan-named-groups
           #:regex-scanner #:scanner
           #:make-scanner))


(defpackage :datapouch.cli
  (:use #:cl)
  (:nicknames :d.cli)
  (:export #:+default-line-separator+
           #:+default-space-characters+
           #:*buffer*
           #:*there-is-no-fresh-line-now*
           #:*prompt-fun*
           #:*custom-readtable*
           #:read-form
           #:disable-bracketed-paste
           #:restore-bracketed-paste
           #:get-repl))


(defpackage :datapouch.reader-macro
  (:use #:cl #:d.cli #:d.regex)
  (:nicknames :d.rmacro)
  (:export #:*commands*
           #:command-reader-macro
           #:install-command-reader-macro
           #:read-line-to-semicolon-or-newline))


(defpackage :datapouch.sql
  (:use #:cl)
  (:nicknames :d.sql)
  (:import-from :sxql
                ;; For re-export
                #:fields #:from #:where
                #:order-by #:group-by
                #:having #:returning #:limit
                #:offset
                #:set=
                #:inner-join #:left-join #:right-join #:full-join
                #:primary-key #:unique-key #:index-key #:foreign-key
                #:add-column
                ;; double check if they work in sqlite
                ;#:modify-column #:alter-column #:change-column #:drop-column #:add-primary-key #:drop-primary-key #:rename-to
                #:on-duplicate-key-update #:on-conflict-do-nothing #:on-conflict-do-update)
  (:export #:*db*
           #:select #:union-queries #:union-all-queries
           #:insert-into #:update #:delete-from
           #:create-table #:drop-table #:alter-table
           #:create-index #:drop-index
           #:use-foreign-keys
           #:integrity-check
           ;; Re-export from sxql
           #:fields #:from #:where
           #:order-by #:group-by
           #:having #:returning #:limit
           #:offset
           #:set=
           #:inner-join #:left-join #:right-join #:full-join
           #:primary-key #:unique-key #:index-key #:foreign-key
           #:add-column
           ;; double check if they work in sqlite
           ;#:modify-column #:alter-column #:change-column #:drop-column #:add-primary-key #:drop-primary-key #:rename-to
           #:on-duplicate-key-update #:on-conflict-do-nothing #:on-conflict-do-update))


(defpackage :datapouch.filesystem
  (:use #:cl #:uiop #:ironclad)
  (:nicknames :d.fs)
  (:export #:*database-path*
           #:*history-path*
           #:*backup-path*
           #:ensure-file-exists
           #:application-files-init))


(defpackage :datapouch.editor
  (:use #:cl #:uiop)
  (:nicknames :d.edit)
  (:export #:call-editor
           #:call-editor-for-many
           #:*editor-interface*
           #:edit-paths
           #:edit-strings))


(defpackage :datapouch.main
  (:use #:cl #:uiop #:d.fs #:d.edit)
  (:nicknames :d.main)
  (:import-from #:d.sql
                #:*db*
                #:use-foreign-keys)
  (:import-from #:d.cli
                #:*buffer*
                #:*there-is-no-fresh-line-now*
                #:disable-bracketed-paste
                #:restore-bracketed-paste
                #:get-repl)
  (:import-from #:d.rmacro
                #:install-command-reader-macro)
  (:export #:*preload-hooks*
           #:*init-hooks*
           #:*exit-hooks*
           #:*debugger-hooks*
           #:make-image))


(defpackage :datapouch.interaction
  (:use #:cl)
  (:nicknames :d.inter)
  (:import-from #:d.cli
                #:read-form
                #:*prompt-fun*)
  (:export #:*max-string-length*
           #:*wrap-marker*
           #:*table-metaformat*
           #:*table-pad-width*
           #:*get-table-name-delimiter*
           #:find-max-field-widths
           #:pretty-print-rows
           #:pretty-print-table
           #:find-one-row-dialog))


;;; Parent package, using cl-reexport
;;; (No, dun want asdf3 and bla-bla-bla. Muh luddite faith doesn't allow it.)
(defpackage :datapouch
  (:use #:cl)
  (:import-from #:sb-ext
                #:quit)
  (:export #:quit))

(in-package :datapouch)
(cl-reexport:reexport-from :datapouch.regex-support)
(cl-reexport:reexport-from :datapouch.cli)
(cl-reexport:reexport-from :datapouch.reader-macro)
(cl-reexport:reexport-from :datapouch.sql)
(cl-reexport:reexport-from :datapouch.filesystem)
(cl-reexport:reexport-from :datapouch.editor)
(cl-reexport:reexport-from :datapouch.interaction)
(cl-reexport:reexport-from :datapouch.main)
(in-package :cl-user)
