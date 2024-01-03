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
           #:get-group
           #:regex-scanner #:scanner
           #:make-scanner
           #:make-command-regex-scanner))


(defpackage :datapouch.cli
  (:use #:cl)
  (:nicknames :d.cli)
  (:export #:+default-line-separator+
           #:+default-space-characters+
           #:*buffer*
           #:*there-is-no-fresh-line-now*
           #:*prompt-fun*
           #:*custom-readtable*
           #:readline
           #:read-form
           #:disable-bracketed-paste
           #:restore-bracketed-paste
           #:get-repl))


(defpackage :datapouch.reader-macro
  (:use #:cl #:d.cli #:d.regex)
  (:nicknames :d.rmacro)
  (:export #:command #:command-regex #:command-handler
           #:*commands*
           #:command-reader-macro
           #:install-command-reader-macro
           #:read-line-to-semicolon-or-newline))


(defpackage :datapouch.filesystem
  (:use #:cl #:uiop)
  (:nicknames :d.fs)
  (:export #:+application-folder+
           #:+working-directory+
           #:+database-extension+
           #:+checksum-extension+
           #:*database-path*
           #:*history-path*
           #:*backup-tiers*
           #:ensure-file-exists
           #:init-database-file
           #:init-application-files
           #:process-all-backup-tiers))


(defpackage :datapouch.sql
  (:use #:cl)
  (:nicknames :d.sql)
  (:import-from :d.fs
                #:*database-path*)
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
  (:export #:*db* ; XXX: but should it?
           #:open-db
           #:close-db
           #:select #:union-queries #:union-all-queries
           #:insert-into #:update #:delete-from
           #:create-table #:drop-table #:alter-table
           #:create-index #:drop-index
           #:use-foreign-keys
           #:check-integrity
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


(defpackage :datapouch.crypto
  (:use #:cl #:uiop #:ironclad)
  (:nicknames :d.crypto)
  (:import-from :d.fs
                #:+checksum-extension+
                #:*database-path*
                #:*backup-tiers*)
  (:export #:*control-database-integrity*
           #:*advise-full-sqlite-integrity-check*
           #:sha512-for-file
           #:write-checksum-to-file
           #:read-checksum-from-file
           #:check-database-integrity
           #:rehash-database))


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
  (:export #:*preload-hooks*
           #:*post-unload-hooks*
           #:*init-hooks*
           #:*exit-hooks*
           #:*debugger-hooks*
           #:make-image))


(defpackage :datapouch.interaction
  (:use #:cl)
  (:nicknames :d.inter)
  (:import-from #:d.cli
                #:readline
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
           #:find-row-dialog))


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
(cl-reexport:reexport-from :datapouch.crypto)
(cl-reexport:reexport-from :datapouch.editor)
(cl-reexport:reexport-from :datapouch.interaction)
(cl-reexport:reexport-from :datapouch.main)
(in-package :cl-user)
