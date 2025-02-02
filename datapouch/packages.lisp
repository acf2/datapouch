;;;; packages.lisp


(in-package :cl-user)


(defpackage :datapouch.auxiliary
  (:use #:cl)
  (:nicknames :d.aux)
  (:export #:*debug-output*
           #:list-existing
           #:list-existing*
           #:ensure-list
           #:append-lists
           #:map-append
           #:rotate
           #:repeat-string
           #:member-of
           #:get-keys-from-hash-table
           #:check-directed-graph-for-cycles
           #:cartesian-product))


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
           #:get-group #:is-group #:list-group-names
           #:regex-scanner #:scanner
           #:make-scanner
           ))


(defpackage :datapouch.cli
  (:use #:cl)
  (:nicknames :d.cli)
  (:export #:+default-line-separator+
           #:+default-space-characters+
           #:*buffer*
           #:*add-fresh-line-after-each-result-print*
           #:*prompt-fun*
           #:*custom-readtable*
           #:*noprint-result* #:*noprint-prompt*
           #:readline
           #:read-form
           #:disable-bracketed-paste
           #:restore-bracketed-paste
           #:get-repl-read-form
           #:*heretical-repl-available*
           #:repl-fun-with-readline))


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
  (:import-from :d.aux
                #:list-existing*)
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
           #:column-tuple
           #:execute #:query #:+statements+ #:build #:build-and-query
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


(defpackage :datapouch.sql.auxiliary
  (:use #:cl #:d.sql)
  (:nicknames :d.sql.aux)
  (:export #:make-name
           #:get-repeated-join-clause
           #:get-chained-table-expression
           #:get-table-power-expression))


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


(defpackage :datapouch.shell
  (:use #:cl #:d.rmacro)
  (:nicknames :d.shell)
  (:import-from :d.aux
                #:*debug-output*
                #:list-existing*)
  (:export 
    ;; S-form
    #:acceptable-regex?
    #:simple-regex-group?
    #:named-regex-group?
    #:acceptable-regex-group?
    #:simple-argument?
    #:acceptable-modifier-for-argument?
    #:argument-with-modifiers?
    #:acceptable-term?
    #:make-command-s-form-scanner
    ;; Expression
    #:subexpression-argument?
    #:acceptable-argument?
    #:acceptable-expression-s-form?
    #:acceptable-built-expression-s-form?
    #:shell-expression #:shell
    #:built-shell-expression-shard #:built-shell
    #:define-subexpression #:define-command
    #:build-expression #:build-shell
    #:make-shell-expression
    #:generate-commands-from-shell
    #:add-shell-subexpressions
    #:add-shell-commands
    #:add-help-to-shell))


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
  (:import-from :d.aux
                #:rotate
                #:repeat-string)
  (:import-from :d.cli
                #:readline
                #:read-form
                #:*prompt-fun*)
  (:import-from :d.regex
                #:make-scanner
                #:concat
                #:combine
                #:make-named-group
                #:get-group
                #:is-group
                #:scan-named-groups)
  (:export #:*max-string-length*
           #:*wrap-marker*
           #:*table-metaformat*
           #:*table-pad-width*
           #:*get-table-name-delimiter*
           #:dialog
           #:yes-or-no-dialog
           #:rotate
           #:find-max-field-widths
           #:pretty-print-rows
           #:pretty-print-table
           #:find-row-dialog
           #:find-row-with-peeking-dialog))


;;; Parent package, using cl-reexport
;;; (No, dun want asdf3 and bla-bla-bla. Muh luddite faith doesn't allow it.)
(defpackage :datapouch
  (:use #:cl)
  (:import-from #:sb-ext
                #:quit)
  (:export #:quit))


(in-package :datapouch)
(cl-reexport:reexport-from :datapouch.auxiliary)
(cl-reexport:reexport-from :datapouch.regex-support)
(cl-reexport:reexport-from :datapouch.cli)
(cl-reexport:reexport-from :datapouch.reader-macro)
(cl-reexport:reexport-from :datapouch.sql)
(cl-reexport:reexport-from :datapouch.sql.auxiliary)
(cl-reexport:reexport-from :datapouch.filesystem)
(cl-reexport:reexport-from :datapouch.crypto)
(cl-reexport:reexport-from :datapouch.editor)
(cl-reexport:reexport-from :datapouch.interaction)
(cl-reexport:reexport-from :datapouch.shell)
(cl-reexport:reexport-from :datapouch.main)
(in-package :cl-user)
