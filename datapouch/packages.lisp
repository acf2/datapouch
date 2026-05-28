;;;; packages.lisp


(in-package :cl-user)


(defpackage :datapouch.interface
  (:use #:cl)
  (:nicknames :d.iface)
  (:export #:concat-two #:concat-many #:concat
           #:combine-two #:combine-many #:combine
           #:put-into #:get-from))


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
           #:list-of-strings
           #:list-of-list-of-strings
           #:repeat-string
           #:prefix?
           #:common-prefix
           #:add-to-assoc!
           #:member-of
           #:concat-keyword
           #:get-keys-from-hash-table
           #:check-directed-graph-for-cycles
           #:cartesian-product
           #:traverse))


(defpackage :datapouch.regex-support
  (:use #:cl #:cl-ppcre #:d.iface)
  (:nicknames :d.regex)
  (:import-from :d.aux
                #:list-of-strings)
  (:export #:allow-named-registers
           #:regex #:tree #:group-map
           #:regex-from-string
           #:wrap-in-noncapturing-group
           #:make-named-group
           #:concat-separated #:optional-concat
           #:interchange #:interchange-three
           #:scan
           #:get-group #:is-group #:list-group-names
           #:regex-scanner #:scanner #:group-list
           #:make-scanner
           #:match-to-assoc
           #:match-to-group-table
           #:match-to-group-tree
           #:scan-to-tree
           #:sampled-regex
           #:regex-allows-all-samples
           #:regex-denies-all-samples
           #:find-incompatible-sampled-regexes))


(defpackage :datapouch.cli
  (:use #:cl)
  (:nicknames :d.cli)
  (:export #:+default-line-separator+
           #:+default-space-characters+
           #:*buffer*
           #:*add-fresh-line-after-each-result-print*
           #:*prompt-fun*
           #:*datapouch-readtable*
           #:*noprint-result* #:*noprint-prompt*
           #:readline
           #:read-form
           #:disable-bracketed-paste
           #:restore-bracketed-paste
           #:get-parametrized-repl-read-form
           #:*heretical-repl-available*
           #:repl-fun-with-readline
           #:register-datapouch-autocomplete))


(defpackage :datapouch.command.reader-macro
  (:use #:cl)
  (:nicknames :d.rmacro)
  (:export #:*rmacro-callbacks*
           #:*stop-characters*
           #:install-command-reader-macro
           #:install-command-reader-autoprint-hook
           #:read-line-up-to))


(defpackage :datapouch.command.auxiliary
  (:use #:cl #:d.regex)
  (:nicknames :d.c.aux)
  (:export #:make-regex-parser
           #:make-rmacro-callback
           #:*saved-parsers-function-list*
           #:with-immutable-parsers))


(defpackage :datapouch.command.expression
  (:use #:cl #:d.rmacro)
  (:nicknames :d.expr)
  (:import-from :d.iface
                #:put-into
                #:get-from)
  (:import-from :d.aux
                #:*debug-output*
                #:list-existing*)
  (:export
    #:expression #:get-named-regex-group #:expression-type #:handler #:config #:docs
    #:expression-config #:use-nongroup-argument #:allow-traversal
    #:create-expression
    #:lexicon
    #:set-in-lexicon #:get-from-lexicon
    ;#:named-result #:result-name #:result-value ; XXX: TBD, should be internal-only ???
    #:make-result #:return-match #:return-named-match
    #:wrap-with-lexicon
    #:make-command #:with-lexicon #:with-new-lexicon #:with-anonymous-lexicon
    #:set-expressions #:make-commands))


(defpackage :datapouch.command.pattern
  (:use #:cl)
  (:nicknames :d.ptrn)
  (:import-from :d.iface
                #:put-into
                #:get-from))


(defpackage :datapouch.application
  (:use #:cl)
  (:nicknames :d.app)
  (:export #:*application-stack*
           #:application
           #:push-new-application #:get-current-return
           #:app-read-form
           #:get-app-repl-read-form))


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
                #:is-group)
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
(cl-reexport:reexport-from :datapouch.interface)
(cl-reexport:reexport-from :datapouch.auxiliary)

(cl-reexport:reexport-from :datapouch.regex-support)

(cl-reexport:reexport-from :datapouch.command.reader-macro)
(cl-reexport:reexport-from :datapouch.command.auxiliary)
(cl-reexport:reexport-from :datapouch.command.expression)
(cl-reexport:reexport-from :datapouch.command.pattern)

(cl-reexport:reexport-from :datapouch.application)

(cl-reexport:reexport-from :datapouch.sql)
(cl-reexport:reexport-from :datapouch.sql.auxiliary)

(cl-reexport:reexport-from :datapouch.filesystem)
(cl-reexport:reexport-from :datapouch.crypto)

(cl-reexport:reexport-from :datapouch.cli)
(cl-reexport:reexport-from :datapouch.editor)
(cl-reexport:reexport-from :datapouch.interaction)

(cl-reexport:reexport-from :datapouch.main)
(in-package :cl-user)
