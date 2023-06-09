;;====================================================================
;; Grew mode
;; Mode used to write Grew with emacs (highlight)

(require 'generic-x) ;;pour Emacs OK, mais semble ne pas marcher avec XEmacs
(define-generic-mode 'grew-mode
  '("%");;comments
  '("rule" "without" "with" "commands" "global" "import" "include");;keywords
  '(
;;    ("class\\s (\*\*)* +\\(\\sw[a-zA-Z0-9_.-]*\\)" 1 'font-lock-type-face);noms de classes
;;    ("\?[a-zA-Z0-9]+" . font-lock-variable-name-face)
;;    ("\![a-zA-Z0-9]+" . font-lock-constant-face)
;;    ("\(\\(\\sw[a-zA-Z0-9_.-]*\\(,\\sw[a-zA-Z0-9_.-]*\\)*\\)\)" 1 font-lock-constant-face);;params & node props
;;    ("$\\(\\sw*\\)*" . font-lock-constant-face);;params inside

;;    ("$[a-zA-Z0-9_àéèçâôûêäïüö'\-]+" . font-lock-constant-face);;params inside
    ("del_edge" . font-lock-constant-face)
    ("add_edge" . font-lock-constant-face)
    ("shift" . font-lock-constant-face)
    ("shift_in" . font-lock-constant-face)
    ("shift_out" . font-lock-constant-face)
    ("del_node" . font-lock-constant-face)
    ("add_node" . font-lock-constant-face)
    ("del_feat" . font-lock-constant-face)
    ("append_feats" . font-lock-constant-face)
    ("prepend_feats" . font-lock-constant-face)
    ("@[a-zA-Z0-9_]+" . font-lock-variable-name-face)
    ("$[a-zA-Z0-9_]+" . font-lock-variable-name-face)

    )
  '(".grs\\'") ;;file extension
  nil
  "Major mode for grew file")
