;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((haskell-mode
  . ((dante-target       . "lib:spiros")
     (dante-project-root . "~/haskell/spiros/"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE ;;
;;
;; use the nearest `cabal.project' (found via `locate-dominating-file'):
;; `default-directory' is w.r.t. the current file, not this file (i.e. `.dir-locals.el').
;;
;; use `haskell-mode' (not `dante-mode'):
;; because a `.dir-locals.el' associates only major (not minor) modes with variables.
;;

;;TODO (locate-dominating-file default-directory "cabal.project")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;