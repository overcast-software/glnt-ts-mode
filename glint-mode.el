;;; glint-mode.el --- Major mode for Glint (.gts/.gjs) files -*- lexical-binding: t -*-

;; Author: Doug Headley <doug@dougheadley.com>
;; Keywords: languages
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1
;; URL: https://github.com/overcast-software/lsp-glint

;;; Commentary:
;; Major mode for editing Glint template files (.gts/.gjs).

;;; Code:

;; Simple derived mode so we have a distinct major-mode
;; We derive from prog-mode to avoid tree-sitter dependencies
(define-derived-mode gts-mode prog-mode "GTS"
  "Major mode for Glint .gts files."
  (setq-local comment-start "//")
  (setq-local comment-end ""))

(add-to-list 'auto-mode-alist '("\\.gts\\'" . gts-mode))
(add-to-list 'auto-mode-alist '("\\.gjs\\'" . gts-mode))

(provide 'glint-mode)
;;; glint-mode.el ends here
