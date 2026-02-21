;;; lsp-glint.el --- LSP support for Ember Glint -*- lexical-binding: t -*-

;; Author: Doug Headley <doug@dougheadley.com>
;; Keywords: languages, tools
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1
;; URL: https://github.com/overcast-software/lsp-glint

;;; Commentary:
;; LSP support for Glint (.gts/.gjs) files.

;;; Code:

;;; lsp-glint.el --- Glint LSP client -*- lexical-binding: t; -*-

(require 'lsp-mode)
(require 'cl-lib)

;; --- GTS MODE -------------------------------------------------

;; Simple derived mode so we have a distinct major-mode
(define-derived-mode gts-mode typescript-ts-mode "GTS"
  "Major mode for Glint .gts files.")

(add-to-list 'auto-mode-alist '("\\.gts\\'" . gts-mode))
(add-to-list 'auto-mode-alist '("\\.gjs\\'" . js-ts-mode))

;; --- DEBUG ---------------------------------------------------

(defvar lsp-glint-debug t)

(defun lsp-glint--log (fmt &rest args)
  (when lsp-glint-debug
    (apply #'message (concat "[lsp-glint] " fmt) args)))

;; --- SERVER COMMAND -------------------------------------------

(defun lsp-glint--server-command ()
  (let ((cmd (or (executable-find "glint-language-server")
                 (executable-find "node_modules/.bin/glint-language-server")
                 "glint-language-server")))
    (lsp-glint--log "Using server: %s" cmd)
    (list cmd "--stdio")))

;; --- ROOT DETECTION --------------------------------------------

(defun lsp-glint-root ()
  (or (locate-dominating-file default-directory "glint.json")
      (locate-dominating-file default-directory "tsconfig.json")
      (locate-dominating-file default-directory "package.json")
      (lsp--suggest-project-root)))

;; --- DISABLE TSSERVER IN GTS ----------------------------------

(defun lsp-glint-disable-ts ()
  (when (and buffer-file-name
             (string-match-p "\\.g[jt]s$" buffer-file-name))
    (setq-local lsp-disabled-clients '(ts-ls))
    (lsp-glint--log "Disabled ts-ls in this buffer")))

(add-hook 'gts-mode-hook #'lsp-glint-disable-ts)
(add-hook 'js-ts-mode-hook #'lsp-glint-disable-ts)

;; --- REGISTER CLIENT ------------------------------------------

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-glint--server-command)
  :activation-fn (lsp-activate-on "glint")
  :major-modes '(gts-mode js-ts-mode)
  :priority 10
  :server-id 'glint
  :root-directory #'lsp-glint-root))

;; --- LANGUAGE ID ----------------------------------------------

(add-to-list 'lsp-language-id-configuration '(gts-mode . "glint"))
(add-to-list 'lsp-language-id-configuration '(js-ts-mode . "glint"))

(provide 'lsp-glint)
;;; lsp-glint.el ends here
