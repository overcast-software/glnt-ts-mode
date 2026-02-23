;;; lsp-glint.el --- LSP support for Ember Glint -*- lexical-binding: t; -*-

;; Author: Doug Headley <doug@dougheadley.com>
;; Keywords: languages, tools, lsp, ember, glint
;; Package-Requires: ((emacs "30.1") (lsp-mode "9.0"))
;; Version: 0.1.0
;; URL: https://github.com/overcast-software/glint-ts-mode

;;; Commentary:
;; LSP support for Ember Glint (.gts/.gjs) files using glint-language-server.

;;; Code:

(require 'lsp-mode)
(require 'glint-ts-mode)

(defgroup lsp-glint nil
  "LSP support for Ember Glint."
  :group 'lsp-mode)

;; ---------------------------------------------------------------------
;; Project root detection
;; ---------------------------------------------------------------------

(defun lsp-glint--project-root ()
  "Return project root directory for Glint."
  (or (locate-dominating-file default-directory "glint.json")
      (locate-dominating-file default-directory "tsconfig.json")
      (locate-dominating-file default-directory "package.json")
      (lsp-workspace-root)))

;; ---------------------------------------------------------------------
;; Language server command resolution
;; ---------------------------------------------------------------------

(defun lsp-glint--server-command ()
  "Return the command to start glint-language-server."
  (let* ((root (lsp-glint--project-root))
         (local (when root
                  (expand-file-name "node_modules/.bin/glint-language-server" root)))
         (cmd (cond
               ((and local (file-executable-p local)) local)
               ((executable-find "glint-language-server") "glint-language-server")
               (t (error "glint-language-server not found")))))
    (list cmd "--stdio")))

;; ---------------------------------------------------------------------
;; LSP client registration
;; ---------------------------------------------------------------------

;;;###autoload
(lsp-register-client
(make-lsp-client
 :new-connection (lsp-stdio-connection #'lsp-glint--server-command)
 :major-modes '(glint-ts-mode)
 :priority 10
 :server-id 'glint
 :multi-root t
 :initialized-fn (lambda (workspace)
                   ;; tell the server this is TypeScript
                   (with-lsp-workspace workspace
                     (lsp--set-configuration
                      `(:languageId "typescript"))))))
;; ---------------------------------------------------------------------
;; Disable TypeScript LSP in Glint buffers
;; ---------------------------------------------------------------------

(defun lsp-glint--disable-ts-ls ()
  "Disable TypeScript LSP in Glint buffers."
  (setq-local lsp-disabled-clients
              (append lsp-disabled-clients '(ts-ls))))

;; ---------------------------------------------------------------------
;; Auto-start hooks
;; ---------------------------------------------------------------------

;;;###autoload
(add-hook 'glint-ts-mode-hook #'lsp-glint--disable-ts-ls)

;;;###autoload
(add-hook 'glint-ts-mode-hook #'lsp-deferred)

(provide 'lsp-glint)
;;; lsp-glint.el ends here
