;;; lsp-glint.el --- LSP support for Ember Glint -*- lexical-binding: t -*-

;; Author: Doug Headley <doug@dougheadley.com>
;; Keywords: languages, tools
;; Package-Requires: ((emacs "29.1") (lsp-mode "8.0") (glint-mode "0.1"))
;; Version: 0.1
;; URL: https://github.com/overcast-software/lsp-glint

;;; Commentary:
;; LSP support for Glint (.gts/.gjs) files.

;;; Code:

(require 'lsp-mode)
(require 'glint-mode)

;; --- DEBUG ---------------------------------------------------

(defgroup lsp-glint nil
  "LSP support for Glint."
  :group 'lsp-mode
  :link '(url-link "https://github.com/overcast-software/lsp-glint"))

(defcustom lsp-glint-debug t
  "Enable debug logging for lsp-glint."
  :type 'boolean
  :group 'lsp-glint)

(defun lsp-glint--log (fmt &rest args)
  "Log a debug message with FORMAT and ARGS."
  (when lsp-glint-debug
    (apply #'message (concat "[lsp-glint] " fmt) args)))

(defun lsp-glint-debug-info ()
  "Display debug information about the current Glint setup."
  (interactive)
  (let ((buf (get-buffer-create "*lsp-glint-debug*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== LSP Glint Debug Info ===\n\n")
      
      (insert "** Buffer Info **\n")
      (insert (format "Buffer: %s\n" (buffer-name)))
      (insert (format "Major mode: %s\n" major-mode))
      (insert (format "File: %s\n" (or buffer-file-name "no file")))
      (insert (format "Default directory: %s\n\n" default-directory))
      
      (insert "** Server Command **\n")
      (condition-case err
          (insert (format "Command: %s\n" (lsp-glint--server-command)))
        (error (insert (format "ERROR: %s\n" err))))
      (insert (format "glint-language-server in PATH: %s\n"
                      (executable-find "glint-language-server")))
      (insert (format "Local node_modules: %s\n\n"
                      (executable-find "node_modules/.bin/glint-language-server")))
      
      (insert "** Project Root **\n")
      (condition-case err
          (insert (format "Root: %s\n" (lsp-glint-root)))
        (error (insert (format "ERROR: %s\n" err))))
      (insert (format "glint.json: %s\n"
                      (locate-dominating-file default-directory "glint.json")))
      (insert (format "tsconfig.json: %s\n"
                      (locate-dominating-file default-directory "tsconfig.json")))
      (insert (format "package.json: %s\n\n"
                      (locate-dominating-file default-directory "package.json")))
      
      (insert "** LSP State **\n")
      (insert (format "lsp-mode loaded: %s\n" (featurep 'lsp-mode)))
      (insert (format "LSP workspaces: %s\n" (length (lsp-workspaces))))
      (insert (format "Current workspace: %s\n"
                      (if (lsp-workspaces)
                          (lsp--workspace-print (car (lsp-workspaces)))
                        "none")))
      (insert (format "Disabled clients: %s\n" lsp-disabled-clients))
      (insert (format "Enabled clients: %s\n\n" lsp-enabled-clients))
      
      (insert "** Language ID Configuration **\n")
      (insert (format "gts-mode language: %s\n"
                      (alist-get 'gts-mode lsp-language-id-configuration)))
      (insert (format "All language IDs: %s\n\n" 
                      (seq-filter (lambda (x) 
                                    (or (eq (car x) 'gts-mode)
                                        (and (stringp (car x))
                                             (string-match-p "gjs\\|gts" (car x)))))
                                  lsp-language-id-configuration)))
      
      (insert "** Registered Clients **\n")
      (let ((glint-client (gethash 'glint lsp-clients)))
        (if glint-client
            (progn
              (insert (format "Glint client registered: YES\n"))
              (insert (format "Server ID: %s\n" (lsp--client-server-id glint-client)))
              (insert (format "Major modes: %s\n" (lsp--client-major-modes glint-client)))
              (insert (format "Priority: %s\n" (lsp--client-priority glint-client))))
          (insert "Glint client registered: NO\n")))
      (insert "\n")
      
      (insert "** Auto-mode-alist **\n")
      (dolist (entry auto-mode-alist)
        (when (and (stringp (car entry))
                   (string-match-p "gjs\\|gts" (car entry)))
          (insert (format "%s -> %s\n" (car entry) (cdr entry)))))
      
      (special-mode))
    (pop-to-buffer buf)))

;; --- SERVER COMMAND -------------------------------------------

(defun lsp-glint--server-command ()
  "Return the command to start the Glint language server."
  (let* ((local-server (expand-file-name "node_modules/.bin/glint-language-server" 
                                         (lsp-glint-root)))
         (cmd (cond
               ((file-executable-p local-server)
                (lsp-glint--log "Using local server: %s" local-server)
                local-server)
               ((executable-find "glint-language-server")
                (lsp-glint--log "Using global server: %s" 
                                (executable-find "glint-language-server"))
                (executable-find "glint-language-server"))
               (t
                (lsp-glint--log "WARNING: glint-language-server not found!")
                "glint-language-server"))))
    (lsp-glint--log "Server command: %s --stdio" cmd)
    (list cmd "--stdio")))

;; --- ROOT DETECTION --------------------------------------------

(defun lsp-glint-root ()
  "Find the root directory for the Glint project."
  (let ((root (or (locate-dominating-file default-directory "glint.json")
                  (locate-dominating-file default-directory "tsconfig.json")
                  (locate-dominating-file default-directory "package.json")
                  (lsp--suggest-project-root))))
    (lsp-glint--log "Project root: %s" root)
    root))

;; --- DISABLE TSSERVER IN GTS ----------------------------------

(defun lsp-glint-disable-ts ()
  "Disable TypeScript language server in Glint files."
  (when (and buffer-file-name
             (string-match-p "\\.g[jt]s$" buffer-file-name))
    (lsp-glint--log "Disabling ts-ls for file: %s" buffer-file-name)
    (setq-local lsp-disabled-clients '(ts-ls))
    (lsp-glint--log "Disabled clients: %s" lsp-disabled-clients)))

(add-hook 'gts-mode-hook #'lsp-glint-disable-ts)
(add-hook 'js-ts-mode-hook #'lsp-glint-disable-ts)

;; --- REGISTER CLIENT ------------------------------------------

(lsp-glint--log "Registering Glint LSP client...")

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-glint--server-command)
  :activation-fn (lsp-activate-on "typescript" "javascript")
  :major-modes '(gts-mode)
  :priority 10
  :server-id 'glint
  :add-on? nil
  :initialization-options (lambda () (ht))
  :notification-handlers (ht)
  :action-handlers (ht)
  :after-open-fn (lambda ()
                   (lsp-glint--log "Glint LSP client opened for buffer: %s" 
                                   (buffer-name)))))

(lsp-glint--log "Glint LSP client registered with server-id: glint")

;; --- LANGUAGE ID ----------------------------------------------

;; Glint handles both .gts (TypeScript) and .gjs (JavaScript) files
(add-to-list 'lsp-language-id-configuration '(gts-mode . "typescript"))
(add-to-list 'lsp-language-id-configuration '("\\.gjs\\'" . "javascript"))

(provide 'lsp-glint)
;;; lsp-glint.el ends here
