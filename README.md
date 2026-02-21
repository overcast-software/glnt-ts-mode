## Installation

M-x package-install RET lsp-glint

(require 'lsp-glint)

## Usage

Open a .gts or .gjs file. LSP will start automatically.

## Customization

(setq lsp-glint-use-global t)
(setq lsp-node-args '("--max-old-space-size=4096"))
