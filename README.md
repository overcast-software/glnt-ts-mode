![CI](https://github.com/overcast-software/lsp-glint/actions/workflows/ci.yml/badge.svg)

# glint-ts-mode

Tree-sitter major mode and LSP integration for **Ember Glint** (`.gts` / `.gjs`) files in Emacs.

This package provides:

- `glint-ts-mode` — Tree-sitter major mode derived from `tsx-ts-mode`
- `lsp-glint` — `lsp-mode` client for `glint-language-server`

Designed for **Emacs 30+ only**.

---

## Requirements

- Emacs **30.1+**
- Node.js
- [`glint-language-server`](https://github.com/typed-ember/glint)
- `lsp-mode` 9.0+

---

## Installation

### MELPA

```elisp
M-x package-install RET glint-ts-mode
```

### use-package

```elisp
(use-package glint-ts-mode
  :ensure t)
```
Opening a .gts or .gjs file will activate the mode and start LSP automatically.

## Usage

Open a .gts or .gjs file.

- `glint-ts-mode` is enabled automatically
- `glint-language-server` starts via `lsp-mode`
- TypeScript LSP (ts-ls) is disabled for Glint buffers

## Tree-sitter Setup

This package relies on native Tree-sitter support in Emacs 30.

Install required grammars:

```elisp
M-x treesit-install-language-grammar RET tsx
M-x treesit-install-language-grammar RET typescript
```

### Optional: Glimmer grammar

This step is only if you need highlighting for GLimmer templates.

```elisp
M-x treesit-install-language-grammar RET glimmer
```

To make these grammars available:

```elisp
(setq treesit-language-source-alist
      '((tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (glimmer "https://github.com/alexlafroscia/tree-sitter-glimmer")))
```

More information: https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-Program-Source.html

## Customization
### Node memory settings

```elisp
(setq lsp-node-args '("--max-old-space-size=4096"))
```

## Design Notes

- glint-ts-mode derives from tsx-ts-mode
- Tree-sitter highlighting uses the TSX grammar
- Glint is treated as TypeScript at the LSP protocol level
- No legacy tree-sitter-mode support is included

## License

MIT

