# hexl-utf8

An Emacs minor mode for `hexl-mode` that replaces the right-hand ASCII column with UTF-8 decoded text, making Japanese kanji, kana, CJK ideographs, emoji, and other multi-byte characters readable while the hex column on the left remains unchanged.

`hexl-mode` の右側 ASCII カラムを UTF-8 デコードテキストに差し替え、漢字・かな・絵文字などを判読可能にする Emacs マイナーモードです。

## Features

- Replaces the ASCII column in `hexl-mode` with UTF-8 decoded characters
- Cursor tracking: highlights the decoded character corresponding to the byte under the cursor
- Multi-byte characters are highlighted as a whole regardless of which constituent byte the cursor is on
- Configurable placeholder characters for continuation bytes, invalid bytes, and control characters

## Requirements

- Emacs 27.1 or later

## Installation

### Using straight.el

```elisp
(straight-use-package
 '(hexl-utf8 :type git :host github :repo "fvi-att/hexl-utf.el"))
```

### Using straight.el with use-package

```elisp
(use-package hexl-utf8
  :straight (hexl-utf8 :type git :host github :repo "fvi-att/hexl-utf.el")
  :hook (hexl-mode . hexl-utf8-mode))
```

### Manual

Download `hexl-utf8.el` and place it in your `load-path`, then:

```elisp
(require 'hexl-utf8)
(add-hook 'hexl-mode-hook #'hexl-utf8-mode)
```

## Usage

Enable `hexl-utf8-mode` manually in a `hexl-mode` buffer:

```
M-x hexl-utf8-mode
```

Or enable automatically whenever `hexl-mode` is activated:

```elisp
(add-hook 'hexl-mode-hook #'hexl-utf8-mode)
```

To force a refresh of the decoded column:

```
M-x hexl-utf8-refresh
```

## Customization

| Variable | Default | Description |
|---|---|---|
| `hexl-utf8-continuation-char` | `·` | Character shown for UTF-8 continuation bytes |
| `hexl-utf8-invalid-char` | `?` | Character shown for invalid UTF-8 bytes |
| `hexl-utf8-control-char` | `.` | Character shown for ASCII control bytes |
| `hexl-utf8-update-idle` | `0.15` | Idle seconds before refreshing overlays after a buffer change |

Faces: `hexl-utf8-ascii-face`, `hexl-utf8-multibyte-face`, `hexl-utf8-placeholder-face`, `hexl-utf8-cursor-face`

## License

This project is available under the terms of the GNU General Public License v3.0 or later (GPL-3.0-or-later).
