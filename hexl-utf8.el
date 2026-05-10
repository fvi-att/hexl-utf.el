;;; hexl-utf8.el --- UTF-8 decoded text column for hexl-mode -*- lexical-binding: t; -*-

;; Author: jshimizujp <jshimizujp@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: data, files, i18n, hex
;; URL: https://github.com/fvi-att/hexl-utf.el

;;; Commentary:

;; `hexl-utf8-mode' is a buffer-local minor mode for `hexl-mode' that
;; replaces the right-hand ASCII column with text decoded as UTF-8, so
;; that Japanese kanji, kana, CJK ideographs, emoji and other multi-byte
;; characters become readable while the hex column on the left remains
;; unchanged.
;;
;; Cursor tracking: when point is inside a hex byte, the corresponding
;; character in the decoded column is highlighted with
;; `hexl-utf8-cursor-face'.  For a multi-byte UTF-8 character, the
;; entire character is highlighted regardless of which constituent byte
;; the cursor is on.
;;
;; Usage:
;;
;;   (require 'hexl-utf8)
;;   (add-hook 'hexl-mode-hook #'hexl-utf8-mode)
;;
;; 日本語による説明:
;;
;; hexl-mode の右側 ASCII カラムを UTF-8 デコードテキストに差し替え、
;; 漢字・かな・絵文字などを判読可能にするマイナーモードです。
;; カーソルが載っているバイトに対応する文字はハイライト表示されます。
;; 多バイト文字はどのバイト（先頭・継続バイト）にいても文字全体が
;; ハイライトされます。

;;; Code:

(require 'hexl)
(require 'cl-lib)

(defgroup hexl-utf8 nil
  "UTF-8 decoded text column for `hexl-mode'."
  :group 'hexl
  :prefix "hexl-utf8-")

(defcustom hexl-utf8-continuation-char ?·
  "Character shown for UTF-8 continuation bytes (middle dot by default)."
  :type 'character
  :group 'hexl-utf8)

(defcustom hexl-utf8-invalid-char ??
  "Character shown for bytes that are not valid UTF-8."
  :type 'character
  :group 'hexl-utf8)

(defcustom hexl-utf8-control-char ?.
  "Character shown for ASCII control bytes."
  :type 'character
  :group 'hexl-utf8)

(defcustom hexl-utf8-update-idle 0.15
  "Idle seconds to wait before refreshing overlays after a buffer change."
  :type 'number
  :group 'hexl-utf8)

(defface hexl-utf8-ascii-face
  '((t :inherit default))
  "Face for plain ASCII characters in the decoded column."
  :group 'hexl-utf8)

(defface hexl-utf8-multibyte-face
  '((t :inherit font-lock-string-face))
  "Face for multi-byte UTF-8 characters (kanji, kana, emoji, ...)."
  :group 'hexl-utf8)

(defface hexl-utf8-placeholder-face
  '((t :inherit shadow))
  "Face for continuation, control and invalid placeholder characters."
  :group 'hexl-utf8)

(defface hexl-utf8-cursor-face
  '((((class color) (background dark))
     :background "#ff8c00" :foreground "black" :weight bold)
    (((class color) (background light))
     :background "#ff6f00" :foreground "white" :weight bold)
    (t :inverse-video t :weight bold))
  "Face applied to the decoded character under the hex cursor."
  :group 'hexl-utf8)

(defvar-local hexl-utf8--overlays nil
  "All line overlays installed by `hexl-utf8-mode'.")

(defvar-local hexl-utf8--update-timer nil
  "Idle timer for refreshing overlays after buffer changes.")

(defvar-local hexl-utf8--decoded nil
  "Global decoded vector for the current buffer (set by `hexl-utf8--apply').")

(defvar-local hexl-utf8--cursor-addr -1
  "Byte address of the last-known cursor position; -1 means unknown.")

(defvar-local hexl-utf8--last-point -1
  "Buffer position last seen by `hexl-utf8--update-cursor'.")

(defvar-local hexl-utf8--last-ovs nil
  "Overlays currently rendered with the cursor highlight.")

;;; ── UTF-8 parsing ─────────────────────────────────────────────────────────

(defun hexl-utf8--hex-digit (c)
  (cond ((and (>= c ?0) (<= c ?9)) (- c ?0))
        ((and (>= c ?a) (<= c ?f)) (+ 10 (- c ?a)))
        ((and (>= c ?A) (<= c ?F)) (+ 10 (- c ?A)))))

(defun hexl-utf8--utf8-leading-length (b)
  (cond ((< b #x80) 1)
        ((< b #xc2) 0)
        ((< b #xe0) 2)
        ((< b #xf0) 3)
        ((< b #xf5) 4)
        (t 0)))

(defun hexl-utf8--decode (bytes)
  "Classify each byte of unibyte string BYTES for UTF-8.
Returns a vector; element i is one of:
  (:char CHAR LEN)  -- leading byte of CHAR (LEN bytes total)
  :cont             -- continuation byte
  :invalid          -- not valid UTF-8"
  (let* ((n (length bytes))
         (out (make-vector n :invalid))
         (i 0))
    (while (< i n)
      (let* ((b (aref bytes i))
             (len (hexl-utf8--utf8-leading-length b)))
        (cond
         ((or (= len 0) (> (+ i len) n))
          (aset out i :invalid)
          (setq i (1+ i)))
         (t
          (let ((ok t) (j 1))
            (while (and ok (< j len))
              (let ((cb (aref bytes (+ i j))))
                (unless (and (>= cb #x80) (< cb #xc0))
                  (setq ok nil)))
              (setq j (1+ j)))
            (if (not ok)
                (progn (aset out i :invalid) (setq i (1+ i)))
              (let* ((sub (substring bytes i (+ i len)))
                     (dec (ignore-errors (decode-coding-string sub 'utf-8 t)))
                     (ch (and (stringp dec) (= (length dec) 1) (aref dec 0))))
                (cond
                 (ch
                  (aset out i (list :char ch len))
                  (dotimes (k (1- len)) (aset out (+ i 1 k) :cont))
                  (setq i (+ i len)))
                 (t
                  (aset out i :invalid)
                  (setq i (1+ i)))))))))))
    out))

;;; ── Buffer scanning ───────────────────────────────────────────────────────

(defun hexl-utf8--scan-buffer ()
  "Return a list of plists for each hexl line.
Each plist has :ascii-start :line-end :bytes :byte-offset."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((lines nil) (offset 0))
        (while (re-search-forward
                "^\\([0-9a-fA-F]+\\): \\([0-9a-fA-F ]+?\\)\\(  +\\)"
                nil t)
          (let* ((hex (match-string 2))
                 (ascii-start (match-end 3))
                 (line-end (line-end-position))
                 (hexlen (length hex))
                 (buf (make-string 64 0))
                 (count 0) (k 0))
            (while (< k hexlen)
              (let ((c1 (aref hex k))
                    (c2 (and (< (1+ k) hexlen) (aref hex (1+ k)))))
                (cond
                 ((eq c1 ?\s) (setq k (1+ k)))
                 ((and c2 (hexl-utf8--hex-digit c1) (hexl-utf8--hex-digit c2))
                  (when (>= count (length buf))
                    (setq buf (concat buf (make-string 64 0))))
                  (aset buf count
                        (+ (* 16 (hexl-utf8--hex-digit c1))
                           (hexl-utf8--hex-digit c2)))
                  (setq count (1+ count) k (+ k 2)))
                 (t (setq k (1+ k))))))
            (when (> count 0)
              (push (list :ascii-start ascii-start
                          :line-end   line-end
                          :bytes      (substring buf 0 count)
                          :byte-offset offset)
                    lines)
              (setq offset (+ offset count)))))
        (nreverse lines)))))

;;; ── Rendering ─────────────────────────────────────────────────────────────

(defun hexl-utf8--char-base-face (ch)
  (cond ((or (< ch #x20) (= ch #x7f)) 'hexl-utf8-placeholder-face)
        ((< ch #x80)                  'hexl-utf8-ascii-face)
        (t                            'hexl-utf8-multibyte-face)))

(defun hexl-utf8--char-display (ch)
  (if (or (< ch #x20) (= ch #x7f)) hexl-utf8-control-char ch))

(defun hexl-utf8--highlight-placeholder-p (byte hl-start hl-end)
  "Return non-nil if the placeholder at BYTE should be highlighted.
Placeholders for continuation bytes of a valid UTF-8 character are not the
displayed character itself, so they are highlighted only for one-byte ranges
such as invalid bytes."
  (and hl-start hl-end (= hl-start byte) (= hl-end (1+ byte))))

(defun hexl-utf8--render-line (line decoded &optional hl-start hl-end)
  "Build the propertized display string for LINE using DECODED.
HL-START..HL-END (exclusive) is a global byte range whose bytes should
be rendered with `hexl-utf8-cursor-face' merged over the base face.
For a multi-byte UTF-8 character, only the decoded character rendered at
its leading byte is highlighted; continuation placeholders stay unhighlighted."
  (let* ((offset (plist-get line :byte-offset))
         (bytes  (plist-get line :bytes))
         (n      (length bytes))
         (parts nil)
         (i 0))
    (cl-flet ((highlight-char-p
               (byte)
               (and hl-start hl-end (= hl-start byte) (< byte hl-end))))
      (while (< i n)
        (let ((entry (aref decoded (+ offset i))))
          (cond
           ((and (consp entry) (eq (car entry) :char))
            (let* ((ch   (nth 1 entry))
                   (len  (nth 2 entry))
                   (cs   (+ offset i))
                   (hlp  (highlight-char-p cs))
                   (base (hexl-utf8--char-base-face ch))
                   (face (if hlp
                             `(:inherit (hexl-utf8-cursor-face ,base))
                           base)))
              (push (propertize (string (hexl-utf8--char-display ch))
                                'face face)
                    parts)
              (setq i (+ i len))))
           ((eq entry :cont)
            (let* ((cs   (+ offset i))
                   (hlp  (hexl-utf8--highlight-placeholder-p
                          cs hl-start hl-end))
                   (face (if hlp
                             '(:inherit (hexl-utf8-cursor-face
                                         hexl-utf8-placeholder-face))
                           'hexl-utf8-placeholder-face)))
              (push (propertize (string hexl-utf8-continuation-char)
                                'face face)
                    parts))
            (setq i (1+ i)))
           (t
            (let* ((cs   (+ offset i))
                   (hlp  (hexl-utf8--highlight-placeholder-p
                          cs hl-start hl-end))
                   (face (if hlp
                             '(:inherit (hexl-utf8-cursor-face
                                         hexl-utf8-placeholder-face))
                           'hexl-utf8-placeholder-face)))
              (push (propertize (string hexl-utf8-invalid-char)
                                'face face)
                    parts))
            (setq i (1+ i)))))))
    (apply #'concat (nreverse parts))))

;;; ── Overlay management ────────────────────────────────────────────────────

(defun hexl-utf8--clear-overlays ()
  (mapc #'delete-overlay hexl-utf8--overlays)
  (setq hexl-utf8--overlays nil))

(defun hexl-utf8--apply ()
  "Rebuild all line overlays from scratch."
  (hexl-utf8--clear-overlays)
  (setq hexl-utf8--cursor-addr -1
        hexl-utf8--last-point  -1
        hexl-utf8--last-ovs    nil)
  (let ((lines (hexl-utf8--scan-buffer)))
    (when lines
      (let* ((all-bytes (apply #'concat
                               (mapcar (lambda (l) (plist-get l :bytes)) lines)))
             (decoded (hexl-utf8--decode all-bytes)))
        (setq hexl-utf8--decoded decoded)
        (dolist (line lines)
          (let* ((start (plist-get line :ascii-start))
                 (end   (plist-get line :line-end)))
            (when (and start end (< start end))
              (let ((ov (make-overlay start end nil t nil)))
                (overlay-put ov 'display    (hexl-utf8--render-line line decoded nil nil))
                (overlay-put ov 'hexl-utf8  t)
                (overlay-put ov 'evaporate  t)
                ;; Store line metadata for cursor tracking
                (overlay-put ov 'hexl-utf8-line line)
                (push ov hexl-utf8--overlays)))))
        ;; Now apply current cursor highlight
        (hexl-utf8--update-cursor)))))

(defun hexl-utf8--addr-from-point ()
  "Return the byte address corresponding to point, or nil.
Uses overlay metadata directly so it works regardless of `hexl-current-address'
quirks (e.g. when point is somewhere in the ASCII region or in the
gutter spaces between hex groups)."
  (let* ((p   (point))
         (bol (line-beginning-position))
         (eol (line-end-position))
         result)
    (dolist (ov hexl-utf8--overlays)
      (unless result
        (let* ((ov-start (overlay-start ov))
               (ov-bol   (when ov-start
                           (save-excursion
                             (goto-char ov-start)
                             (line-beginning-position)))))
          (when (and ov-bol (= ov-bol bol))
            (let* ((line   (overlay-get ov 'hexl-utf8-line))
                   (offset (plist-get line :byte-offset))
                   (count  (length (plist-get line :bytes)))
                   (ascii-start (plist-get line :ascii-start)))
              (cond
               ;; Point is inside the ASCII column: 1 char per byte
               ((and (>= p ascii-start) (<= p eol))
                (let ((idx (min (1- count) (- p ascii-start))))
                  (setq result (+ offset (max 0 idx)))))
               ;; Point is in the hex column.  Layout per byte-pair:
               ;;   "AABB " (4 hex digits + 1 space) = 5 columns, 2 bytes.
               ;; The address prefix "00003420: " is 10 columns wide
               ;; for an 8-digit address (8 + 2).  We locate the first
               ;; hex digit by scanning, to be tolerant of varying
               ;; address widths.
               ((>= p bol)
                (save-excursion
                  (goto-char bol)
                  (when (re-search-forward "^[0-9a-fA-F]+: " eol t)
                    (let* ((hex-start (point))
                           (rel (- p hex-start)))
                      (when (>= rel 0)
                        (let* ((group  (/ rel 5))
                               (within (mod rel 5))
                               (in-pair (cond ((<= within 1) 0)
                                              ((<= within 3) 1)
                                              (t 0)))
                               (idx (+ (* group 2) in-pair))
                               (idx (min (1- count) (max 0 idx))))
                          (setq result (+ offset idx))))))))))))))
    result))

(defun hexl-utf8--overlays-overlapping (start end)
  "Return overlays whose line covers any byte in [START, END)."
  (let (result)
    (dolist (ov hexl-utf8--overlays)
      (let* ((line   (overlay-get ov 'hexl-utf8-line))
             (offset (plist-get line :byte-offset))
             (count  (length (plist-get line :bytes))))
        (when (and (< offset end) (> (+ offset count) start))
          (push ov result))))
    result))

(defun hexl-utf8--char-range-at (addr)
  "Return cons (START . END) describing the byte range of the UTF-8 char
that contains ADDR, or nil.  If ADDR points at a continuation byte,
walks back to its leading byte."
  (let ((decoded hexl-utf8--decoded))
    (when (and decoded (>= addr 0) (< addr (length decoded)))
      (let ((entry (aref decoded addr)))
        (cond
         ((and (consp entry) (eq (car entry) :char))
          (cons addr (+ addr (nth 2 entry))))
         ((eq entry :cont)
          ;; Walk back to find the leading byte
          (let ((p (1- addr)))
            (while (and (>= p 0) (eq (aref decoded p) :cont))
              (setq p (1- p)))
            (if (and (>= p 0)
                     (consp (aref decoded p))
                     (eq (car (aref decoded p)) :char))
                (cons p (+ p (nth 2 (aref decoded p))))
              ;; Orphan continuation – treat as 1-byte invalid
              (cons addr (1+ addr)))))
         (t (cons addr (1+ addr))))))))

(defun hexl-utf8--redraw-overlay (ov hl-start hl-end)
  "Redraw OV; bytes in [HL-START, HL-END) are highlighted (nil = none)."
  (when (overlay-buffer ov)
    (let ((line (overlay-get ov 'hexl-utf8-line)))
      (overlay-put ov 'display
                   (hexl-utf8--render-line line hexl-utf8--decoded
                                           hl-start hl-end)))))

;;; ── Cursor tracking ───────────────────────────────────────────────────────

(defun hexl-utf8--update-cursor ()
  "Sync decoded-column highlight with the current cursor position."
  (when (and (derived-mode-p 'hexl-mode)
             hexl-utf8--decoded)
    (let ((p (point)))
      (unless (= p hexl-utf8--last-point)
        (setq hexl-utf8--last-point p)
        (let ((addr (or (hexl-utf8--addr-from-point)
                        (ignore-errors (hexl-current-address)))))
          (unless (and addr (= addr hexl-utf8--cursor-addr))
            (setq hexl-utf8--cursor-addr (or addr -1))
            ;; Restore previously highlighted overlays
            (dolist (ov hexl-utf8--last-ovs)
              (when (overlay-buffer ov)
                (hexl-utf8--redraw-overlay ov nil nil)))
            (setq hexl-utf8--last-ovs nil)
            ;; Highlight the character containing ADDR (may span 2 lines)
            (when addr
              (let* ((range (hexl-utf8--char-range-at addr)))
                (when range
                  (let* ((cs (car range))
                         (ce (cdr range))
                         (ovs (hexl-utf8--overlays-overlapping cs ce)))
                    (dolist (ov ovs)
                      (hexl-utf8--redraw-overlay ov cs ce))
                    (setq hexl-utf8--last-ovs ovs)))))))))))

;;; ── Buffer-change refresh ─────────────────────────────────────────────────

(defun hexl-utf8--schedule-update (&rest _)
  (when (timerp hexl-utf8--update-timer)
    (cancel-timer hexl-utf8--update-timer))
  (let ((buf (current-buffer)))
    (setq hexl-utf8--update-timer
          (run-with-idle-timer
           hexl-utf8-update-idle nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq hexl-utf8--update-timer nil)
                 (when (bound-and-true-p hexl-utf8-mode)
                   (hexl-utf8--apply)))))))))

;;; ── Minor mode ────────────────────────────────────────────────────────────

(defun hexl-utf8--turn-off ()
  (when (bound-and-true-p hexl-utf8-mode)
    (hexl-utf8-mode -1)))

(defun hexl-utf8-refresh ()
  "Force an immediate refresh of the UTF-8 decoded column."
  (interactive)
  (unless (bound-and-true-p hexl-utf8-mode)
    (user-error "hexl-utf8-mode is not enabled in this buffer"))
  (hexl-utf8--apply))

;;;###autoload
(define-minor-mode hexl-utf8-mode
  "Toggle a UTF-8 decoded text column inside `hexl-mode'.

The right-hand ASCII column is replaced via display overlays with the
bytes decoded as UTF-8.  The character (or placeholder) corresponding
to the byte under the cursor is highlighted with `hexl-utf8-cursor-face'."
  :lighter " hexlU8"
  (cond
   (hexl-utf8-mode
    (unless (derived-mode-p 'hexl-mode)
      (setq hexl-utf8-mode nil)
      (user-error "hexl-utf8-mode requires `hexl-mode'"))
    (hexl-utf8--apply)
    (add-hook 'after-change-functions #'hexl-utf8--schedule-update nil t)
    (add-hook 'post-command-hook      #'hexl-utf8--update-cursor    nil t)
    (add-hook 'hexl-mode-exit-hook    #'hexl-utf8--turn-off         nil t))
   (t
    (when (timerp hexl-utf8--update-timer)
      (cancel-timer hexl-utf8--update-timer)
      (setq hexl-utf8--update-timer nil))
    (hexl-utf8--clear-overlays)
    (setq hexl-utf8--decoded      nil
          hexl-utf8--cursor-addr  -1
          hexl-utf8--last-point   -1
          hexl-utf8--last-ovs     nil)
    (remove-hook 'after-change-functions #'hexl-utf8--schedule-update t)
    (remove-hook 'post-command-hook      #'hexl-utf8--update-cursor    t)
    (remove-hook 'hexl-mode-exit-hook    #'hexl-utf8--turn-off         t))))

(provide 'hexl-utf8)
;;; hexl-utf8.el ends here
