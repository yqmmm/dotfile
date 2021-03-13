;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Qianmian Yu"
      user-mail-address "im.qianmian.yu@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Consolas" :size 15)
      doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/notes/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Make arrow key greate again
(map! :nvm "<left>" 'evil-first-non-blank
      :nvm "<right>" 'evil-end-of-line
      :nvm "<up>" 'beginning-of-defun
      :nvm "<down>" 'end-of-defun)

(map! :nvm "s-{" '+workspace/switch-left
      :nvm "s-}" '+workspace/switch-right)

;; Make window movement great again
(map! :g "s-j" 'evil-window-down
      :g "s-k" 'evil-window-up
      :g "M-s-h" 'evil-window-left ;; DANGER ALERT: command-h is "hide window" macos which I rarely use. So i map it to "M-s-h" in BetterTouchTool
      :g "s-l" 'evil-window-right
      )

;; smart-input-source https://github.com/laishulu/emacs-smart-input-source
(use-package sis
  ;; :hook
  ;; enable the /follow context/ and /inline region/ mode for specific buffers
  ;; (((text-mode prog-mode) . sis-context-mode)
  ;;  ((text-mode prog-mode) . sis-inline-mode))

  :config
  ;; For MacOS
  (sis-ism-lazyman-config
   "com.apple.keylayout.US"
   "com.sogou.inputmethod.sogou.pinyin")

  ;; enable the /cursor color/ mode
  ;; (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)
  )


(defun zz/org-download-paste-clipboard (&optional use-default-filename)
  (interactive "P")
  (require 'org-download)
  (let ((file
         (if (not use-default-filename)
             (read-string (format "Filename [%s]: "
                                  org-download-screenshot-basename)
                          nil nil org-download-screenshot-basename)
           nil)))
    (org-download-clipboard file)))

(after! org
  (setq org-agenda-files '("~/notes" "~/notes/daily"))
  (setq org-log-done 'time)
  (setq org-image-actual-width '(500))
  (setq org-download-method 'directory)
  (setq org-download-image-dir "~/notes/img/")
  (setq org-download-heading-lvl nil)
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")
  (setq org-download-screenshot-method "/usr/local/bin/pngpaste %s")
  (map! :map org-mode-map
        "C-c l a y" #'zz/org-download-paste-clipboard
        "C-M-y" #'zz/org-download-paste-clipboard))

;; This is a Emacs mac port specific configuration, see 'Mac Fullscreen' for help
;; TODO This does not work properly
;; (set-frame-parameter nil 'fullscreen 'fullscreen)

(global-wakatime-mode)

;; Disable pixel-by-pixel scrolling, since it's extremely choppy.
(setq mac-mouse-wheel-smooth-scroll nil)

;; ----------
;; ivy-bibtex
;; ----------
(defun bibtex-completion-format-citation-org-ref (keys)
  "Format ebib references for keys in KEYS."
  (s-join ", "
          (--map (format "cite:%s" it) keys)))

; TODO Change this to bibtex-completion?
(use-package! ivy-bibtex
  :defer t
  :config
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-plus)))

(setq bibtex-completion-bibliography
      '("~/notes/zotero.bib"))
(setq bibtex-completion-pdf-field "file")
(setq bibtex-completion-pdf-open-function
  (lambda (fpath)
    (call-process "open" nil 0 nil fpath)))
(setq bibtex-completion-format-citation-functions
      '((org-mode . bibtex-completion-format-citation-org-ref)))
(setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
(map! :leader
      :n
      "\"" #'ivy-bibtex)


;; ----------
;; org-roam
;; ----------
(setq org-roam-directory "~/notes")
; Open org-roam buffer (backlinks) in the bottom with a vertical screen.
(setq org-roam-buffer-position
      (lambda ()
        (if (> (frame-native-height) (frame-native-width))
            'bottom 'right)))

;; ----------
;; org-ref
;; ----------
(use-package org-ref
  :after org
    :preface
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-default-bibliography "~/notes/zotero.bib"
         ;; org-ref-bibliography-notes "/home/haozeke/Git/Gitlab/Mine/Notes/bibnotes.org"
         ;; org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         ;; org-ref-notes-directory "/home/haozeke/Git/Gitlab/Mine/Notes/"
         ;; org-ref-notes-function 'orb-edit-notes
    )
    :config
    (setq org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex)
  )
;; ---------------
;; org-roam-bibtex
;; ---------------
(use-package! org-roam-bibtex
  :load-path "~/notes/zotero.bib" ;Modify with your own path
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
         (("C-c n a" . orb-note-actions))))

; osx-like jumping
(map! "s-[" 'better-jumper-jump-backward)
(map! "s-]" 'better-jumper-jump-forward)

; Need this for pitched font to work, Search "doom emacs pitched font"
;; (use-package! mixed-pitch
;;   :hook (org-mode . mixed-pitch-mode)
;;   :config
;;   (setq mixed-pitch-set-heigth t))
;; (add-hook 'org-mode-hook 'variable-pitch-mode)

(defun markdown-convert-buffer-to-org ()
  "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           (format "pandoc -f markdown -t org -o '%s'"
                           ;; (format "pandoc -f markdown -t org -o '%s' | | sed -E "/^[[:space:]]+:/ d""
                                   (concat (file-name-sans-extension (buffer-file-name)) ".org"))))
