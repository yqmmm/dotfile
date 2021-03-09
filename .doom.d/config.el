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

(map! :nvm "<left>" 'evil-first-non-blank
      :nvm "<right>" 'evil-end-of-line)


(setq org-agenda-files '("~/notes/" "~/notes/log/"))

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

(autoload 'ivy-bibtex "ivy-bibtex" "" t)
;; ivy-bibtex requires ivy's `ivy--regex-ignore-order` regex builder, which
;; ignores the order of regexp tokens when searching for matching candidates.
;; Add something like this to your init file:
;; (setq ivy-re-builders-alist
;;       '((ivy-bibtex . ivy--regex-ignore-order)
;;         (t . ivy--regex-plus)))
(setq bibtex-completion-bibliography
      '("~/notes/zotero.bib"))
(setq bibtex-completion-pdf-field "file")
(setq bibtex-completion-pdf-open-function
  (lambda (fpath)
    (call-process "open" nil 0 nil fpath)))
(setq bibtex-completion-format-citation-functions
      '((org-mode . bibtex-completion-format-citation-org-title-link-to-PDF)))
(setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
(map! :leader
      :n
      "\"" #'ivy-bibtex)

; osx-like jumping
(map! "s-[" 'better-jumper-jump-backward)
(map! "s-]" 'better-jumper-jump-forward)

; org-roam
(setq org-roam-directory "~/notes")
;; (add-hook 'after-init-hook 'org-roam-mode)

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
                                   (concat (file-name-sans-extension (buffer-file-name)) ".org"))))
