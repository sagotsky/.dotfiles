(add-to-list 'load-path "~/.emacs.d/")

; load 256 colors
    ;;; This is for GNU Emacs 22
    (defun terminal-init-screen ()
      "Terminal initialization function for screen."
      ;; Use the xterm color initialization code.
      (load "term/xterm")
      (xterm-register-default-colors)
      (tty-set-up-initial-frame-faces))

    ;;; This is for GNU Emacs 21
    (if (= 21 emacs-major-version)
        (load "xterm-256color"))

;(keyboard-translate ?\C-h ?\C-?)

; Highlight current line.  Change color if urxvt color vars are set
(require 'highlight-current-line)
(highlight-current-line-on t)

(set-face-background 'highlight-current-line-face "#777")  

(setq hl-color "#606066")

(setq color (getenv "COLORFGBG"))
(print color)

(cond
 ;black
 ((string= color "15;default")   (setq hl-color "#303033") )
 ((string= color "15;default;0") (setq hl-color "#303033") )
 ;white
 ((string= color "0;default;15") (setq hl-color "#ffc") )
 ((string= color "0;15")         (setq hl-color "#ffc") )
 ((string= color "0;default")    (setq hl-color "#ffc") ) 
)

(set-face-background 'highlight-current-line-face hl-color)  

(global-font-lock-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t)
(setq default-major-mode 'text-mode)
(setq initial-major-mode 'text-mode)
(setq-default save-place t)

(c-set-offset 'case-label '+) ; fixes switch/case indenting in cc-mode
(show-paren-mode 1)

;; Filetype Modes

(autoload 'javascript-mode "javascript" "Javascript Mode" t)
(autoload 'php-mode "php-mode" "PHP Mode" t)
(autoload 'css-mode "css-mode" "CSS Mode" t)
(autoload 'docbook-xml-mode "docbook-xml-mode" "Docbook XML Mode" t)
(setq auto-mode-alist (append (list
			       '("\\.alias$" . sh-mode)
			       '("\\.conf$" . sh-mode)
			       '("\\.css$" . css-mode)
			       '("\\.docbook$" . docbook-xml-mode)
			       '("\\.inc$" . php-mode)
			       '("\\.html$" . html-mode)
			       '("\\.htm$" . html-mode)
			       '("\\.hs$" . haskell-mode)
			       '("\\.js$" . javascript-mode)
			       '("\\.json$" . javascript-mode)
			       '("\\.local$" . sh-mode)
			       '("\\.module$" . php-mode)
			       '("\\.php$" . php-mode)
			       '("\\.py$" . python-mode)
			       '("\\rc$" . sh-mode)
			       '("\\.sh$" . sh-mode)
			       '("\\.svg$" . javascript-mode)
			       '("\\.xml$" . html-mode)
			       '("\\.xul$" . html-mode)
) auto-mode-alist))

(add-hook 'c-mode-common-hook
	  (lambda() 
	    (which-function-mode t)))

(custom-set-variables
 '(load-home-init-file t t))
(custom-set-faces)
