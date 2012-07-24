(setq default-directory "/d/wo/erl/")
(global-set-key (kbd "C-SPC") 'nil)
;; share clipboard with outside programs
(setq x-select-enable-clipboard t)

(desktop-save-mode 1)


(tool-bar-mode nil)
;;(menu-bar-mode nil)
(scroll-bar-mode nil)
(setq make-backup-files nil)
;; 设置另外一些颜色：语法高亮显示的背景和主题，区域选择的背景和主题，二次选择的背景和选择
(set-face-foreground 'highlight "white")
(set-face-background 'highlight "blue")
(set-face-foreground 'region "cyan")
(set-face-background 'region "blue")
(set-face-foreground 'secondary-selection "skyblue")
(set-face-background 'secondary-selection "darkblue")
(set-foreground-color "grey")
(set-background-color "black")
(set-cursor-color "gold1")
(set-mouse-color "gold1")
;;(set-scroll-bar-mode nil)
;设定光标为短线
(setq-default cursor-type 'bar)
;设置标题
(setq frame-title-format
        '("  dp - Emacs   -   [ " (buffer-file-name "%f \]"
                (dired-directory dired-directory "%b \]"))))
;设置启动大小
(set-frame-height (selected-frame) 65)
(set-frame-width (selected-frame) 170)

(setq inhibit-startup-message t);关闭起动时LOGO
(setq visible-bell t);关闭出错时的提示声
(setq default-major-mode 'erlang-mode);一打开就起用 text 模式
(global-font-lock-mode t);语法高亮
(auto-image-file-mode t);打开图片显示功能
(fset 'yes-or-no-p 'y-or-n-p);以 y/n代表 yes/no
(column-number-mode t);显示列号
(show-paren-mode t);显示括号匹配
(setq mouse-yank-at-point t);支持中键粘贴
(transient-mark-mode t);允许临时设置标记
(setq x-select-enable-clipboard t);支持emacs和外部程序的粘贴
(setq auto-save-default nil);关闭备份文件#xxx#

;;-----kill ring 长度
(setq kill-ring-max 200)
         
(require 'linum)
(global-linum-mode 1)


;;--------------------------快捷键定义------------------------
(global-set-key [(f12)] 'loop-alpha)  ;;玻璃
;;F1列模式
(global-set-key [f1] 'cua-mode)
;;恢复，常用键
(global-set-key [f2] 'undo)
;;F4,kill键，习惯设置，关闭当前buffer
(global-set-key [f4] 'kill-this-buffer)
;;定义查找快捷键
(global-set-key [f5] 'replace-regexp) ;;支持正则表达式
(global-set-key [f6] 'replace-string)

(global-set-key [f8] 'erlang-mode)
(global-set-key [f7] 'ecb-activate) ;;定义F7键为激活ecb
(global-set-key [C-f7] 'ecb-deactivate)
(global-set-key [f11] 'delete-other-windows) ;;设置F11为删除其它窗口
 (global-set-key (kbd "C-c z") (quote shell))

;;-------------------------------全选---------------------


(defun select-all ()
  "Select the whole buffer."
  (interactive)
  (goto-char (point-min))
  ;; Mark current position and push it into the mark ring.
  (push-mark-command nil nil)
  (goto-char (point-max))
  (message "ok."))

(provide 'select-all)

(autoload 'select-all "select-all"
  "Select the whole buffer." t nil)

;; user defined keys

(global-set-key "\C-x\C-a" 'select-all)
;;-------------------glass style------------------


(setq alpha-list '((85 55) (100 100)))  
 
(defun loop-alpha ()  
  (interactive)  
  (let ((h (car alpha-list)))                  
    ((lambda (a ab)  
       (set-frame-parameter (selected-frame) 'alpha (list a ab))  
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))  
       ) (car h) (car (cdr h)))  
    (setq alpha-list (cdr (append alpha-list (list h))))  
    ))  


;;--------------------------erlang----------------------------------------------------------------------------------------------------------
(setq load-path (cons "/opt/erl/lib/erlang/lib/tools-2.6.7/emacs" load-path))
(setq erlang-root-dir "/opt/erl")
(setq exec-path (cons "/opt/erl/bin" exec-path))
;;(setq erlang-man-root-dir "/opt/erl/man")
(require 'erlang-start)

(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))
(add-to-list 'load-path "~/.emacs.d/erlware-mode")

;;----------------------distel-------------------

(let ((distel-dir "~/.emacs.d/distel/elisp"))
    (unless (member distel-dir load-path)
        ;; Add distel-dir to the end of load-path
        (setq load-path (append load-path (list distel-dir)))))

(require 'distel)
(distel-setup)

;; Some Erlang customizations

(add-hook 'erlang-mode-hook
  (lambda ()

  ;; when starting an Erlang shell in Emacs, default in the node name

    (setq inferior-erlang-machine-options '("-sname" "emacs"))
    
    ;; add Erlang functions to an imenu menu

    (imenu-add-to-menubar "imenu")))

;; prevent annoying hang-on-compile
(defvar inferior-erlang-prompt-timeout t)

;; tell distel to default to that node
(setq erl-nodename-cache
      (make-symbol
       (concat
        "emacs@"
        ;; Mac OS X uses "name.local" instead of "name", this should work
        ;; pretty much anywhere without having to muck with NetInfo
        ;; ... but I only tested it on Mac OS X.
        (car (split-string (shell-command-to-string "hostname"))))))


;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
  (lambda ()
;; add some Distel bindings to the Erlang shell
        (dolist (spec distel-shell-keys)
        (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;;------------------------esense--------------------

(setq load-path (cons "~/.emacs.d/esense-1.12" load-path))
(require 'esense-start)
(setq esense-indexer-program "~/.emacs.d/esense-1.12/esense.sh")



;;-------------------flymake-------------------

(defun flymake-erlang-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                       (file-name-directory buffer-file-name))))
    (list "~/.emacs.d/emakefly" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))
;;(add-hook 'find-file-hook 'flymake-find-file-hook)



(defun ebm-find-rebar-top-recr (dirname)
      (let* ((project-dir (locate-dominating-file dirname "rebar.config")))
        (if project-dir
            (let* ((parent-dir (file-name-directory (directory-file-name project-dir)))
                   (top-project-dir (if (and parent-dir (not (string= parent-dir "/")))
                                       (ebm-find-rebar-top-recr parent-dir)
                                      nil)))
              (if top-project-dir
                  top-project-dir
                project-dir))
              project-dir)))

    (defun ebm-find-rebar-top ()
      (interactive)
      (let* ((dirname (file-name-directory (buffer-file-name)))
             (project-dir (ebm-find-rebar-top-recr dirname)))
        (if project-dir
            project-dir
          (erlang-flymake-get-app-dir))))

     (defun ebm-directory-dirs (dir name)
        "Find all directories in DIR."
        (unless (file-directory-p dir)
          (error "Not a directory `%s'" dir))
        (let ((dir (directory-file-name dir))
              (dirs '())
              (files (directory-files dir nil nil t)))
            (dolist (file files)
              (unless (member file '("." ".."))
                (let ((absolute-path (expand-file-name (concat dir "/" file))))
                  (when (file-directory-p absolute-path)
                    (if (string= file name)
                        (setq dirs (append (cons absolute-path
                                                 (ebm-directory-dirs absolute-path name))
                                           dirs))
                        (setq dirs (append
                                    (ebm-directory-dirs absolute-path name)
                                    dirs)))))))
              dirs))

    (defun ebm-get-deps-code-path-dirs ()
        (ebm-directory-dirs (ebm-find-rebar-top) "ebin"))

    (defun ebm-get-deps-include-dirs ()
       (ebm-directory-dirs (ebm-find-rebar-top) "include"))

    (fset 'erlang-flymake-get-code-path-dirs 'ebm-get-deps-code-path-dirs)
    (fset 'erlang-flymake-get-include-dirs-function 'ebm-get-deps-include-dirs)

;;----------------ecb--------------
;; cedet
(load-file "~/.emacs.d/cedet-1.0/common/cedet.el")
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
(global-srecode-minor-mode 1)            ; Enable template insertion menu


(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
(require 'ecb)
;;(require 'ecb-autoloads)
;;(setq ecb-auto-activate t)
(setq ecb-tip-of-the-day nil)

;; tabbar
(add-to-list 'load-path "~/.emacs.d/tabbar")
(require 'tabbar)
(tabbar-mode t)
(global-set-key [(meta j)] 'tabbar-backward)
(global-set-key [(meta k)] 'tabbar-forward)

;;--------------------- tabbar----------
(set-face-attribute 'tabbar-default-face nil
                    :family "DejaVu Sans Mono"
                    :background "gray80"
                    :foreground "gray30"
                    :height 1.0
                    )
(set-face-attribute 'tabbar-button-face nil
                    :inherit 'tabbar-default
                    :box '(:line-width 1 :color "yellow70")
                    )
(set-face-attribute 'tabbar-selected-face nil
                    :inherit 'tabbar-default
                    :foreground "DarkGreen"
                    :background "LightGoldenrod"
                    :box '(:line-width 2 :color "Darkgoldenrod")
                    :overline "black"
                    :underline "black"
                    :weight 'bold
                    )
(set-face-attribute 'tabbar-unselected-face nil
                    :inherit 'tabbar-default
                    :box '(:line-width 2 :color "#00B2BF")
                    )
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
;;wrangler

;; --------- wrangler -----------------------
(add-to-list 'load-path "/usr/local/share/wrangler/elisp")
(require 'wrangler)

;;---------------------YASnippet--------------------

(add-to-list 'load-path
             "~/.emacs.d")
(require 'yasnippet-bundle)


;;----------------------auto-complete-------------


;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;(require 'auto-complete-config)
;;(ac-config-default)
(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)


