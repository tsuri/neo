;;; early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(advice-add #'x-apply-session-resources :override #'ignore)

(setq frame-inhibit-implied-resize t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
