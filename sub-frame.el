;;; sub-frame.el ---

;; Copyright (C) 2012 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
(defconst sub-frame-version "0.1")
;; Keywords: convenience, sub, frame

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the

;; GNU General Public License for more details.

;;; Commentary:

;; Installation:

;;; Code:

(eval-when-compile (require 'cl))

(defgroup sub-frame nil
  ""
  :group 'convenience
  :prefix "sf:")

;; sub-frame position and size config variables.
(defcustom sf:frame-left 1920
  ""
  :type 'integer
  :group 'sub-frame)

(defcustom sf:frame-top 0
  ""
  :type 'integer
  :group 'sub-frame)

(defcustom sf:frame-width 80
  ""
  :type 'integer
  :group 'sub-frame)

(defcustom sf:frame-height 28
  ""
  :type 'integer
  :group 'sub-frame)

(defcustom sf:frame-parameters '((name . "SUB-FRAME")
                                 (alpha . 95)
                                 (vertical-scroll-bars . right))
  ""
  :type 'alist
  :group 'sub-frame)

(defun sf:search-frame-by-name (name)
  "search frame by name."
  (let* ((frame-names-alist (make-frame-names-alist))
         (frame (cdr (assoc name frame-names-alist))))
    frame))

(defun sf:select-sub-frame (frame)
  "select sub frame."
  (select-frame frame)
  (make-frame-visible frame)
  (raise-frame frame))

(defun sf:get-sub-frame ()  
  "get log frame. if not exist, create it."
  (let* ((frame (sf:search-frame-by-name (cdr (assoc 'name sf:frame-parameters))))
         (frame (or frame (make-frame sf:frame-parameters))))
    (set-frame-position frame sf:frame-left sf:frame-top)
    (set-frame-size frame sf:frame-width sf:frame-height)
    frame))

(defmacro sf:sub-frame-op (&rest body)
  "for define sub-frame operations.
   use gensym make this Warning: Function `gensym' from cl package called at runtime
"
  ;; (let ((prev-frame (gensym)))
  `(let ((sf:prev-frame (window-frame (get-buffer-window))))
     (sf:select-sub-frame (sf:get-sub-frame))
     ,@body
     (delete-other-windows)
     (sf:select-sub-frame sf:prev-frame)))

(defun sf:async-shell-command (&optional cmd &optional out-buffer &optional error-buffer)
  (interactive)
  "run async-shell-command, output to sub-frame."
  (let ((cmd (or cmd (read-string "shell command?:")))
        (out-buffer (or out-buffer "*Async Shell Command*")))
    (sf:sub-frame-op
     (async-shell-command cmd out-buffer error-buffer)
     (switch-to-buffer out-buffer))))

(defun sf:buffer (&optional buffer)
  (interactive)
  "switch buffer."
  (setq buffer (or buffer (read-buffer "buffer:")))
  (sf:sub-frame-op
   (switch-to-buffer buffer)))

(defun sf:current-buffer ()
  (interactive)
  ""
  (let ((buffer (current-buffer)))
    (sf:sub-frame-op
     (switch-to-buffer buffer))))

(defun sf:switch-to-sub-frame-buffer ()
  (interactive)
  ""
  (let ((buffer nil))
    (sf:sub-frame-op
     (setq buffer (current-buffer)))
    (switch-to-buffer buffer)))

(defun sf:jump-to-buffer-top (&optional buffer)
  (interactive)
  "sub frame buffer cursor jump to buffer top."
  (sf:sub-frame-op
   (goto-char (point-min))))

(defun sf:jump-to-buffer-bottom (&optional buffer)
  (interactive)
  "sub frame buffer cursor jump to buffer bottom."
  (sf:sub-frame-op
   (goto-char (point-max))))

(defun sf:scroll-up (&optional buffer)
  (interactive)
  "sub frame buffer cursor scroll up."
  (sf:sub-frame-op
   (scroll-up)))

(defun sf:scroll-down (&optional buffer)
  (interactive)
  "sub frame buffer cursor scroll down."
  (sf:sub-frame-op
   (scroll-down)))

(defun sf:close ()
  (interactive)
  "close sub-frame"
  (let ((subframe (sf:get-sub-frame)))
    (when subframe
      (delete-frame subframe))))

(defun sf:toggle-hidden ()
  (interactive)
  "hidden/uhidden sub-frame."
  (let ((subframe (sf:get-sub-frame)))
    (when subframe
      (if (frame-visible-p subframe)
          (make-frame-invisible subframe)
        (make-frame-visible subframe)))))

(provide 'sub-frame)
