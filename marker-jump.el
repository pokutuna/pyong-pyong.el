;;; marker-jump.el
;;

(defvar mkj:marker-stack ())
(defvar mkj:marker-alist ())
(defvar mkj:max-buffer-name-length 0)
(defvar mkj:max-line-number-length 0)

(defvar mkj:marker-star-face '(face anything-M-x-key-face))
(defvar mkj:buffer-name-face font-lock-constant-face)
(defvar mkj:line-number-face font-lock-comment-delimiter-face)

(defvar mkj:candidate-number-limit 30)

(defun mkj:push-point-marker ()
  ""
  (interactive)
  (push (point-marker) mkj:marker-alist)
  (push (point-marker) mkj:marker-stack)
  (message "[marker-jump] current position pushed."))


(defun mkj:pop-point-marker ()
  ""
  (interactive)
  (let ((marker (pop mkj:marker-stack)))
    (if marker
        (mkj:goto-marker marker)
      (message "[marker-jump] no markers"))
    ))



(defun mkj:marker-line (marker)
  ""
  (with-current-buffer (marker-buffer marker)
      (save-excursion
        (letf ((pos (marker-position marker)) (line) (pos-in-line))
          (goto-char pos)
          (setf line (thing-at-point 'line))
          (when (stringp line)
               (setf line (replace-regexp-in-string "\n" " " line))
               (setq pos-in-line (- pos (point-at-bol)))
               (setf line (format "%s%s%s"
                                  (substring line 0 pos-in-line)
                                  (propertize "*" 'face mkj:marker-star-face)
                                  (substring line (1+ pos-in-line))))
               (list (buffer-name (marker-buffer marker)) (line-number-at-pos pos) line)
               )))))


(defun mkj:goto-marker (marker)
  (let ((buf (marker-buffer marker)) (pos (marker-position marker)))
    (switch-to-buffer buf)
    (set-buffer buf)
    (goto-char pos)))


(defvar anything-c-source-mkj
  '((name . "MarkerJump")
    (candidates . mkj:marker-alist)
    (cons candidate-number-limit mkj:candidate-number-limit)
    (candidate-transformer . anything-c-mkj-candidate-transformer)
    (action ("Jump to" . mkj:goto-marker))
    ))



(defun anything-c-mkj-candidate-transformer (candidates)
  (setq mkj:max-buffer-name-length 0 mkj:max-line-number-length 0)
  (let ((line-info (mapcar '(lambda (marker) (append (mkj:marker-line marker) (list marker))) candidates)))
    (loop for l in line-info do
          (when l
            (let ((buffer-name-length (length (nth 0 l)))
                  (line-number-length (length (number-to-string (nth 1 l)))))
              (when (> buffer-name-length mkj:max-buffer-name-length)
                (setq mkj:max-buffer-name-length buffer-name-length))
              (when (> line-number-length mkj:max-line-number-length)
                (setq mkj:max-line-number-length line-number-length))
              )))

    (mapcar '(lambda (l)
               (cons
                (format (format "%%-%ds %%%ds %%s"
                                mkj:max-buffer-name-length
                                mkj:max-line-number-length)
                        (propertize (nth 0 l) 'face mkj:buffer-name-face)
                        (propertize (number-to-string (nth 1 l)) 'face mkj:line-number-face)
                        (nth 2 l))
                (nth 3 l)))
            line-info)
  ))


(defun anything-c-mkj-candidates ()
  (mapcar 'mkj:marker-line mkj:marker-alist))


(defun mkj:default-binding ()
  (global-set-key (kbd "C-.") 'mkj:push-point-marker)
  (global-set-key (kbd "C-,") 'mkj:pop-point-marker)
  )

(provide 'marker-jump)
