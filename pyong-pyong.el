;;; pyong-pyong.el

;;

(defvar pyong:marker-stack ())
(defvar pyong:marker-cache ())
(defvar pyong:last-pyonged-marker nil)

(defvar pyong:max-buffer-name-length 0)
(defvar pyong:max-line-number-length 0)

(defvar pyong:marker-star-face 'anything-M-x-key-face)
(defvar pyong:buffer-name-face 'font-lock-constant-face)
(defvar pyong:line-number-face 'font-lock-comment-delimiter-face)

(defvar pyong:candidate-number-limit 30)

(defun pyong:push-point-marker ()
  (interactive)
  (push (point-marker) pyong:marker-cache)
  (push (point-marker) pyong:marker-stack)
  (message "[pyong] current position pushed."))


(defun pyong:pop-point-marker ()
  (interactive)
  (let ((marker (pop pyong:marker-stack)))
    (if marker
        (pyong:goto-marker marker)
      (message "[pyong] no markers"))))


(defvar anything-c-source-pyong-last-jumped
  '((name . "Pyong Last Jumped Position")
    (cons candidates (list pyong:marker-cache))
    (type . pyong-marker)))


(defvar anything-c-source-pyong
  '((name . "PyongJump")
    (candidates . pyong:marker-cache)
    (type . pyong-marker)))


(define-anything-type-attribute 'pyong-marker
  '((candidate-transformer . anything-c-pyong-candidate-transformer)
    (cons candidate-number-limit pyong:candidate-number-limit)
    (action ("jump" . pyong:goto-marker)
            ("jump and delete" . (lambda (marker)
                                   (pyong:goto-marker marker)
                                   (delete marker pyong:marker-cache)))
            ("delete"  . (lambda (marker) (delete marker pyong:marker-cache)))))
  "marker for pyong-pyong.el")


(defun anything-c-pyong-candidate-transformer (candidates)
  (pyong:init-max-sizes)
  (let ((line-infos
         (mapcar '(lambda (marker) (pyong:marker-to-display-info marker)) candidates)))
    (dolist (l line-infos) (pyong:update-max-sizes-if-bigger l))
    (mapcar '(lambda (l) (cons (pyong:format-display-info l) (nth 3 l))) line-infos)))


(defun pyong:goto-marker (marker)
  (setq pyong:last-pyonged-marker (point-marker))
  (let ((buf (marker-buffer marker)) (pos (marker-position marker)))
    (switch-to-buffer buf)
    (set-buffer buf)
    (goto-char pos)))


(defun pyong:marker-to-display-info (marker)
  (with-current-buffer (marker-buffer marker)
      (save-excursion
        (letf ((pos (marker-position marker)) (line))
          (goto-char pos)
          (setf line (or (thing-at-point 'line) " "))
          (when (stringp line)
               (setf line (replace-regexp-in-string "\n" " " line))
               (setf line (pyong:put-star-in-string line (- pos (point-at-bol))))
               (list (buffer-name (marker-buffer marker))
                     (number-to-string (line-number-at-pos pos))
                     line
                     marker)
               )))))


(defun pyong:put-star-in-string (string pos)
  (format "%s%s%s"
          (substring string 0 pos)
          (propertize "*" 'face pyong:marker-star-face)
          (substring string (1+ pos))))


(defun pyong:init-max-sizes ()
  (setq pyong:max-buffer-name-length 0
        pyong:max-line-number-length 0))


(defun pyong:update-max-sizes-if-bigger (display-info)
  (when display-info
    (let ((buffer-name-length (length (nth 0 l)))
          (line-number-length (length (nth 1 l))))
      (when (> buffer-name-length pyong:max-buffer-name-length)
        (setq pyong:max-buffer-name-length buffer-name-length))
      (when (> line-number-length pyong:max-line-number-length)
        (setq pyong:max-line-number-length line-number-length))
      )))


(defun pyong:format-display-info (display-info)
  (let ((line-format-string (format "%%-%ds %%%ds %%s"
                                    pyong:max-buffer-name-length
                                    pyong:max-line-number-length)))
    (format line-format-string
            (propertize (nth 0 l) 'face pyong:buffer-name-face)
            (propertize (nth 1 l) 'face pyong:line-number-face)
            (nth 2 l))))


(defun pyong:default-binding ()
  (global-set-key (kbd "C-.") 'pyong:push-point-marker)
  (global-set-key (kbd "C-,") 'pyong:pop-point-marker)
  )

(provide 'marker-jump)
(pyong:default-binding)
