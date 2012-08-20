;;; marker-jump.el
;;

(defvar mkj:marker-alist ())

(defun mkj:push-point-marker ()
  ""
  (interactive)
  (push (point-marker) mkj:marker-alist)
  (message "[marker-jump] current position pushed."))


(defun mkj:pop-point-marker ()
  ""
  (interactive)
  (let ((marker (pop mkj:marker-alist)))
    (if marker
        (mkj:goto-marker marker)
      (message "[marker-jump] no markers"))
    ))


(defun mkj:marker-line (marker)
  ""
  (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (thing-at-point 'line))))


(defun mkj:goto-marker (marker)
  (let ((buf (marker-buffer marker)) (pos (marker-position marker)))
    (switch-to-buffer buf)
    (set-buffer buf)
    (goto-char pos)))


(defvar anything-c-source-mkj
  '((name . "MarkerJump")
    (candidates . mkj:marker-alist)
    (candidate-transformer . anything-c-mkj-candidate-transformer)
    (action ("Jump to" . mkj:goto-marker))
    ))


(defun anything-c-mkj-candidate-transformer (candidates)
  (mapcar '(lambda (marker)
            (cons (mkj:marker-line marker) marker))
          candidates))


(defun anything-c-mkj-candidates ()
  (mapcar 'mkj:marker-line mkj:marker-alist))



(mapcar (lambda (num)
           (cons (+ num 1) num))
        '(1 2 3))


(provide 'marker-jump)
