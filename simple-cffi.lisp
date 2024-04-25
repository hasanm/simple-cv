(require :cffi)
(require :cffi-libffi)
(require :cl-ppcre)

;; (push #P"/home/p-hasan/src/lisp/lisp-magick-wand/" asdf:*central-registry*)
;; (require "lisp-magick-wand")


(defparameter *input* "/data/aoe_images/my.png")
(defparameter *output* "/data/aoe_images/out.png")
(defparameter *lines* "/data/aoe_images/lines.txt")
(defparameter *rhos* "/data/aoe_images/rhos.txt")

(cffi:define-foreign-library libsimple
  (t (:default "libsimple")))

(setq cffi:*foreign-library-directories* (list #P"/home/p-hasan/work/qt/simple-cv/build/"))

(cffi:use-foreign-library libsimple)
;; (cffi:load-foreign-library 'libsimple)

(cffi:defcenum imread-modes
  :IMREAD_GRAYSCALE
  :IMREAD_COLOR)

(cffi:defcfun "easy_init" :pointer)

(defclass my-container ()
  ((pointer :initform (easy-init))
   (c-strings :initform '())
   ))

(cffi:define-foreign-type my-container-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser my-container))

(defmethod cffi:translate-to-foreign (handle (type my-container-type))
  (slot-value handle 'pointer))


(cffi:defcfun "adaptive_threshold" :int)

(cffi:defcfun "experiment" :int)

(cffi:defcfun "copy_image" :int
  (filename :pointer))

(cffi:defcfun "paste_image" :int
  (filename :pointer))

(cffi:defcfun "cut_image" :int
  (x :int)
  (y :int)
  (p :int)
  (q :int))

(cffi:defcfun "load_image" :int
  (handle my-container)
  (filename :pointer)
  (read-mode :int))


(cffi:defcfun "new_lines" :int
  (handle my-container))

(cffi:defcfun "push_line" :int
  (handle my-container)
  (x1 :int)
  (x2 :int)
  (y1 :int)
  (y2 :int))

(cffi:defcfun "draw_lines" :int
  (handle my-container))

(cffi:defcfun "draw_rectangle" :int
  (x :int)
  (y :int)
  (p :int)
  (q :int))


(cffi:defcfun "adaptive_threshold" :int)
(cffi:defcfun "my_merge" :int)
(cffi:defcfun "make_gray" :int)
(cffi:defcfun "grab_cut" :int)

(cffi:defcfun "canny_edge" :int
  (edge-thresh :int))

(cffi:defcfun "hough_lines" :int
  (edge-thresh :int)
  (min-theta :double)
  (max-theta :double))

(cffi:defcfun "hough_lines_p" :int
  (edge-thresh :int)
  (vote-thresh :int)
  (min-line-length :int)
  (max-line-gap :int))

(cffi:defcfun "gaussian_blur" :int
  (m-size :int))

(cffi:defcfun "find_contours" :int
  (m-size :int)
  (edge-thresh :int))

(cffi:defcfun "hough_circles" :int
  (dp :double)
  (param1 :double)
  (param2 :double)
  (min-radius :int)
  (max-radius :int)    
  )

(defun get-minimap ()
  (cut-image 1250 1200 1750 1425)
  )

(defun get-wood ()
  (let ((x 10))
   (cut-image x 10 (+ x 135) 70))
  )

(defun get-food ()
  (let ((x 145))
   (cut-image x 10 (+ x 135) 70))
  )

(defun get-gold ()
  (let ((x 280))
   (cut-image x 10 (+ x 135) 70))
  )

(defun get-stone ()
  (let ((x 420))
   (cut-image x 10 (+ x 135) 70))
  )


(defun get-vills ()
  (let ((x 540))
   (cut-image x 10 (+ x 210) 70))
  )


(defun save-this ()
  (let ()
    (unwind-protect
         (cffi:with-foreign-string (filename *output*)
           (copy-image filename)))))

(defun save-image (filename)
  (unwind-protect
       (cffi:with-foreign-string (f filename)
         (paste-image f)))  
  )


(defun test (n)
  (let ((image-filenames '(
                           "/data/images/IMG_20221013_040208_1390.JPG"
                           "/data/images/IMG_20221013_040217_1391.JPG"
                           "/data/images/IMG_20221013_040225_1392.JPG"                           
                           "/data/aoe_images/Screenshot 2022-04-10 14.23.17.png"
                           "/data/aoe_images/Screenshot 2022-04-23 22.41.28.png"
                           "/data/aoe_images/Screenshot 2022-05-07 17.15.09.png"
                           "/data/aoe_images/Screenshot 2022-05-12 21.20.30.png"
                           "/data/aoe_images/Screenshot 2022-06-16 13.56.17.png"
                           "/data/aoe_images/Screenshot 2022-06-17 23.15.03.png"
                           "/data/aoe_images/Screenshot 2022-08-31 02.12.46.png"
                           "/data/aoe_images/Screenshot 2022-09-10 18.57.06.png"
                           "/data/aoe_images/Screenshot 2022-09-18 03.02.22.png"
                           "/data/aoe_images/Screenshot 2022-11-02 23.07.17.png"
                           "/data/aoe_images/Screenshot 2022-11-11 14.14.09.png"
                           "/data/aoe_images/Screenshot 2022-11-11 14.14.14.png"
                           "/data/aoe_images/Screenshot 2022-11-11 14.14.40.png"
                           "/data/aoe_images/Screenshot 2022-11-25 11.53.13.png"
                           "/data/aoe_images/Screenshot 2022-12-16 05.27.13.png"
                           "/data/aoe_images/Screenshot 2022-12-25 13.29.59.png"
                           "/data/aoe_images/Screenshot 2023-01-01 21.09.53.png"
                           "/data/aoe_images/Screenshot 2023-02-20 18.08.42.png"
                           "/data/aoe_images/Screenshot 2023-02-22 13.35.17.png"
                           "/data/aoe_images/Screenshot 2023-02-26 04.58.34.png"
                           "/data/aoe_images/Screenshot 2023-03-07 22.48.50.png"                           

                           )))
    (unwind-protect
         (cffi:with-foreign-string (filename (nth n image-filenames))
           (copy-image filename)
           ;; (cut-image 1250 1200 1750 1425)
           ))))
;; (load-image "/data/001233702.PNG")
;; (draw-rectangle 0 0 1000 500)
;; (my-merge)
;; (canny-edge 50)
;; (hough-lines 60 10d0 (+ (/ pi 6) (/ pi 2)) pi)
;; (hough-lines 60 50 10 0d0 pi)
;; (hough-lines 150 0d0 pi)
;; (hough-lines-p 60 50 10)
;; (hough-lines-p 40 50 10)
;; (gaussian-blur 101)
;; (adaptive-threshold)
;; (sb-ext:exit)

(defun my-load-image (filename)
  (let ((handle (make-instance 'my-container)))
    (unwind-protect
         (cffi:with-foreign-string (f filename)
           (loop for i from 1 to 10
                 do (format t "~a: ~a~%" i (load-image handle f (cffi:foreign-enum-value 'imread-modes :IMREAD_COLOR))))))))

(defun load-lines ()
  (let ((filename "/data/aoe_images/lines.txt")
        (lines))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            do (let* ((my-line (remove #\Return line))
                     (points (mapcar #'parse-integer (cl-ppcre:split "," my-line)))
                      (x1 (nth 0 points))
                      (x2 (nth 1 points))
                      (y1 (nth 2 points))
                      (y2 (nth 3 points))
                      (slope (abs (/ (- y1 y2) (- x1 x2)))))
                 ;; (format t "~10d|~10d|~10d|~10d| ~10,3f~%" x1 x2 y1 y2 slope)
                 (push (list x1 x2 y1 y2) lines))))
    lines))


(defun load-circles ()
  (let ((filename "/data/aoe_images/circles.txt")
        (circles))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            do (let* ((my-line (remove #\Return line))
                     (points (mapcar #'parse-integer (cl-ppcre:split "," my-line)))
                      (x (nth 0 points))
                      (y (nth 1 points))
                      (r (nth 2 points)))
                 ;; (format t "~10d|~10d|~10d|~10d| ~10,3f~%" x1 x2 y1 y2 slope)
                 (push (list x y r) circles))))
    circles))


(defun load-rhos ()
  (let ((filename "/data/aoe_images/rhos.txt")
        (lines))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            do (let* ((my-line (remove #\Return line))
                      (parts (cl-ppcre:split "," my-line))
                      (rho (read-from-string (nth 0 parts)))
                      (theta (read-from-string (nth 1 parts))))
                 (format t "~10d|~10,3f~%" rho theta)
                 )))
    lines))


(defun my-load-image (index) 
  (let ((image-filenames '(
                           "/data/images/IMG_20221013_040208_1390.JPG"
                           "/data/images/IMG_20221013_040217_1391.JPG"
                           "/data/images/IMG_20221013_040225_1392.JPG"
                           ))
        (handle (make-instance 'my-container)))
    (unwind-protect
         (cffi:with-foreign-string (filename (nth index image-filenames))
           (copy-image filename)
           (hough-lines-p 40 50 50 10)           
           (let ((lines (load-lines)))
             (new-lines handle)
             (loop for line in lines
                   for i from 0
                   do (let* ((x1 (nth 0 line))
                             (x2 (nth 1 line))
                             (y1 (nth 2 line))
                             (y2 (nth 3 line))
                             (diff-x (abs (- x1 x2)))
                             (diff-y (abs (- y1 y2)))
                             (l
                               (sqrt
                                (+ (expt diff-x 2)
                                   (expt diff-y 2)
                                   )))
                             (slope (/ diff-y diff-x))

                             )

                        (progn
                          ;; (format t "~a~%" line)
                          (format t "~3d ~10d|~10d|~10d|~10d|~10d|~10d|~10,3f|~10,3f~%" i x1 x2 y1 y2 diff-x diff-y slope l )
                          (push-line handle x1 x2 y1 y2))))
             (draw-lines handle))))))

;; (find-contours 101 40)
(test 1)
;; (experiment)

;; (hough-circles 1.5d0 100d0 50d0 30 100)

;; (format t "~a~%" (sort (load-circles) #'< :key #'first))  
;; (sb-ext:exit)

