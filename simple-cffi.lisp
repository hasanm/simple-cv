(require :cffi)
(require :cffi-libffi)

;; (push #P"/home/p-hasan/src/lisp/lisp-magick-wand/" asdf:*central-registry*)
;; (require "lisp-magick-wand")

(cffi:define-foreign-library libsimple
  (t (:default "libsimple")))


(setq cffi:*foreign-library-directories* (list #P"/home/p-hasan/work/qt/simple-cv/build/"))

(cffi:use-foreign-library libsimple)
;; (cffi:load-foreign-library 'libsimple)

(cffi:defcstruct person
  (number :int))

(cffi:defcfun "copy_image" :int
  (filename :pointer))

(cffi:defcfun "paste_image" :int
  (filename :pointer))

(cffi:defcfun "cut_image" :int
  (x :int)
  (y :int)
  (p :int)
  (q :int))

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
  (min-line-length :int)
  (max-line-gap :int))

(cffi:defcfun "gaussian_blur" :int
  (m-size :int))

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

(defun load-image (filename)
  (unwind-protect
       (cffi:with-foreign-string (f filename)
         (copy-image f))))

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
(test 1)
;; (my-merge)
;; (canny-edge 50)
;; (hough-lines 60 50 10 (+ (/ pi 6) (/ pi 2)) pi)
;; (hough-lines 60 50 10 0d0 pi)
;; (hough-lines 60 0d0 pi)
;; (hough-lines-p 60 50 10)
; (hough-lines-p 40 150 10)
;; (gaussian-blur 101)
(adaptive-threshold)
(sb-ext:exit)


;;(sb-ext:exit)





;; (make-gray)

;; (my-merge)

;; (save-image "/data/images/merged.png")

;; (adaptive-threshold)



;;(copy-image 1250 1200 1750 1425)

;; (cut-image 100 100 300 300)

;; (reload)
;; (get-minimap)


;; (sb-ext:exit)




;; (cffi:close-foreign-library 'libsimple)
