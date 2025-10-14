(ql:quickload :split-sequence)
(ql:quickload :alexandria)
;; following code was NOT made by me. online stack overflow user designed this.
(defun counter (len)
  (loop for i below len
        collect i))
;; the following code was made by me! :)
(defun fit-into-threes (lst)
  (loop for val in (counter (/ (length lst) 3))
	collect (subseq lst (* 3 val) (* 3 (1+ val)))))

(defun fit-into-threes-2 (lst)
  (mapcar (lambda (x y z) (append (list x) (list y) (list z)))
	  (loop for val in lst by #'cdddr collect val)
	  (loop for val in (cdr lst) by #'cdddr collect val)
	  (loop for val in (cddr lst) by #'cdddr collect val)))

(defun make-ppm (size data &optional (format "P3") (max 255) (output "output.ppm"))
  "SIZE is a list of (width height). DATA is a list of (r g b) values"
  (with-open-file (f output
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-sequence (concatenate 'string format "
") f) 
    (write-sequence (concatenate 'string (write-to-string (car size)) " " (write-to-string (cadr size)) "
") f)
    (write-sequence (concatenate 'string (write-to-string max) "
 ") f)
      (loop for val in data
	    do (write-sequence (apply #'concatenate (append (list 'string) (mapcar #'(lambda (x) (concatenate 'string x " ")) (mapcar #'write-to-string val)))) f))))

(defun string-to-list (string)
  (butlast (mapcar (lambda (x) (if (not (equal x "")) (parse-integer x)))
	  (cddr (split-sequence:split-sequence #\SPACE string)))))
(defun read-file (input)
  (let ((x ""))
    (with-open-file (f input)
      (do ((l (read-line f) (read-line f nil 'eof)))
          ((eql l 'eof) "done!")
	(setq x (concatenate 'string x l))))
    x))

(defun grayscale (img)
  (loop for pixel in img
	collect (make-list 3 :initial-element (round (/ (reduce #'+ pixel) 3))))) 

(setq width 300)
(setq height 300)

(defvar img (fit-into-threes-2 (string-to-list (read-file "input.ppm"))))
(defvar gray-img (grayscale img))

(make-ppm (list width height) gray-img)

;; 0.109
;; 0.132

(defun get-region (img middle width)
  "IMG should be a list of elements that are (r g b) MIDDLE should be the starting index and WIDTH should be the width of the img"
  (list
   (subseq img (- (1- middle) width) (- (+ 2 middle) width))
   (subseq img (1- middle) (+ 2 middle))
   (subseq img (+ width (1- middle)) (+ width (+ 2 middle)))))

(defun convolve (mtrx1 mtrx2)
  (apply #'+ (mapcar #'(lambda (x y) (* x y)) mtrx1 mtrx2)))

(defvar dir-x '(-1 0 1 -2 0 2 -1 0 1))
(defvar dir-y '(-1 -2 -1 0 0 0 1 2 1))

(defun prep-for-convolution (img size)
  "thickens border of image due to convolution's data loss. image IMG and SIZE (width height)"
  (let* ((new-img
	  (append
	   (subseq img 0 (car size))
	   img
	   (subseq img (- (* (car size) (cadr size)) (car size)) (* (car size) (cadr size)))))
	 (full-img '()))
;;    (make-ppm '(300 302) new-img "P3" 255 "mid-output.ppm")      ;; debugging!
    (loop for pos in (counter (+ 2 (cadr size)))
	  do (let* ((row (subseq new-img (* (cadr size) pos) (* (cadr size) (1+ pos)))))
	       (setq full-img (append full-img (list (car row)) row (last row)))))
;;    (make-ppm '(302 302) full-img "P3" 255 "midder-output.ppm")  ;; debugging!
    full-img))


(defun convolve-img (img size filter)
  "image IMG and SIZE (width height)"
  (let* ((expanded (prep-for-convolution img size)))
;;    (print (nth (1+ 89999) expanded))
    (loop for pxl-pos in (counter (- (length img) 0))
;;	  do (if (> pxl-pos 88000) (print pxl-pos))
	  collect (convolve
		   (simplify-output
		    (get-region expanded (+ (car size) (1+ pxl-pos)) (car size)))
		   filter))))

(defun new-convolve-img (img size filter
			 &optional (expanded (prep-for-convolution img size))
			   (x 0) (y 0) (product nil))
  "image IMG and SIZE (width height) with matrix FILTER"
  (let ((val (+ 2 (cadr size) 1 x (* 3 y))))
    ;;(print (list product (list val x y))) debugging
    (cond
      ((eq (+ val 1 (+ 2 (cadr size))) (* (+ 2 (car size)) (+ 2 (cadr size))))
       product)
      ((eq (mod val (+ 2 (cadr size))) (1- (+ 2 (cadr size))))
       (new-convolve-img img size filter expanded (1- x) (1+ y) product))
      (t
       (new-convolve-img img size filter expanded (1+ x) y
			 (append product
				 (list
			    	  (convolve
				   (simplify-output
				    (get-region expanded val (+ 2 (cadr size))))
				   filter))))))))

(defun simplify-output (output)
  (alexandria::flatten (mapcar #'(lambda (x) (list (caar x) (caadr x) (caaddr x))) output)))


(defvar tst-final-2
  (mapcar (lambda (z) (make-list 3 :initial-element (round z)))
	  (mapcar (lambda (x y) (sqrt (+ (* x x) (* y y))))
		  (new-convolve-img gray-img (list width height) dir-x)
		  (new-convolve-img gray-img (list width height) dir-y))))

(time (make-ppm (list 300 300) tst-final-2))



(print (new-convolve-img IMGTST '(3 3) dir-x))


(defvar TEST (prep-for-convolution '((1 1 1) (2 2 2) (3 3 3) (4 4 4) (5 5 5) (6 6 6) (7 7 7) (8 8 8) (9 9 9)) '(3 3)))
(defvar IMGTST '((1 1 1) (2 2 2) (3 3 3) (4 4 4) (5 5 5) (6 6 6) (7 7 7) (8 8 8) (9 9 9)))

(print (append nil (list (convolve '(1 1 2 1 1 2 4 4 5) dir-x))))
(print (get-region TEST 6 5))

(print (convolve-img '((1 1 1) (2 2 2) (3 3 3) (4 4 4) (5 5 5) (6 6 6) (7 7 7) (8 8 8) (9 9 9)) '(3 3) dir-x))


(print (subseq expanded-img 0 9))

(print (convolve
	(simplify-output (get-region expanded-img (+ 300 (1+ 90000)) 300))
       dir-x))

   
(setq expanded-img (prep-for-convolution gray-img (list width height))) ;; goal: 91204

(make-ppm '(302 302) expanded-img "P3" 255 "double-check.ppm")

(print (get-region expanded-img 301 300))

(make-ppm '(300 300) (convolve-img gray-img '(300 300) dir-x) "P3" 255 "dir-x.ppm")





(print tst-final)

	
  


			  





