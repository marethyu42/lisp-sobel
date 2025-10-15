;; importing packages:
(ql:quickload :split-sequence)
(ql:quickload :alexandria)

;; following code was NOT made by me. online stack overflow user designed this.
(defun counter (len)
  "equivalent to a range function in python, starting from 0 and going to 1- LEN"
  (loop for i below len
        collect i))

;; the following code was made by me! :)
(defun fit-into-threes-2 (lst)
  "convert a LST of values into trios (i.e. (1 2 3 4 5 6) -> ((1 2 3) (4 5 6))"
  (mapcar (lambda (x y z) (append (list x) (list y) (list z)))
	  (loop for val in lst by #'cdddr collect val)
	  (loop for val in (cdr lst) by #'cdddr collect val)
	  (loop for val in (cddr lst) by #'cdddr collect val)))

(defun make-ppm (size data &optional (format "P3") (max 255) (output "output.ppm")) ;;using ppms to import file information! (how dare i use side effects in lisp :O)
  "SIZE is a list of (width height). DATA is a list of (r g b) values. import DATA to create a ppm. bonus, you can rename the file with OUTPUT. has side effects." 
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

(defun string-to-list (str)
  "convert a STRING of integers into a list of integers"
  (butlast (mapcar (lambda (x) (if (not (equal x "")) (parse-integer x)))
		   (cddr (split-sequence:split-sequence #\SPACE str)))))

(defun read-file (input)
  "read ppm named INPUT into a string"
  (let ((x ""))
    (with-open-file (f input)
      (do ((l (read-line f) (read-line f nil 'eof)))
          ((eql l 'eof) "done!")
	(setq x (concatenate 'string x l))))
    x))

(defun grayscale (img)
  "grayscale an IMG (a list of elements that are (r g b))"
  (loop for pixel in img
	collect (make-list 3 :initial-element (round (/ (reduce #'+ pixel) 3))))) 

(setq width 300)
(setq height 300)  ;; defining the height & width for our image ;; goal: make this automatic? (a bit hard to do :c)

(defvar img (fit-into-threes-2 (string-to-list (read-file "input.ppm")))) ;;importing our image
(defvar gray-img (grayscale img)) ;;grayscaling it

(make-ppm (list width height) gray-img) ;; unnecessary bonus middle step to show how the image is now gray

(defun get-region (img middle width)
  "IMG should be a list of elements that are (r g b) MIDDLE should be the starting index and WIDTH should be the width of the img. gives a list of the 3x3 region around the middle point"
  (list
   (subseq img (- (1- middle) width) (- (+ 2 middle) width))
   (subseq img (1- middle) (+ 2 middle))
   (subseq img (+ width (1- middle)) (+ width (+ 2 middle)))))

(defun convolve (mtrx1 mtrx2)
  "takes MTRX1 and MTRX2 and convolves the two of them to get one scalar"
  (apply #'+ (mapcar #'(lambda (x y) (* x y)) mtrx1 mtrx2)))

(defvar dir-x '(-1 0 1 -2 0 2 -1 0 1))
(defvar dir-y '(-1 -2 -1 0 0 0 1 2 1)) ;;defining our two filters, our x and y derivative for sobel feldman

(defun prep-for-convolution (img size)
  "thickens border of image due to convolution's data loss. IMG of data and size of SIZE (width height)"
  (let* ((new-img
	  (append
	   (subseq img 0 (car size))
	   img
	   (subseq img (- (* (car size) (cadr size)) (car size)) (* (car size) (cadr size)))))
	 (full-img '()))
    (loop for pos in (counter (+ 2 (cadr size)))
	  do (let* ((row (subseq new-img (* (cadr size) pos) (* (cadr size) (1+ pos)))))
	       (setq full-img (append full-img (list (car row)) row (last row)))))
    full-img))


(defun new-convolve-img (img size filter
			 &optional (expanded (prep-for-convolution img size))
			   (x 0) (y 0) (product nil)) ;; (god i hated this function (IT WAS THE SOURCE OF MY ERRORS D:) that's why its "new")
  ;; this is in charge of deciding how/where we apply the filter on our base image which does explain why the bug make it look weird
  "image IMG and SIZE (width height) with 3x3 matrix FILTER"
  (let ((val (+ 2 (cadr size) 1 x (* 3 y))))
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
  "convert a grayscaled img 3x3 region OUPUT into 9 sole numbers, i.e. ((3 3 3) (4 4 4) ...) -> (3,4,...) for simplicity"
  (alexandria::flatten (mapcar #'(lambda (x) (list (caar x) (caadr x) (caaddr x))) output)))

(defvar final ;;final output! yay
  (mapcar (lambda (z) (make-list 3 :initial-element (round z)))
	  (mapcar (lambda (x y) (sqrt (+ (* x x) (* y y))))
		  (new-convolve-img gray-img (list width height) dir-x)
		  (new-convolve-img gray-img (list width height) dir-y))))

(time (make-ppm (list width height) final)) ;;side effect, write output to file


;; id love to add:
;; - make everything work with one function call (ough it's a little bit of a mess right now :P) for convenience
;; - use the edge detected image to make lines in the source image colored (make edges gold!)
;; - use the edge detected image to alter the edges (make edges brighter!)
;;   - try out other fun images (where does this work well vs not at all?)
;;   - is it pretty?? if not: how do i make it pretty?
;; - make it faster >:3
;; - huge leap: convert between png to ppm for the user? (what if i just api call to someone else who does the hardwork :P)


;; large term goals, if i want to make this a big platform for my computer graphics needs:

;; - gaussian blur
;; - kuhwahara
;; - DoG
;; - ditherpunk (https://surma.dev/things/ditherpunk/)
;; - canny edge detector
