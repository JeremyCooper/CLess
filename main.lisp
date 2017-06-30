(ql:quickload 'lispbuilder-sdl-examples)
(defpackage #:cless
  (:use :common-lisp)
  (:export :main))

(in-package :cless)

(defparameter *w* 50)
(defparameter *active-player* :TWO)
(defparameter *state* nil)
(defparameter *prev-square* nil)
(defparameter *next-square* nil)
(defparameter *highlight* sdl:*yellow*)
(defparameter *board* (make-hash-table :test #'equal))
(defparameter *game-over* nil)
(defstruct square
  occupant
  owner
  x
  y
  bg-color
  highlighted)
(defun init-board ()
  (let ((temp-square nil)
	(homerow '(:ROOK
		   :KNIGHT
		   :BISHOP
		   :QUEEN
		   :KING
		   :BISHOP
		   :KNIGHT
		   :ROOK))
	(color sdl:*white*))
    (loop for y upto 7 do
	 (loop for x upto 7 do
	      (let ((square-name (concatenate 'string
					      (write-to-string x)
					      "X"
					      (write-to-string y))))
		(if (eql x 0) (setf color (if (eql color sdl:*white*) sdl:*black* sdl:*white*)))
		(cond
		  ((or (eql y 0) (eql y 7))
		   (setf temp-square (make-square :occupant (nth x homerow)
						  :owner (if (eql y 0)
							     :ONE
							     :TWO)
						  :x x :y y)))
		  ((eql y 1)
		   (setf temp-square (make-square :occupant :NEW-PAWN-ONE
						  :owner :ONE
						  :x x :y y)))
		  #| ((eql y 2)
		  (setf temp-square (make-square :occupant :ROOK
		  :owner :TWO
		  :x x :y y)))|#
		  ((eql y 6)
		   (setf temp-square (make-square :occupant :NEW-PAWN-TWO
						  :owner :TWO
						  :x x :y y)))
		  (t
		   (setf temp-square (make-square :occupant nil :x x :y y))))
		
		(setf (square-bg-color temp-square) color
		      (gethash square-name *board*) temp-square
		      color (if (eql color sdl:*white*) sdl:*black* sdl:*white*)))))))
(defun get-square (x y)
  (let ((square-name (concatenate 'string
				  (princ-to-string x)
				  "X"
				  (princ-to-string y))))
    (gethash square-name *board*)))
(defmacro gen-variants (variants &body body)
  `(progn
     ,@(loop for n in variants collect
	    (let ((z nil))
	      (read-from-string
	       (with-output-to-string (s)
		 (map 'nil
		      #'(lambda (x)
			  (if (char= #\^ x)
			      (setf z t)
			      (if z
				  (progn
				    (princ (nth (digit-char-p x) n) s)
				    (setf z nil))
				  (princ x s))))
		      (write-to-string (first body) :escape nil))))))))
#|(defun gen-square (x y)
(let ((square-name (concatenate 'string
				"SQUARE-"
				(write-to-string x)
				"X"
				(write-to-string y))))
  (intern square-name)))
(defmacro gen-board ()
  (let ((squares nil))
    (loop for x upto 8
       do (loop for y upto 8
	     do (push (list (gen-square x y) (make-square :occupant nil
							  :x x
							  :y y
							  :highlighted nil))
		      squares)))
    `(progn
       (defstruct board
	 ,@squares)
       (defparameter *board* (make-board)))))
(gen-board) |# ;; <== Evil black magic
(gen-variants ((occupant) (owner) (x) (y) (bg-color) (highlighted))
  (defun ^0-square (x y)
    (let ((square (get-square x y)))
      (square-^0 square))))

(defun load-piece (piece color x y w)
  (cond
    ((or (eql piece :PAWN-ONE) (eql piece :PAWN-TWO)
	 (eql piece :NEW-PAWN-ONE) (eql piece :NEW-PAWN-TWO))
     (sdl:draw-box (sdl:rectangle :x x :y y :w w :h w)
		   :color color))
    ((eql piece :ROOK)
     (sdl:draw-box (sdl:rectangle :x x :y y :w w :h w)
		   :color color))
    ((eql piece :KNIGHT)
     (sdl:draw-box (sdl:rectangle :x x :y y :w w :h w)
		   :color color))
    ((eql piece :BISHOP)
     (sdl:draw-box (sdl:rectangle :x x :y y :w w :h w)
		   :color color))
    ((eql piece :QUEEN)
     (sdl:draw-box (sdl:rectangle :x x :y y :w w :h w)
		   :color color))
    ((eql piece :KING)
     (sdl:draw-box (sdl:rectangle :x x :y y :w w :h w)
		   :color color))))
(defun render-piece (occupant owner x y)
  (let ((color (if (eql owner :ONE) sdl:*red* sdl:*blue*))
	(x (- (+ (* x *w*) (/ *w* 2)) 5))
	(y (- (+ (* y *w*) (/ *w* 2)) 5))
	(w 10))
    (load-piece occupant color x y w)))
(defun render-board () ;;renderer
  (sdl:clear-display sdl:*black*)
  (maphash
   #'(lambda (k v)
       (with-slots (occupant owner x y bg-color highlighted) v
	 ;;(format t "~a ~a ~a ~a ~a ~a ~%" k occupant x y bg-color highlighted)
	 ;;(if (eql bg-color sdl:*white*)
	 (sdl:draw-box (sdl:rectangle :x (* x *w*)
				      :y (* y *w*)
				      :w *w*
				      :h *w*)
		       :color (if highlighted *highlight* bg-color))
	 (if occupant
	     (render-piece occupant owner x y))))
   *board*))
(defun clear-board-highlights ()
  (maphash
   #'(lambda (k v)
       (setf (square-highlighted v) nil))
   *board*))
(defun determine-square (_x _y)
  (let* ((x (floor (/ _x *w*)))
	 (y (floor (/ _y *w*))))
    (if (or (< x 7) (< y 7))
	(get-square x y)
	nil)))

;;ruleset
(let ((loaded nil)
      (legal-moves (make-hash-table :test #'equal))
      (start-x nil) (start-y nil)
      (dest-x nil) (dest-y nil)
      (start-piece nil) (dest-piece nil))
  
  (defun setup-move-table ()
    (setf (gethash :NEW-PAWN-ONE legal-moves)
	  '((0 1) (0 2))
	  (gethash :PAWN-ONE legal-moves)
	  '((0 1))
	  (gethash :ATTACK-PAWN-ONE legal-moves)
	  '((-1 1) (1 1))
	  (gethash :NEW-PAWN-TWO legal-moves)
	  '((0 -1) (0 -2))
	  (gethash :PAWN-TWO legal-moves)
	  '((0 -1))
	  (gethash :ATTACK-PAWN-TWO legal-moves)
	  '((-1 -1) (1 -1))
	  (gethash :KNIGHT legal-moves)
	  '((-1 -2) (1 -2) (2 -1) (2 1)
	    (1 2) (-1 2) (-2 1) (-2 -1))
	  (gethash :ROOK legal-moves)
	  '((1 0) (-1 0) (0 1) (0 -1))
	  (gethash :BISHOP legal-moves)
	  '((-1 -1) (1 -1) (1 1) (-1 1))
	  (gethash :KING legal-moves)
	  '((1 0) (-1 0) (0 1) (0 -1)
	    (-1 -1) (1 -1) (1 1) (-1 1))
	  (gethash :QUEEN legal-moves)
	  '((1 0) (-1 0) (0 1) (0 -1)
	    (-1 -1) (1 -1) (1 1) (-1 1))))
  (defun step-vectors ()
    (let* ((vectors (gethash start-piece legal-moves))
	   (i (length vectors))
	   (valids nil))
      (dotimes (i i)
	;;(format t "ITERATION: ~a~%" i)
	(let* ((current-vector (nth i vectors))
	       (dx (first current-vector))
	       (dy (second current-vector)))
	  (do ((collision nil)
	       (x (+ start-x dx) (+ x dx))
	       (y (+ start-y dy) (+ y dy)))
	      (collision)
	    (if (or (< x 0) (< y 0)
		    (> x 7) (> y 7))
		(setf collision t)
		(if (setf collision (square-owner (get-square x y)))
		    ;;(format t "Collision: ~a~%")
		    (if (not (eql collision *active-player*))
			(if (not (or (eql start-piece :PAWN-ONE)
				     (eql start-piece :PAWN-TWO)
				     (eql start-piece :NEW-PAWN-ONE)
				     (eql start-piece :NEW-PAWN-TWO)))
			    (push (list x y) valids)))
		    (push (list x y) valids)))
	    (if (or (eql start-piece :KNIGHT)
		    (eql start-piece :KING)
		    (eql start-piece :PAWN-ONE)
		    (eql start-piece :PAWN-TWO)
		    (eql start-piece :NEW-PAWN-ONE)
		    (eql start-piece :NEW-PAWN-TWO))
		(setf collision t)))))
      (cond ((or (eql start-piece :PAWN-ONE)
		 (eql start-piece :NEW-PAWN-ONE))
	     (setf vectors (gethash :ATTACK-PAWN-ONE legal-moves)))
	    ((or (eql start-piece :PAWN-TWO)
		 (eql start-piece :NEW-PAWN-TWO))
	     (setf vectors (gethash :ATTACK-PAWN-TWO legal-moves))))
      (loop for n in vectors
	 do
	   (let ((collision nil)
		 (x (+ start-x (first n)))
		 (y (+ start-y (second n))))
	     (if (not (or (< x 0) (< y 0)
			  (> x 7) (> y 7)))
		 (if (setf collision (square-owner (get-square x y)))
		     (if (not (eql collision *active-player*))
			 (push (list x y) valids))))))
      valids))
  (defun display-legal-moves (square)
    (setf start-x (square-x square)
	  start-y (square-y square)
	  start-piece (square-occupant square))
    (loop for n in (step-vectors) do
	 (setf (square-highlighted (get-square (first n) (second n))) t)))
  (defun legal-move-p (destination)
    (if (not loaded)
	(progn
	  (setup-move-table)
	  (setf loaded t)))
    (setf dest-x (square-x destination)
	  dest-y (square-y destination)
	  dest-piece (square-occupant destination))
    (let ((legal-p nil))
      (if (eql (square-owner destination) *active-player*)
	  ;;add castling here?
	  (progn
	    (setf *next-square* destination)
	    (return-from legal-move-p nil))
	  (loop for n in (step-vectors) do
	       (progn
		 (if (and (eql dest-x (first n))
			  (eql dest-y (second n)))
		     (setf legal-p t)))))
      legal-p))
  (defun move-piece (destination)
    (cond
      ((eql start-piece :NEW-PAWN-ONE)
       (setf start-piece :PAWN-ONE))
      ((eql start-piece :NEW-PAWN-TWO)
       (setf start-piece :PAWN-TWO)))
    (let ((destroyed-piece nil))
      (if (setf destroyed-piece (square-occupant destination))
	  (progn
	    (format t "~a takes ~a!~%" start-piece destroyed-piece)
	    (if (eql destroyed-piece :KING)
		(game-over)))))
    (setf (square-occupant destination) start-piece
	  (square-occupant *prev-square*) nil
	  (square-owner destination) *active-player*
	  (square-owner *prev-square*) nil)))
(defun game-over ()
  (format t "~a wins!" *active-player*)
  (setf *game-over* t))
(defun main ()
  (init-board)
  (setf *game-over* nil
	*state* nil)
  (sdl:with-init ()
    (sdl:window (+ (* *w* 8) 100)
		(+ (* *w* 8))
		:title-caption "CLess")
    ;;(setf (sdl:frame-rate) 60)
    (let ((draw-frame t)
	  (square nil))
      (sdl:with-events ()
	(:quit-event () t)
	(:mouse-button-down-event (:button button :x x :y y)
				  (if (not *game-over*)
				      (if (eql button sdl:sdl-button-left)
					  (if (setf square (determine-square x y))
					      (cond
						((eql *state* nil)
						 (if (eql *active-player* (square-owner square))
						     (progn
						       (clear-board-highlights)
						       (display-legal-moves square)
						       (setf *state* :MOVE
							     *prev-square* square))))
						((eql *state* :MOVE)
						 (if (legal-move-p square)
						     (progn
						       (clear-board-highlights)
						       (move-piece square)
						       (setf *active-player*
							     (if (eql *active-player* :ONE)
								 :TWO
								 :ONE))
						       (setf *state* nil))
						     (progn
						       ;; (clear-board-highlights)
						       (setf *state* nil))))))))
				      (setf draw-frame t))
	(:idle () ;;main game loop
	       (if *next-square*
		   (progn
		     (clear-board-highlights)
		     (display-legal-moves *next-square*)
		     (setf *state* :MOVE
			   *prev-square* *next-square*
			   *next-square* nil)))
	       (if draw-frame
		   (progn
		     (render-board)
		     (sdl:update-display)
		     (setf draw-frame nil)))
	       (if *game-over*
		   (progn
		     (sdl:clear-display sdl:*black*)
		     (sdl:update-display))))))))
