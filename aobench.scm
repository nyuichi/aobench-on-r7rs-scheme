;;; for R7RS (chibi-scheme)
;;
;; (import (scheme base)
;; 	(scheme file)
;; 	(scheme write)
;; 	(scheme inexact)
;; 	(srfi 27))
;;
;; (define-syntax dotimes
;;   (syntax-rules ()
;;     ((dotimes (n count) body ...)
;;      (do ((n 0 (+ n 1)))
;; 	 ((= n count))
;;        body ...))))

;;; for Gauche
(use srfi-27)
(define (square x) (* x x))

;;; this looks good to me
(define drand48 random-real)

(define PI 3.14159265358979)

(define WIDTH 256)
(define HEIGHT 256)
(define NSUBSAMPLES 2)
(define NAO-SAMPLES 8)

;;; vector-3d

(define (vec x y z)
  (let ((v (make-vector 3)))
    (vector-set! v 0 x)
    (vector-set! v 1 y)
    (vector-set! v 2 z)
    v))

(define (vx x)
  (vector-ref x 0))

(define (vy x)
  (vector-ref x 1))

(define (vz x)
  (vector-ref x 2))

(define (v+ a b)
  (vec (+ (vx a) (vx b))
       (+ (vy a) (vy b))
       (+ (vz a) (vz b))))

(define (v- a b)
  (vec (- (vx a) (vx b))
       (- (vy a) (vy b))
       (- (vz a) (vz b))))

(define (vscale v x)
  (vec (* x (vx v))
       (* x (vy v))
       (* x (vz v))))

(define (vcross a b)
  (vec
    (- (* (vy a) (vz b)) (* (vy b) (vz a)))
    (- (* (vz a) (vx b)) (* (vz b) (vx a)))
    (- (* (vx a) (vy b)) (* (vx b) (vy a)))))

(define (vdot a b)
  (+ (* (vx a) (vx b))
     (* (vy a) (vy b))
     (* (vz a) (vz b))))

(define (vlen a)
  (sqrt (vdot a a)))

(define (vnormalize a)
  (let ((len (vlen a)))
    (if (> (abs len) 1.0e-17)
	(vscale a (/ len))
	a)))

;;; ray

(define (make-ray org dir)
  (cons org dir))

(define (ray-org ray)
  (car ray))

(define (ray-dir ray)
  (cdr ray))

;;; isect

(define (make-isect t p n hit)
  (let ((v (make-vector 4)))
    (vector-set! v 0 t)
    (vector-set! v 1 p)
    (vector-set! v 2 n)
    (vector-set! v 3 hit)
    v))

(define (isect-t i)
  (vector-ref i 0))

(define (isect-p i)
  (vector-ref i 1))

(define (isect-n i)
  (vector-ref i 2))

(define (isect-hit i)
  (vector-ref i 3))

;;; sphere

(define (make-sphere center radius)
  (let ((v (make-vector 2)))
    (vector-set! v 0 center)
    (vector-set! v 1 radius)
    v))

(define (sphere-center s)
  (vector-ref s 0))

(define (sphere-radius s)
  (vector-ref s 1))

;;; plane

(define (make-plane p n)
  (let ((v (make-vector 2)))
    (vector-set! v 0 p)
    (vector-set! v 1 n)
    v))

(define (plane-p p)
  (vector-ref p 0))

(define (plane-n p)
  (vector-ref p 1))

;;; objects

(define spheres (make-vector 3))
(define plane #f)

(define (ray-sphere-intersect isect ray sphere)
  (let ((rs (v- (ray-org ray)
		(sphere-center sphere))))
    (let* ((B (vdot rs (ray-dir ray)))
	   (C (- (vdot rs rs) (square (sphere-radius sphere))))
	   (D (- (* B B) C)))
      (when (> D 0)
	(let ((t (- .0 B (sqrt D))))
	  (when (< .0 t (isect-t isect))
	    (let ((hit 1)
		  (p (v+ (ray-org ray)
			 (vscale (ray-dir ray) t)))
		  (n (v- (isect-p isect)
			 (sphere-center sphere))))
	      (set! isect (make-isect t p (vnormalize n) hit))))))
      isect)))

(define (ray-plane-intersect isect ray plane)
  (let ((d (- (vdot (plane-p plane)
		    (plane-n plane))))
	(v (vdot (ray-dir ray)
		 (plane-n plane))))
    (when (>= (abs v) 1.0e-17)
      (let ((t (- (/ (+ (vdot (ray-org ray) (plane-n plane)) d) v))))
	(when (< .0 t (isect-t isect))
	  (let ((hit 1)
		(p (v+ (ray-org ray)
		       (vscale (ray-dir ray) t)))
		(n (plane-n plane)))
	    (set! isect (make-isect t p n hit))))))
    isect))

(define (orthobasis n)
  (let ((v (cond
	    ((< -0.6 (vx n) 0.6)
	     (vec 1.0 0.0 0.0))
	    ((< -0.6 (vy n) 0.6)
	     (vec 0.0 1.0 0.0))
	    ((< -0.6 (vz n) 0.6)
	     (vec 0.0 0.0 1.0))
	    (else
	     (vec 1.0 0.0 0.0)))))
    (let* ((s (vnormalize (vcross v n)))
	   (t (vnormalize (vcross n s))))
      (vector s t n))))

(define (ambient-occlusion isect)
  (let ((ntheta NAO-SAMPLES)
	(nphi NAO-SAMPLES)
	(eps 0.0001))
    (let ((p (v+ (isect-p isect)
		 (vscale (isect-n isect) eps)))
	  (basis (orthobasis (isect-n isect)))
	  (occlusion 0.0))
      (dotimes (i (* ntheta nphi))
	(let ((theta (sqrt (drand48)))
	      (phi (* 2 PI (drand48))))
	  (let ((x (* (cos phi) theta))
		(y (* (sin phi) theta))
		(z (sqrt (- 1.0 (square theta)))))
	    (let ((rx (+ (* x (vx (vector-ref basis 0)))
			 (* y (vx (vector-ref basis 1)))
			 (* z (vx (vector-ref basis 2)))))
		  (ry (+ (* x (vy (vector-ref basis 0)))
			 (* y (vy (vector-ref basis 1)))
			 (* z (vy (vector-ref basis 2)))))
		  (rz (+ (* x (vz (vector-ref basis 0)))
			 (* y (vz (vector-ref basis 1)))
			 (* z (vz (vector-ref basis 2))))))
	      (let ((ray (make-ray p (vec rx ry rz))))
		(let ((occ-isect (make-isect 1.0e+17 (vec .0 .0 .0) (vec .0 .0 .0) 0)))
		  (set! occ-isect
			(ray-sphere-intersect occ-isect ray (vector-ref spheres 0)))
		  (set! occ-isect
			(ray-sphere-intersect occ-isect ray (vector-ref spheres 1)))
		  (set! occ-isect
			(ray-sphere-intersect occ-isect ray (vector-ref spheres 2)))
		  (set! occ-isect
			(ray-plane-intersect occ-isect ray plane))
		  (when (not (zero? (isect-hit occ-isect)))
		    (set! occlusion (+ occlusion 1)))))))))
      (set! occlusion
	    (/ (- (* ntheta nphi) occlusion)
	       (* ntheta nphi)))
      (vec occlusion occlusion occlusion))))

(define (clamp f)
  (let ((i (* f 255.5)))
    (cond
      ((< i 0) 0)
      ((> i 255) 255)
      (else (exact (floor i))))))

(define (render w h nsubsamples)
  (let ((img (make-vector (* w h 3)))
	(col (vec .0 .0 .0)))
    (dotimes (y h)
      (dotimes (x w)
	(set! col (vec .0 .0 .0))
	(dotimes (v nsubsamples)
	  (dotimes (u nsubsamples)
	    (let ((px (/ (+ x (/ u nsubsamples) (/ w -2.0))
			 (/ w 2.0)))
		  (py (- (/ (+ y (/ v nsubsamples) (/ h -2.0))
			    (/ h 2.0)))))
	      (let ((ray (make-ray (vec .0 .0 .0) (vnormalize (vec px py -1.0)))))
		(let ((isect (make-isect 1.0e+17 (vec .0 .0 .0) (vec .0 .0 .0) 0)))
		  (set! isect (ray-sphere-intersect isect ray (vector-ref spheres 0)))
		  (set! isect (ray-sphere-intersect isect ray (vector-ref spheres 1)))
		  (set! isect (ray-sphere-intersect isect ray (vector-ref spheres 2)))
		  (set! isect (ray-plane-intersect isect ray plane))
		  (when (not (zero? (isect-hit isect)))
		    (set! col (v+ col (ambient-occlusion isect)))))))))
	(set! col (vscale col (/ (square nsubsamples))))
	(vector-set! img (+ (* 3 (+ (* y w) x)) 0) (clamp (vx col)))
	(vector-set! img (+ (* 3 (+ (* y w) x)) 1) (clamp (vy col)))
	(vector-set! img (+ (* 3 (+ (* y w) x)) 2) (clamp (vz col))))
    img))

(define (init-scene)
  (vector-set! spheres 0 (make-sphere (vec -2.0 0.0 -3.5) 0.5))
  (vector-set! spheres 1 (make-sphere (vec -0.5 0.0 -3.0) 0.5))
  (vector-set! spheres 2 (make-sphere (vec 1.0 0.0 -2.2) 0.5))
  (set! plane (make-plane (vec 0.0 -0.5 0.0) (vec 0.0 1.0 0.0))))

(define (save-ppm filename w h img)
  (with-output-to-file filename
    (lambda ()
      (display "P3\n")
      (display w) (display " ") (display h) (newline)
      (display "255\n")
      (dotimes (y h)
	(dotimes (x w)
	  (display (vector-ref img (+ (* 3 (+ (* y w) x)) 0)))
	  (display " ")
	  (display (vector-ref img (+ (* 3 (+ (* y w) x)) 1)))
	  (display " ")
	  (display (vector-ref img (+ (* 3 (+ (* y w) x)) 2)))
	  (newline))))))

(define (run)
  (init-scene)
  (let ((img (render WIDTH HEIGHT NSUBSAMPLES)))
    (save-ppm "ao.ppm" WIDTH HEIGHT img)))

(run)
