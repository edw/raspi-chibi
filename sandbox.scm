(import (chibi)
        (except (chibi show) fn)
        (skim core)
        (srfi 1)
        (srfi 9)
        (srfi 33)
        (srfi 69))

(define (framebuffer-ready? fb)
  "Return a truthy value if framebuffer FB is usable"
  (and (>= (car fb) 0)
       (bytevector? (cadr fb))))

(define (framebuffer-plot fb x y val)
  "Plot a pixel (slowly) at X, Y with VAL directly to the framebuffer"
  (let ((offset (+ (* y 8) x))
        (buffer (cadr fb)))
    (framebuffer-plot-primitive buffer offset val)))

(define lob (cond-expand (little-endian 0) (else 1)))
(define hob (cond-expand (little-endian 1) (else 0)))

(define-record-type <bitmap>
  (construct-bitmap w h buffer)
  bitmap?
  (w bitmap-width)
  (h bitmap-height)
  (buffer bitmap-buffer))

(define black 0)
(define white 65535)

(define (make-bitmap w h)
  (construct-bitmap w h (make-bytevector (* w h 2) 0)))

(define (plot bm x y color)
  (bytevector-u8-set! (bitmap-buffer bm)
                      (+ (* 2 (+ (* y (bitmap-width bm)) x)) lob)
                      (bitwise-and #x00ff color))
  (bytevector-u8-set! (bitmap-buffer bm)
                      (+ (* 2 (+ (* y (bitmap-width bm)) x)) hob)
                      (-> (bitwise-and #xff00 color)
                          (arithmetic-shift -8))))

(define (blit bm x y)
  (framebuffer-blit-primitive (cadr fb)
                              (bitmap-buffer bm)
                              (bitmap-width bm)
                              (bitmap-height bm)
                              x
                              y))

(define (scale n max+1)
  "Return a value in [0,1] representing a scaling of N within [0, MAX]"
  (exact (round (* (min n 1.0) (- max+1 1)))))

(define (rgb->val r g b)
  "Return a 565-RGB value representing R, G, and B values in [0,1]"
  (bitwise-ior (arithmetic-shift (scale r 32) 11)
               (arithmetic-shift (scale g 64) 5)
               (scale b 32)))

(define pi (* 2 (asin 1)))
(define pi/3 (/ pi 3.0))
(define pi*2 (* pi 2.0))

(define (mod2pi x)
  "Return X (a real) modulo pi times two"
  (let ((y (floor (/ x pi*2))))
    (- x (* pi*2 y))))

(define (mod2 x)
  "Return X (a real) modulo two"
  (let ((y (floor (/ x 2))))
    (- x (* 2 y))))

(define (hsv->val h s v)
  "Return a 565-RGB value representing an HSV color"
  (let* ((h (mod2pi h))
         (s (-> s (max 0.0) (min 1.0)))
         (v (-> v (max 0.0) (min 1.0)))
         (c (* v s))
         (h1 (/ h pi/3))
         (x (* c (- 1 (abs (- (mod2 h1) 1)))))
         (m (- v c))
         (helper (lambda (r g b)
                   (rgb->val (+ r m) (+ g m) (+ b m)))))
    (cond ((< h1 1)
           (helper c x 0))
          ((< h1 2)
           (helper x c 0))
          ((< h1 3)
           (helper 0 c x))
          ((< h1 4)
           (helper 0 x c))
          ((< h1 5)
           (helper x 0 c))
          (else
           (helper c 0 x)))))

(define (ramp)
  (dosiq #(r (iota 11 0.0 0.1)
           g (iota 11 0.0 0.1)
           b (iota 11 0.0 0.1))
    (begin (blit (make-bitmap 8 8 (rgb->val r g b))))))

(define (wheel x y spread b theta)
  (let ((bitmap (make-bitmap 8 8))
        (ox x)
        (oy y)
        (r 4))
    (dosiq #(x (iota 8)
             y (iota 8)
             lit: #(px (- x ox)
                    py (- y oy)
                    theta (+ theta (atan (/ px py)))
                    theta (if (> py 0) theta (+ theta pi))
                    r (sqrt (+ (* px px) (* py py)))
                    r (min (/ r spread) 1.0)
                    val (hsv->val theta r b)))
      (plot bitmap x y val))
    (blit bitmap 0 0)))

(define (ramp-wheel)
  (dosiq #(b (iota 17 0.0 (/ 1.0 16.0)))
    (wheel 3.5 3.5 4.0 b 0)))

(define (rotate-wheel-forever x y r b steps)
  (let ((delta (/ pi*2 steps)))
     (lip iter #(theta 0)
       (wheel x y r b theta)
       (iter (mod2pi (+ theta delta))))))

(define (insane)
  (let ((delta-theta (/ pi*2 65536))
         (delta-iota (/ pi*2 1941))
         (delta-spread-magnitude 0.1)
         (min-spread 1.0)
         (max-spread 6.0)
         (radius 5.0)
         (origin-x 3.5)
         (origin-y 3.5)
         (brightness 0.5)
         (delta-brightness-magnitude 0.07)
         (min-brightness 0.0)
         (max-brightness 1.0))
    (lip iter #(theta 0 iota 0
                spread min-spread
                delta-spread delta-spread-magnitude
                delta-brightness delta-brightness-magnitude)
      (let ((x (+ origin-x (* radius (cos iota))))
            (y (+ origin-y (* radius (sin iota))))
            (spread (+ spread delta-spread))
            (brightness (+ brightness delta-brightness)))
        (wheel x y spread brightness theta)
        (iter (mod2pi (+ theta delta-theta))
              (mod2pi (+ iota delta-iota))
              spread
              (cind (> spread max-spread)
                    (* -1.0 delta-spread-magnitude)
                    (< spread min-spread)
                    delta-spread-magnitude
                    else
                    delta-spread)
              (cond ((> brightness max-brightness)
                     (* -1.0 delta-brightness-magnitude))
                    ((< brightness min-brightness)
                     delta-brightness-magnitude)
                    (else
                     delta-brightness)))))))

(define (brot cx cy scale)
  (define (cell-color c z iters)
    (if (> iters 255)
        (hsv->val 0.0 0.0 0.0)
        (if (> (magnitude z) 2)
            (let ((iters-norm (/ (- 256 iters) 256)))
              (hsv->val (* pi*2 iters-norm) 1.0 iters-norm))
            (cell-color c (+ (expt z 2) c) (+ iters 1)))))
  (let* ((w 8)
         (h 8)
         (cx-pxs (/ (- w 1) 2))
         (cy-pxs (/ (- w 1) 2))
         (pxs (make-bitmap 8 8)))
    (let iter-y ((y 0))
      (if (< y h)
          (let ((i (+ cy (* scale (- y cy-pxs)))))
            (let iter-x ((x 0))
              (if (< x w)
                  (let* ((j (+ cx (* scale (- x cx-pxs)))))
                    (plot pxs x y (cell-color (make-rectangular i j) 0 0))
                    (iter-x (+ x 1)))
                  (iter-y (+ y 1)))))
          pxs))))

(define (iter-brot cx cy begin-scale scale-step iters)
  (let iter ((scale begin-scale)
             (i 0))
    (blit (brot cx cy scale) 0 0)
    (if (< i iters)
        (iter (* scale scale-step) (+ i 1)))))

; (iter-brot fb 0.35 -0.70 0.5 0.95 200)

(define (iter-read-evdev fd)
  (let iter ((value (read-evdev fd)))
    (if value
        (show #t "event: " value nl))
    (iter (read-evdev fd))))

(define (keydown? k)
  (evdev-keydown? evfd k))

(define (get-millis-monotonic)
  (let* ((ts (clock-gettime clock-monotonic-raw))
         (sec (vector-ref ts 0))
         (msec (* (vector-ref ts 1) 10e-07)))
    (exact (floor (+ (* sec 10e03) msec)))))

(define (explore-brot cx cy scale scale-step)
  (let iter ((cx cx)
             (cy cy)
             (scale scale)
             (last-key #f)
             (key-msec #f)
             (penult-key #f))
    (blit (brot cx cy scale) 0 0)
    (let ((nudge (/ scale 3))
          (wall-msec (get-millis-monotonic)))
      (cond
       ((keydown? key-left)
        (iter (+ cx nudge) cy scale 'left (or key-msec wall-msec) last-key))
       
       ((keydown? key-right)
        (iter (- cx nudge) cy scale 'right (or key-msec wall-msec) last-key))
       
       ((keydown? key-up)
        (iter cx (+ cy nudge) scale 'up (or key-msec wall-msec) last-key))
       
       ((keydown? key-down)
        (iter cx (- cy nudge) scale 'down (or key-msec wall-msec) last-key))
       
       ((keydown? key-push)
        (if (and (eq? last-key 'push)
                 (> wall-msec (+ key-msec 500)))
            (iter cx cy (/ scale scale-step) 'push key-msec last-key)
            (iter cx cy scale 'push (or key-msec wall-msec) last-key)))
       
       (else
        (cond ((and (eq? last-key 'push)
                    (<= wall-msec (+ key-msec 500)))
               (iter cx cy (* scale scale-step) #f #f last-key))
              (else
               (when (and (not last-key) penult-key)
                     (show #t "(explore-brot " cx " " cy " " scale
                           " " scale-step ")" nl))
               (iter cx cy scale #f #f last-key))))))))

;; (explore-brot 0.163002692367463 -1.22465413174641 3.36999333339384e-04 0.9)
;; (explore-brot 7.6835493612747e-05 -1.57518464463028 0.000312108128894424 0.9)
;; (explore-brot 0.000677636708120639 -1.57497165488726 3.67981244787789e-07 0.9)
;; (explore-brot 0.000676970662067573 -1.57497105875764 3.73671931178952e-06 0.9)

;; Macro view (red, 2px top and bottom borders, magenta band)
;; (explore-brot 0.000677636708120639 -0.529436759655122 0.403472963491905 0.9)

;; Macro view (red, 1px top and bottom borders)
;; (explore-brot -8.14520911458772e-05 -0.399215288627525 0.294131790385599 0.9)

;; Similar
;; (explore-brot 0.000201934636950964 -0.519542868607324 0.363125667142715 0.9)

;; (iter-brot 0.163002692367463 -1.22465413174641 0.2 0.9 100)
