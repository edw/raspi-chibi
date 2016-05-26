(import (except (scheme base) equal?)
        (chibi repl)
        (chibi filesystem))

(define evdev-name "Raspberry Pi Sense HAT Joystick")
(define fbdev-name "RPi-Sense FB")

(define evfd #f)
(define fb #f)

(define key-left 105)
(define key-up 103)
(define key-right 106)
(define key-down 108)
(define key-push 28)

(define clock-realtime 0)
(define clock-monotonic 1)
(define clock-process-cputime-id 2)
(define clock-thread-cputime-id 3)
(define clock-monotonic-raw 4)
(define clock-realtime-coarse 5)
(define clock-monotonic-coarse 6)
(define clock-boottime 7)

(load "sandbox.scm")

(dynamic-wind
    (lambda ()
      (set! evfd (evdev-open evdev-name))
      (set! fb (framebuffer-open fbdev-name)))
    repl
    (lambda ()
      (if (car fb)
          (begin (framebuffer-close fb)
                 (set-car! fb #f)
                 (show #t "Closed framebuffer" nl))
          (show #t "Could not open framebuffer" nl))
      (if (>= evfd 0)
          (begin (evdev-close evfd)
                 (set! evfd #f)
                 (show #t "Closed event device" nl))
          (show #t "Could not open event device" nl))))
