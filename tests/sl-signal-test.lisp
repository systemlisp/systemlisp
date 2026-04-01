(in-package :system-lisp-test)

(def-suite* sl-signal-test
  :description "Tests for the various types of signals supported by SL"
  :in sl-main-test)

;;;============================================================
;;; sl-signal-binary tests
;;;============================================================
(def-suite* sl-signal-binary-test
  :in sl-signal-test)

(test signal-binary-initial-value
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((sig (make-instance 'sl-signal-binary :bit-width 8)))
      (is (sl= 0 (value sig))))))

(test signal-binary-setb-integer
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((sig (make-instance 'sl-signal-binary :bit-width 8)))
      (setb sig 42)
      (is (sl= 42 (value sig))))))

(test signal-binary-setb-sl-uint
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((sig (make-instance 'sl-signal-binary :bit-width 8)))
      (setb sig (uint 99 :bits 8))
      (is (sl= 99 (value sig))))))

(test signal-binary-setb-sl-int
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((sig (make-instance 'sl-signal-binary :bit-width 8 :signed t)))
      (setb sig (int -5 :bits 8))
      (is (sl= -5 (value sig))))))

(test signal-binary-unsigned-truncation
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((sig (make-instance 'sl-signal-binary :bit-width 4)))
      (setb sig #xFF)
      (is (sl= #xF (value sig))))))

(test signal-binary-signed-truncation
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((sig (make-instance 'sl-signal-binary :bit-width 4 :signed t)))
      (setb sig 8)
      (is (sl= -8 (value sig))))))

(test signal-binary-propagation-single
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((driver (make-instance 'sl-signal-binary :bit-width 8))
	  (load   (make-instance 'sl-signal-binary :bit-width 8)))
      (connect-driver-load driver load)
      (setb driver 77)
      (is (sl= 77 (value load))))))

(test signal-binary-propagation-chain
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((a (make-instance 'sl-signal-binary :bit-width 16))
	  (b (make-instance 'sl-signal-binary :bit-width 16))
	  (c (make-instance 'sl-signal-binary :bit-width 16)))
      (connect-driver-load a b)
      (connect-driver-load b c)
      (setb a #xABCD)
      (is (sl= #xABCD (value b)))
      (is (sl= #xABCD (value c))))))

(test signal-binary-propagation-fanout
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((driver (make-instance 'sl-signal-binary :bit-width 8))
	  (load1  (make-instance 'sl-signal-binary :bit-width 8))
	  (load2  (make-instance 'sl-signal-binary :bit-width 8))
	  (load3  (make-instance 'sl-signal-binary :bit-width 8)))
      (connect-driver-load driver load1)
      (connect-driver-load driver load2)
      (connect-driver-load driver load3)
      (setb driver 55)
      (is (sl= 55 (value load1)))
      (is (sl= 55 (value load2)))
      (is (sl= 55 (value load3))))))

(test signal-binary-propagation-width-narrowing
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((driver (make-instance 'sl-signal-binary :bit-width 16))
	  (load   (make-instance 'sl-signal-binary :bit-width 8)))
      (connect-driver-load driver load)
      (setb driver #x1234)
      (is (sl= #x34 (value load))))))

(test signal-binary-multiple-updates
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((driver (make-instance 'sl-signal-binary :bit-width 8))
	  (load   (make-instance 'sl-signal-binary :bit-width 8)))
      (connect-driver-load driver load)
      (setb driver 10)
      (is (sl= 10 (value load)))
      (setb driver 20)
      (is (sl= 20 (value load)))
      (setb driver 0)
      (is (sl= 0 (value load))))))

(test signal-binary-setb-signal-to-signal
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((a (make-instance 'sl-signal-binary :bit-width 8))
	  (b (make-instance 'sl-signal-binary :bit-width 8)))
      (setb a 42)
      (setb b a)
      (is (sl= 42 (value b))))))

;;;============================================================
;;; sl-signal-binary-lambda tests
;;;============================================================
(def-suite* sl-signal-binary-lambda-test
  :in sl-signal-test)

(test lambda-basic-connect-comb
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((input (make-instance 'sl-signal-binary :bit-width 8))
	  (output (make-instance 'sl-signal-binary-lambda :bit-width 8)))
      (connect-comb output
		    (lambda () (sl+ (value input) 1))
		    (vector input))
      (setb input 10)
      (is (sl= 11 (value output))))))

(test lambda-two-inputs
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((a (make-instance 'sl-signal-binary :bit-width 8))
	  (b (make-instance 'sl-signal-binary :bit-width 8))
	  (sum (make-instance 'sl-signal-binary-lambda :bit-width 9)))
      (connect-comb sum
		    (lambda () (sl+ (value a) (value b)))
		    (vector a b))
      (setb a 100)
      (is (sl= 100 (value sum)))
      (setb b 55)
      (is (sl= 155 (value sum))))))

(test lambda-logical-and
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((a (make-instance 'sl-signal-binary :bit-width 8))
	  (b (make-instance 'sl-signal-binary :bit-width 8))
	  (result (make-instance 'sl-signal-binary-lambda :bit-width 8)))
      (connect-comb result
		    (lambda () (sl-logand (value a) (value b)))
		    (vector a b))
      (setb a #xFF)
      (setb b #x0F)
      (is (sl= #x0F (value result))))))

(test lambda-mux-2to1
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((sel (make-instance 'sl-signal-binary :bit-width 1))
	  (in0 (make-instance 'sl-signal-binary :bit-width 8))
	  (in1 (make-instance 'sl-signal-binary :bit-width 8))
	  (out (make-instance 'sl-signal-binary-lambda :bit-width 8)))
      (connect-comb out
		    (lambda ()
		      (if (sl= 0 (value sel))
			  (value in0)
			  (value in1)))
		    (vector sel in0 in1))
      (setb in0 10)
      (setb in1 20)
      (setb sel 0)
      (is (sl= 10 (value out)))
      (setb sel 1)
      (is (sl= 20 (value out))))))

(test lambda-chain
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((input   (make-instance 'sl-signal-binary :bit-width 8))
	  (doubled (make-instance 'sl-signal-binary-lambda :bit-width 9))
	  (result  (make-instance 'sl-signal-binary-lambda :bit-width 9)))
      (connect-comb doubled
		    (lambda () (lsh (value input) 1))
		    (vector input))
      (connect-comb result
		    (lambda () (sl+ (value doubled) 1))
		    (vector doubled))
      (setb input 5)
      (is (sl= 10 (value doubled)))
      (is (sl= 11 (value result)))
      (setb input 20)
      (is (sl= 40 (value doubled)))
      (is (sl= 41 (value result))))))

(test lambda-drives-registered-load
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((input  (make-instance 'sl-signal-binary :bit-width 8))
	  (lambda-sig (make-instance 'sl-signal-binary-lambda :bit-width 8))
	  (reg-load   (make-instance 'sl-signal-binary :bit-width 8)))
      (connect-comb lambda-sig
		    (lambda () (sl-lognot (value input)))
		    (vector input))
      (connect-driver-load lambda-sig reg-load)
      (setb input 0)
      (is (sl= 255 (value lambda-sig)))
      (is (sl= 255 (value reg-load)))
      (setb input #xAA)
      (is (sl= #x55 (value lambda-sig)))
      (is (sl= #x55 (value reg-load))))))

(test lambda-network-reg-lambda-lambda-reg
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((driver (make-instance 'sl-signal-binary :bit-width 8))
	  (lam-a  (make-instance 'sl-signal-binary-lambda :bit-width 8))
	  (lam-b  (make-instance 'sl-signal-binary-lambda :bit-width 8))
	  (output (make-instance 'sl-signal-binary :bit-width 8)))
      (connect-comb lam-a
		    (lambda () (sl+ (value driver) 10))
		    (vector driver))
      (connect-comb lam-b
		    (lambda () (lsh (value lam-a) 1))
		    (vector lam-a))
      (connect-driver-load lam-b output)
      (setb driver 5)
      (is (sl= 15 (value lam-a)))
      (is (sl= 30 (value lam-b)))
      (is (sl= 30 (value output)))
      (setb driver 0)
      (is (sl= 10 (value lam-a)))
      (is (sl= 20 (value lam-b)))
      (is (sl= 20 (value output))))))

(test lambda-fanout-to-multiple-loads
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((input  (make-instance 'sl-signal-binary :bit-width 8))
	  (lambda-sig (make-instance 'sl-signal-binary-lambda :bit-width 8))
	  (load1 (make-instance 'sl-signal-binary :bit-width 8))
	  (load2 (make-instance 'sl-signal-binary :bit-width 8)))
      (connect-comb lambda-sig
		    (lambda () (sl+ (value input) 5))
		    (vector input))
      (connect-driver-load lambda-sig load1)
      (connect-driver-load lambda-sig load2)
      (setb input 10)
      (is (sl= 15 (value lambda-sig)))
      (is (sl= 15 (value load1)))
      (is (sl= 15 (value load2))))))

(test lambda-diamond-network
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let ((driver (make-instance 'sl-signal-binary :bit-width 8))
	  (lam-a  (make-instance 'sl-signal-binary-lambda :bit-width 8))
	  (lam-b  (make-instance 'sl-signal-binary-lambda :bit-width 8))
	  (lam-out (make-instance 'sl-signal-binary-lambda :bit-width 8)))
      (connect-comb lam-a
		    (lambda () (sl+ (value driver) 1))
		    (vector driver))
      (connect-comb lam-b
		    (lambda () (sl+ (value driver) 2))
		    (vector driver))
      (connect-comb lam-out
		    (lambda () (sl+ (value lam-a) (value lam-b)))
		    (vector lam-a lam-b))
      (setb driver 10)
      (is (sl= 11 (value lam-a)))
      (is (sl= 12 (value lam-b)))
      (is (sl= 23 (value lam-out))))))

;;;============================================================
;;; sl-signal-proxy tests
;;;============================================================
(def-suite* sl-signal-proxy-test
  :in sl-signal-test)

;;; Bit proxy

(test proxy-bit-write-single-bit
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((parent (make-instance 'sl-signal-binary :bit-width 8))
	   (bit0   (bit parent 0)))
      (setb parent 0)
      (setb bit0 1)
      (is (sl= 1 (value parent)))
      (setb bit0 0)
      (is (sl= 0 (value parent))))))

(test proxy-bit-write-high-bit
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((parent (make-instance 'sl-signal-binary :bit-width 8))
	   (bit7   (bit parent 7)))
      (setb parent 0)
      (setb bit7 1)
      (is (sl= 128 (value parent))))))

(test proxy-bit-preserves-other-bits
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((parent (make-instance 'sl-signal-binary :bit-width 8))
	   (bit4   (bit parent 4)))
      (setb parent #xAA)
      (setb bit4 1)
      (is (sl= #xBA (value parent))))))

(test proxy-bit-write-propagates-to-loads
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((parent (make-instance 'sl-signal-binary :bit-width 8))
	   (load   (make-instance 'sl-signal-binary :bit-width 8))
	   (bit0   (bit parent 0)))
      (connect-driver-load parent load)
      (setb parent 0)
      (setb bit0 1)
      (is (sl= 1 (value parent)))
      (is (sl= 1 (value load))))))

;;; Slice proxy

(test proxy-slice-write
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((parent (make-instance 'sl-signal-binary :bit-width 16))
	   (low-byte (slice parent 7 0)))
      (setb parent 0)
      (setb low-byte #xAB)
      (is (sl= #x00AB (value parent))))))

(test proxy-slice-write-high-byte
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((parent (make-instance 'sl-signal-binary :bit-width 16))
	   (high-byte (slice parent 15 8)))
      (setb parent 0)
      (setb high-byte #xCD)
      (is (sl= #xCD00 (value parent))))))

(test proxy-slice-preserves-other-bits
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((parent (make-instance 'sl-signal-binary :bit-width 16))
	   (low-byte (slice parent 7 0)))
      (setb parent #xFF00)
      (setb low-byte #x42)
      (is (sl= #xFF42 (value parent))))))

(test proxy-slice-write-propagates-to-loads
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((parent (make-instance 'sl-signal-binary :bit-width 16))
	   (load   (make-instance 'sl-signal-binary :bit-width 16))
	   (low-byte (slice parent 7 0)))
      (connect-driver-load parent load)
      (setb parent #xFF00)
      (setb low-byte #x42)
      (is (sl= #xFF42 (value parent)))
      (is (sl= #xFF42 (value load))))))

(test proxy-slice-middle-nibble
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((parent (make-instance 'sl-signal-binary :bit-width 16))
	   (nibble (slice parent 11 8)))
      (setb parent #xF0F0)
      (setb nibble #xA)
      (is (sl= #xFAF0 (value parent))))))

;;; Concat proxy

(test proxy-concat-write
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((sig-lo (make-instance 'sl-signal-binary :bit-width 8))
	   (sig-hi (make-instance 'sl-signal-binary :bit-width 8))
	   (cat    (signal-concat sig-lo sig-hi)))
      (setb cat #xBEEF)
      (is (sl= #xEF (value sig-lo)))
      (is (sl= #xBE (value sig-hi))))))

(test proxy-concat-write-propagates
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((sig-lo (make-instance 'sl-signal-binary :bit-width 8))
	   (sig-hi (make-instance 'sl-signal-binary :bit-width 8))
	   (load-lo (make-instance 'sl-signal-binary :bit-width 8))
	   (load-hi (make-instance 'sl-signal-binary :bit-width 8))
	   (cat     (signal-concat sig-lo sig-hi)))
      (connect-driver-load sig-lo load-lo)
      (connect-driver-load sig-hi load-hi)
      (setb cat #xCAFE)
      (is (sl= #xFE (value sig-lo)))
      (is (sl= #xCA (value sig-hi)))
      (is (sl= #xFE (value load-lo)))
      (is (sl= #xCA (value load-hi))))))

(test proxy-concat-three-signals
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((sig-a (make-instance 'sl-signal-binary :bit-width 4))
	   (sig-b (make-instance 'sl-signal-binary :bit-width 8))
	   (sig-c (make-instance 'sl-signal-binary :bit-width 4))
	   (cat   (signal-concat sig-a sig-b sig-c)))
      (setb cat #xABCD)
      (is (sl= #xD  (value sig-a)))
      (is (sl= #xBC (value sig-b)))
      (is (sl= #xA  (value sig-c))))))

;;; Proxy with connect-comb

(test proxy-slice-with-connect-comb
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((parent (make-instance 'sl-signal-binary :bit-width 16))
	   (source (make-instance 'sl-signal-binary :bit-width 8))
	   (low-byte (slice parent 7 0)))
      (setb parent 0)
      (connect-comb low-byte
		    (lambda () (value source))
		    (vector source))
      (setb source #x42)
      (is (sl= #x0042 (value parent))))))

(test proxy-slice-comb-preserves-other-bits
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((parent (make-instance 'sl-signal-binary :bit-width 16))
	   (source (make-instance 'sl-signal-binary :bit-width 8))
	   (low-byte (slice parent 7 0)))
      (setb parent #xFF00)
      (connect-comb low-byte
		    (lambda () (value source))
		    (vector source))
      (setb source #x42)
      (is (sl= #xFF42 (value parent))))))

(test proxy-bit-with-connect-comb
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((parent (make-instance 'sl-signal-binary :bit-width 8))
	   (source (make-instance 'sl-signal-binary :bit-width 1))
	   (bit3   (bit parent 3)))
      (setb parent 0)
      (connect-comb bit3
		    (lambda () (value source))
		    (vector source))
      (setb source 1)
      (is (sl= 8 (value parent)))
      (setb source 0)
      (is (sl= 0 (value parent))))))

(test proxy-two-slices-independent
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((parent (make-instance 'sl-signal-binary :bit-width 16))
	   (src-lo (make-instance 'sl-signal-binary :bit-width 8))
	   (src-hi (make-instance 'sl-signal-binary :bit-width 8))
	   (lo-slice (slice parent 7 0))
	   (hi-slice (slice parent 15 8)))
      (setb parent 0)
      (connect-comb lo-slice
		    (lambda () (value src-lo))
		    (vector src-lo))
      (connect-comb hi-slice
		    (lambda () (value src-hi))
		    (vector src-hi))
      (setb src-lo #xEF)
      (is (sl= #x00EF (value parent)))
      (setb src-hi #xBE)
      (is (sl= #xBEEF (value parent))))))

;;; Proxy in larger networks

(test proxy-slice-lambda-chain
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((source    (make-instance 'sl-signal-binary :bit-width 8))
	   (lambda-sig (make-instance 'sl-signal-binary-lambda :bit-width 8))
	   (parent    (make-instance 'sl-signal-binary :bit-width 16))
	   (load      (make-instance 'sl-signal-binary :bit-width 16))
	   (lo-slice  (slice parent 7 0)))
      (connect-comb lambda-sig
		    (lambda () (sl+ (value source) 1))
		    (vector source))
      (connect-comb lo-slice
		    (lambda () (value lambda-sig))
		    (vector lambda-sig))
      (connect-driver-load parent load)
      (setb parent #xFF00)
      (setb source 9)
      (is (sl= 10 (value lambda-sig)))
      (is (sl= #xFF0A (value parent)))
      (is (sl= #xFF0A (value load))))))

(test proxy-concat-with-connect-comb
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((sig-lo (make-instance 'sl-signal-binary :bit-width 8))
	   (sig-hi (make-instance 'sl-signal-binary :bit-width 8))
	   (source (make-instance 'sl-signal-binary :bit-width 16))
	   (cat    (signal-concat sig-lo sig-hi)))
      (connect-comb cat
		    (lambda () (value source))
		    (vector source))
      (setb source #xDEAD)
      (is (sl= #xAD (value sig-lo)))
      (is (sl= #xDE (value sig-hi))))))

(test proxy-bit-proxy-propagation-chain
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((parent (make-instance 'sl-signal-binary :bit-width 8))
	   (load1  (make-instance 'sl-signal-binary :bit-width 8))
	   (load2  (make-instance 'sl-signal-binary :bit-width 8))
	   (bit0   (bit parent 0))
	   (bit7   (bit parent 7)))
      (connect-driver-load parent load1)
      (connect-driver-load parent load2)
      (setb parent 0)
      (setb bit0 1)
      (is (sl= 1 (value parent)))
      (is (sl= 1 (value load1)))
      (is (sl= 1 (value load2)))
      (setb bit7 1)
      (is (sl= #x81 (value parent)))
      (is (sl= #x81 (value load1)))
      (is (sl= #x81 (value load2))))))

;;; Signed signal proxies

(test proxy-slice-signed-parent
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((parent (make-instance 'sl-signal-binary :bit-width 16 :signed t))
	   (low-byte (slice parent 7 0)))
      (setb parent (int 0 :bits 16))
      (setb low-byte #x42)
      (is (sl= #x42 (value parent))))))

(test proxy-bit-signed-parent
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (let* ((parent (make-instance 'sl-signal-binary :bit-width 8 :signed t))
	   (bit7   (bit parent 7)))
      (setb parent (int 0 :bits 8))
      (setb bit7 1)
      (is (sl= -128 (value parent))))))
