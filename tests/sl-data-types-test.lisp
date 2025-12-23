;;;(in-package :system-lisp-test)

(def-suite* sl-data-types-test
  :description "Tests for the SL data types"
  :in sl-main-test)

(in-suite sl-data-types-test)

;;; Tests for to-uint
(test to-uint-basic
  "Basic tests for to-uint"
  ;; 8-bit unsigned integers
  (is (= 0 (to-uint 0 8)))
  (is (= 42 (to-uint 42 8)))
  (is (= 255 (to-uint 255 8)))
  (is (= 255 (to-uint 511 8))) ; 511 = 256 + 255, should be masked to 255
  (is (= 23 (to-uint -233 8))) ; Wrapping negative value
  
  ;; 4-bit unsigned integers
  (is (= 5 (to-uint 5 4)))
  (is (= 15 (to-uint 15 4)))
  (is (= 7 (to-uint 23 4)))       ; 23 = 16 + 7, should be masked to 7
  
  ;; Edge cases
  (is (= 0 (to-uint 0 1)))	    ; 1-bit can only represent 0
  (is (= 1 (to-uint 1 1)))	    ; 1-bit can only represent 0 or 1
  (is (= 0 (to-uint 2 1)))	    ; 2 should be masked to 0 in 1-bit
  (is (= 0 (to-uint -1 0)))	    ; 0-bit should always return 0
  (is (= (1- (ash 1 32)) (to-uint -1 32))))  ; All 32 bits set

;;; Tests for to-int
(test to-int-basic
  "Basic tests for to-int"
  ;; 8-bit signed integers (-128 to 127)
  (is (= 0 (to-int 0 8)))
  (is (= 42 (to-int 42 8)))
  (is (= 127 (to-int 127 8)))
  (is (= -128 (to-int 128 8)))    ; Highest bit set = negative value
  (is (= -1 (to-int 255 8)))      ; All bits set = -1
  (is (= 42 (to-int 298 8)))      ; 298 = 256 + 42, should be masked to 42
  (is (= -42 (to-int -42 8)))     ; Proper negative conversion
  
  ;; 4-bit signed integers (-8 to 7)
  (is (= 5 (to-int 5 4)))
  (is (= 7 (to-int 7 4)))
  (is (= -8 (to-int 8 4)))        ; Highest bit set = negative value
  (is (= -1 (to-int 15 4)))       ; All bits set = -1
  (is (= 5 (to-int 21 4)))        ; 21 = 16 + 5, should be masked to 5
  
  ;; Edge cases
  (is (= 0 (to-int 0 1)))         ; 1-bit signed can only represent 0 or -1
  (is (= -1 (to-int 1 1)))        ; In 1-bit signed, 1 represents -1
  (is (= 0 (to-int 2 1)))         ; 2 should be masked to 0 in 1-bit
  (is (= -1 (to-int -1 32)))      ; -1 in 32-bit stays -1
  (is (= (- (ash 1 31)) (to-int (ash 1 31) 32)))) ; Minimum 32-bit value

;;; Test cases for positive/negative boundaries
(test to-uint-int-boundary-cases
  "Test boundary cases for both functions"
  ;; 8-bit
  (is (= 127 (to-int 127 8)))     ; Max positive 8-bit
  (is (= -128 (to-int 128 8)))    ; Min negative 8-bit
  (is (= -128 (to-int -128 8)))   ; Min negative 8-bit as input
  (is (= -128 (to-int (+ 256 -128) 8))) ; Wrapped around
  
  ;; 16-bit
  (is (= 32767 (to-int 32767 16)))     ; Max positive 16-bit
  (is (= -32768 (to-int 32768 16)))    ; Min negative 16-bit
  (is (= -32768 (to-int -32768 16)))   ; Min negative 16-bit as input
  (is (= 0 (to-int 65536 16)))         ; Full wrap-around to 0
  
  ;; Ensure wrapping works correctly
  (is (= 1 (to-uint (+ (ash 1 8) 1) 8)))  ; 257 should wrap to 1 in 8-bit unsigned
  (is (= 1 (to-int (+ (ash 1 8) 1) 8))))  ; 257 should wrap to 1 in 8-bit signed

;;; Unit tests for fixed-width integer reader macros
;;; Helper function to check sl-uint equality
(defun uint-equal-p (a b)
  (and (= (sl-uint-bits a) (sl-uint-bits b))
       (= (sl-uint-value a) (sl-uint-value b))))

;;; Helper function to check sl-int equality
(defun int-equal-p (a b)
  (and (= (sl-int-bits a) (sl-int-bits b))
       (= (sl-int-value a) (sl-int-value b))))

;;; Test unsigned integer reader macro with different bases
(test unsigned-int-base-formats
  "Test #u reader macro with different number bases"
  ;; Binary format
  (is (uint-equal-p #u8b101 (make-sl-uint 8 5))
      "Binary unsigned 8-bit integer")
  (is (uint-equal-p #u16b1010_1010 (make-sl-uint 16 170))
      "Binary unsigned 16-bit integer with underscore separator")
  
  ;; Octal format
  (is (uint-equal-p #u8o17 (make-sl-uint 8 15))
      "Octal unsigned 8-bit integer")
  (is (uint-equal-p #u16o127 (make-sl-uint 16 87))
      "Octal unsigned 16-bit integer")
  
  ;; Decimal format
  (is (uint-equal-p #u8d42 (make-sl-uint 8 42))
      "Decimal unsigned 8-bit integer")
  (is (uint-equal-p #u16d1000 (make-sl-uint 16 1000))
      "Decimal unsigned 16-bit integer")
  
  ;; Hexadecimal format
  (is (uint-equal-p #u8hFF (make-sl-uint 8 255))
      "Hexadecimal unsigned 8-bit integer (uppercase H)")
  (is (uint-equal-p #u16x1a3f (make-sl-uint 16 6719))
      "Hexadecimal unsigned 16-bit integer (lowercase X)")
  (is (uint-equal-p #u32xdead_beef (make-sl-uint 32 3735928559))
      "Hexadecimal unsigned 32-bit integer with underscore separator"))

;;; Test signed integer reader macro with different bases
(test signed-int-base-formats
  "Test #i reader macro with different number bases"
  ;; Binary format
  (is (int-equal-p #i8b+101 (make-sl-int 8 5))
      "Binary signed 8-bit positive integer")
  (is (int-equal-p #i8b-101 (make-sl-int 8 -5))
      "Binary signed 8-bit negative integer")
  
  ;; Octal format
  (is (int-equal-p #i8o+17 (make-sl-int 8 15))
      "Octal signed 8-bit positive integer")
  (is (int-equal-p #i16o-127 (make-sl-int 16 -87))
      "Octal signed 16-bit negative integer")
  
  ;; Decimal format
  (is (int-equal-p #i8d+42 (make-sl-int 8 42))
      "Decimal signed 8-bit positive integer")
  (is (int-equal-p #i16d-1000 (make-sl-int 16 -1000))
      "Decimal signed 16-bit negative integer")
  
  ;; Hexadecimal format
  (is (int-equal-p #i8h+7F (make-sl-int 8 127))
      "Hexadecimal signed 8-bit positive integer")
  (is (int-equal-p #i16x-1a3f (make-sl-int 16 -6719))
      "Hexadecimal signed 16-bit negative integer"))

;;; Test edge cases for bit width limits
(test bit-width-limits
  "Test behavior at bit width limits"
  ;; 8-bit limits
  (is (= (sl-uint-value #u8d255) 255)
      "8-bit unsigned maximum (255)")
  (is (= (sl-uint-value #u8d256) 0)
      "8-bit unsigned overflow wraps to 0")
  
  (is (= (sl-int-value #i8d127) 127)
      "8-bit signed maximum (127)")
  (is (= (sl-int-value #i8d128) -128)
      "8-bit signed overflow wraps to -128")
  (is (= (sl-int-value #i8d-128) -128)
      "8-bit signed minimum (-128)")
  (is (= (sl-int-value #i8d-129) 127)
      "8-bit signed underflow wraps to 127")
  
  ;; 16-bit limits
  (is (= (sl-uint-value #u16d65535) 65535)
      "16-bit unsigned maximum (65535)")
  (is (= (sl-uint-value #u16d65536) 0)
      "16-bit unsigned overflow wraps to 0")
  
  (is (= (sl-int-value #i16d32767) 32767)
      "16-bit signed maximum (32767)")
  (is (= (sl-int-value #i16d32768) -32768)
      "16-bit signed overflow wraps to -32768")
  (is (= (sl-int-value #i16d-32768) -32768)
      "16-bit signed minimum (-32768)")
  (is (= (sl-int-value #i16d-32769) 32767)
      "16-bit signed underflow wraps to 32767"))

;;; Test for implicit sign handling
(test implicit-sign-handling
  "Test implicit sign handling (no + or - provided)"
  (is (int-equal-p #i8d42 (make-sl-int 8 42))
      "Implicit positive sign for decimal")
  (is (int-equal-p #i16xf0 (make-sl-int 16 240))
      "Implicit positive sign for hex"))

;;; Test underscore separators in different number formats
(test underscore-separators
  "Test underscore separators in different number formats"
  (is (uint-equal-p #u32d1_000_000 (make-sl-uint 32 1000000))
      "Decimal with underscore separators")
  (is (uint-equal-p #u16b1010_0101_1100 (make-sl-uint 16 2652))
      "Binary with underscore separators")
  (is (uint-equal-p #u32x1234_abcd (make-sl-uint 32 305441741))
      "Hex with underscore separators"))

;;; Test case insensitivity of radix specifiers
(test radix-specifier-case-insensitivity
  "Test case insensitivity of radix specifiers"
  (is (uint-equal-p #u16b1111 (make-sl-uint 16 15))
      "Lowercase 'b' for binary")
  (is (uint-equal-p #u16B1111 (make-sl-uint 16 15))
      "Uppercase 'B' for binary")
  
  (is (uint-equal-p #u16o177 (make-sl-uint 16 127))
      "Lowercase 'o' for octal")
  (is (uint-equal-p #u16O177 (make-sl-uint 16 127))
      "Uppercase 'O' for octal")
  
  (is (uint-equal-p #u16d100 (make-sl-uint 16 100))
      "Lowercase 'd' for decimal")
  (is (uint-equal-p #u16D100 (make-sl-uint 16 100))
      "Uppercase 'D' for decimal")
  
  (is (uint-equal-p #u16xff (make-sl-uint 16 255))
      "Lowercase 'x' for hex")
  (is (uint-equal-p #u16XFF (make-sl-uint 16 255))
      "Uppercase 'X' for hex")
  
  (is (uint-equal-p #u16hff (make-sl-uint 16 255))
      "Lowercase 'h' for hex")
  (is (uint-equal-p #u16HFF (make-sl-uint 16 255))
      "Uppercase 'H' for hex"))

;;; Test case insensitivity of hex digits
(test hex-digit-case-insensitivity
  "Test case insensitivity of hex digits"
  (is (uint-equal-p #u16xabcd (make-sl-uint 16 43981))
      "Lowercase hex digits")
  (is (uint-equal-p #u16xABCD (make-sl-uint 16 43981))
      "Uppercase hex digits")
  (is (uint-equal-p #u16xaBcD (make-sl-uint 16 43981))
      "Mixed case hex digits"))

;;; Test non-standard bit widths
(test non-standard-bit-widths
  "Test non-standard bit widths"
  (is (= (sl-uint-bits #u7d100) 7)
      "7-bit width unsigned")
  (is (= (sl-uint-value #u7d100) 100)
      "7-bit width unsigned value")
  
  (is (= (sl-int-bits #i13d-1000) 13)
      "13-bit width signed")
  (is (= (sl-int-value #i13d-1000) -1000)
      "13-bit width signed value"))

;;; Tests for uint-bit-get
(test uint-bit-get-tests
  "Test the uint-bit-get function"
  ;; Basic bit retrieval
  (is (= 0 (uint-bit-get 0 0 8)))	; Bit 0 of 0 is 0
  (is (= 1 (uint-bit-get 0 1 8)))	; Bit 0 of 1 is 1
  (is (= 0 (uint-bit-get 1 1 8)))	; Bit 1 of 1 is 0
  (is (= 1 (uint-bit-get 1 2 8)))	; Bit 1 of 2 is 1
  (is (= 1 (uint-bit-get 1 3 8)))	; Bit 1 of 3 is 1
  (is (= 1 (uint-bit-get 2 4 8)))	; Bit 2 of 4 is 1
  (is (= 1 (uint-bit-get 3 8 8)))	; Bit 3 of 8 is 1
  (is (= 1 (uint-bit-get 7 128 8)))	; Bit 7 of 128 is 1
  
  ;; Edge cases
  (is (= 0 (uint-bit-get 8 255 8)))	; Bit position outside range
  (is (= 0 (uint-bit-get 15 32767 8))) ; Bit position outside range, value also outside
  (is (= 0 (uint-bit-get 0 0 8))) ; Works for any bit value b in position 0
  (is (= 1 (uint-bit-get 0 1 8))) ; Works for any bit value b in position 0
  
  ;; With masking
  (is (= 1 (uint-bit-get 0 257 8)))  ; Bit 0 of 257 (masked to 1) is 1
  (is (= 0 (uint-bit-get 1 257 8)))  ; Bit 1 of 257 (masked to 1) is 0
  (is (= 0 (uint-bit-get 8 257 8)))  ; Bit 8 (outside range) is 0
  
  ;; Negative numbers
  (is (= 1 (uint-bit-get 0 -1 8)))  ; Bit 0 of -1 (masked to 255) is 1
  (is (= 1 (uint-bit-get 7 -1 8)))  ; Bit 7 of -1 (masked to 255) is 1
  (is (= 0 (uint-bit-get 8 -1 8))))      ; Bit 8 (outside range) is 0

;;; Tests for int-bit-get
(test int-bit-get-tests
  "Test the int-bit-get function"
  ;; Basic bit retrieval
  (is (= 0 (int-bit-get 0 0 8)))         ; Bit 0 of 0 is 0
  (is (= 1 (int-bit-get 0 1 8)))         ; Bit 0 of 1 is 1
  (is (= 0 (int-bit-get 1 1 8)))         ; Bit 1 of 1 is 0
  (is (= 1 (int-bit-get 1 2 8)))         ; Bit 1 of 2 is 1
  (is (= 1 (int-bit-get 7 -1 8)))        ; Bit 7 of -1 is 1
  (is (= 1 (int-bit-get 7 -128 8)))      ; Bit 7 of -128 is 1
  
  ;; Sign bit extension
  (is (= 1 (int-bit-get 8 -1 8)))        ; Outside bit of negative number returns sign bit (1)
  (is (= 1 (int-bit-get 100 -1 8)))      ; Far outside bit of negative number returns sign bit (1)
  (is (= 0 (int-bit-get 8 127 8)))       ; Outside bit of positive number returns sign bit (0)
  
  ;; With masking
  (is (= 1 (int-bit-get 0 257 8)))       ; Bit 0 of 257 (masked to 1) is 1
  (is (= 0 (int-bit-get 7 257 8)))       ; Bit 7 (sign bit) of 257 (masked to 1) is 0
  
  ;; Negative numbers within range
  (is (= 0 (int-bit-get 0 -2 8)))        ; Bit 0 of -2 is 0
  (is (= 1 (int-bit-get 1 -2 8)))        ; Bit 1 of -2 is 1
  (is (= 1 (int-bit-get 7 -2 8))))       ; Bit 7 (sign bit) of -2 is 1

;;; Tests for struct method bit
(test struct-bit-get
  "Test bit method on sl-uint and sl-int structs"
  (let ((uint8 (uint 170 :bits 8))
        (int8-pos (int 42 :bits 8))
        (int8-neg (int -42 :bits 8)))
    
    ;; Test bit method on sl-uint
    (is (= (bit uint8 0) 0) "Bit 0 of 170 should be 0")
    (is (= (bit uint8 1) 1) "Bit 1 of 170 should be 1")
    (is (= (bit uint8 7) 1) "Bit 7 of 170 should be 1")
    (is (= (bit uint8 8) 0) "Bit 8 is out of range for 8-bit uint")
    
    ;; Test bit method on sl-int (positive)
    (is (= (bit int8-pos 0) 0) "Bit 0 of 42 should be 0")
    (is (= (bit int8-pos 1) 1) "Bit 1 of 42 should be 1")
    (is (= (bit int8-pos 7) 0) "Bit 7 of 42 (sign bit) should be 0")
    (is (= (bit int8-pos 8) 0) "Bit 8 is out of range, returns sign bit for positive")
    
    ;; Test bit method on sl-int (negative)
    (is (= (bit int8-neg 0) 0) "Bit 0 of -42 should be 0")
    (is (= (bit int8-neg 1) 1) "Bit 1 of -42 should be 1")
    (is (= (bit int8-neg 5) 0) "Bit 5 of -42 should be 0")
    (is (= (bit int8-neg 7) 1) "Bit 7 of -42 (sign bit) should be 1")
    (is (= (bit int8-neg 8) 1) "Bit 8 is out of range, returns sign bit for negative")))

;;; Tests for bit-vector method
(test bit-vector-bit-get
  "Test bit method on bit-vector"
  (let ((bv (make-array 8 :element-type 'cl:bit :initial-contents '(1 0 1 0 1 1 0 1))))
    (is (= (cl:bit bv 0) 1) "Bit 0 of bit-vector should be 1")
    (is (= (cl:bit bv 1) 0) "Bit 1 of bit-vector should be 0")
    (is (= (cl:bit bv 7) 1) "Bit 7 of bit-vector should be 1")))

;;; Tests for uint-bit-set
(test uint-bit-set-tests
  "Test the uint-bit-set function"
  ;; Basic bit setting (default is to set to 1)
  (is (= 1 (uint-bit-set 0 0 8)))	; Set bit 0 of 0 to 1 -> 1
  (is (= 3 (uint-bit-set 1 1 8)))	; Set bit 1 of 1 to 1 -> 3
  (is (= 5 (uint-bit-set 2 1 8)))	; Set bit 2 of 1 to 1 -> 5
  (is (= 129 (uint-bit-set 7 1 8)))	; Set bit 7 of 1 to 1 -> 129
  
  ;; Explicit setting to 1
  (is (= 1 (uint-bit-set 0 0 8 t)))	; Set bit 0 of 0 to 1 -> 1
  (is (= 3 (uint-bit-set 1 1 8 t)))	; Set bit 1 of 1 to 1 -> 3
  
  ;; Setting to 0
  (is (= 0 (uint-bit-set 0 1 8 nil)))	  ; Set bit 0 of 1 to 0 -> 0
  (is (= 1 (uint-bit-set 1 3 8 nil)))	  ; Set bit 1 of 3 to 0 -> 1
  (is (= 127 (uint-bit-set 7 255 8 nil))) ; Set bit 7 of 255 to 0 -> 127
  
  ;; Out of range
  (is (= 42 (uint-bit-set 8 42 8))) ; Bit 8 out of range, return unchanged
  (is (= 42 (uint-bit-set 100 42 8))) ; Bit 100 out of range, return unchanged
  
  ;; With masking
  (is (= 1 (uint-bit-set 0 257 8))) ; Set bit 0 of 257 (masked to 1) to 1 -> unchanged (1)
  (is (= 3 (uint-bit-set 1 257 8))) ; Set bit 1 of 257 (masked to 1) to 1 -> 3
  
  ;; Negative numbers
  (is (= 254 (uint-bit-set 0 -1 8 nil)))) ; Set bit 0 of -1 (masked to 255) to 0 -> 254

;;; Tests for int-bit-set
(test int-bit-set-tests
  "Test the int-bit-set function"
  ;; Basic bit setting (default is to set to 1)
  (is (= 1 (int-bit-set 0 0 8)))         ; Set bit 0 of 0 to 1 -> 1
  (is (= 3 (int-bit-set 1 1 8)))         ; Set bit 1 of 1 to 1 -> 3
  (is (= 5 (int-bit-set 2 1 8)))         ; Set bit 2 of 1 to 1 -> 5
  
  ;; Sign bit manipulation
  (is (= -127 (int-bit-set 7 1 8)))      ; Set bit 7 of 1 to 1 -> -127 (negative)
  (is (= 127 (int-bit-set 7 -1 8 nil)))  ; Set bit 7 of -1 to 0 -> 127 (positive)
  
  ;; Setting to 0
  (is (= 0 (int-bit-set 0 1 8 nil)))     ; Set bit 0 of 1 to 0 -> 0
  (is (= 1 (int-bit-set 1 3 8 nil)))     ; Set bit 1 of 3 to 0 -> 1
  
  ;; Out of range
  (is (= 42 (int-bit-set 8 42 8)))       ; Bit 8 out of range, return unchanged
  (is (= -42 (int-bit-set 8 -42 8)))     ; Bit 8 out of range, return unchanged
  
  ;; With masking
  (is (= 1 (int-bit-set 0 257 8)))       ; Set bit 0 of 257 (masked to 1) to 1 -> unchanged (1)
  (is (= 3 (int-bit-set 1 257 8)))       ; Set bit 1 of 257 (masked to 1) to 1 -> 3
  
  ;; Other negative number cases
  (is (= -2 (int-bit-set 0 -1 8 nil)))   ; Set bit 0 of -1 to 0 -> -2
  (is (= -5 (int-bit-set 2 -1 8 nil)))   ; Set bit 2 of -1 to 0 -> -5
  (is (= -1 (int-bit-set 7 127 8)))      ; Set sign bit of 127 to 1 -> -1
  (is (= -128 (int-bit-set 7 0 8))))     ; Set sign bit of 0 to 1 -> -128

;;; Tests for struct method (setf bit)
(test struct-bit-set
  "Test (setf bit) method on sl-uint and sl-int structs"
  (let ((uint8 (uint 170 :bits 8))
        (int8-pos (int 42 :bits 8))
        (int8-neg (int -42 :bits 8)))
    
    ;; Test (setf bit) on sl-uint
    (setf (bit uint8 0) 1)
    (is (= (sl-uint-value uint8) 171) "Setting bit 0 of 170 to 1 should be 171")
    
    (setf (bit uint8 1) 0)
    (is (= (sl-uint-value uint8) 169) "Setting bit 1 of 171 to 0 should be 169")
    
    (setf (bit uint8 8) 1)
    (is (= (sl-uint-value uint8) 169) "Setting bit 8 (out of range) should not change value")
    
    ;; Test (setf bit) on sl-int (positive)
    (setf (bit int8-pos 0) 1)
    (is (= (sl-int-value int8-pos) 43) "Setting bit 0 of 42 to 1 should be 43")
    
    (setf (bit int8-pos 7) 1)
    (is (= (sl-int-value int8-pos) -85) "Setting bit 7 (sign bit) of 43 to 1 should make it negative")
    
    ;; Test (setf bit) on sl-int (negative)
    (setf (bit int8-neg 0) 1)
    (is (= (sl-int-value int8-neg) -41) "Setting bit 0 of -42 to 1 should be -41")
    
    (setf (bit int8-neg 7) 0)
    (is (= (sl-int-value int8-neg) 87) "Setting bit 7 (sign bit) of -41 to 0 should make it positive")))

;;; Tests for non-binary values in (setf bit)
(test non-binary-bit-set
  "Test (setf bit) with non-binary values"
  (let ((uint8 (uint 170 :bits 8))
        (int8 (int 42 :bits 8)))
    
    ;; Non-zero values treated as 1
    (setf (bit uint8 0) 5)
    (is (= (sl-uint-value uint8) 171) "Setting bit 0 with non-zero value should set bit to 1")
    
    (setf (bit int8 2) 42)
    (is (= (sl-int-value int8) 46) "Setting bit 3 with non-zero value should set bit to 1")
    
    ;; Zero treated as 0
    (setf (bit uint8 1) 0)
    (is (= (sl-uint-value uint8) 169) "Setting bit 1 with 0 should clear the bit")))

;;; Tests for edge cases
(test bit-operations-edge-cases
  "Test edge cases for bit operations"
  ;; Test with all bits set
  (let ((all-ones-uint (uint 255 :bits 8))
        (all-ones-int (int -1 :bits 8)))
    
    ;; All ones uint
    (is (= (bit all-ones-uint 0) 1) "Bit 0 of 255 should be 1")
    (is (= (bit all-ones-uint 7) 1) "Bit 7 of 255 should be 1")
    
    ;; All ones int (all 1s represents -1 in two's complement)
    (is (= (bit all-ones-int 0) 1) "Bit 0 of -1 should be 1")
    (is (= (bit all-ones-int 7) 1) "Bit 7 of -1 should be 1")
    
    ;; Clear a bit in all-ones
    (setf (bit all-ones-uint 3) 0)
    (is (= (sl-uint-value all-ones-uint) 247) "Clearing bit 3 of 255 should be 247")
    
    (setf (bit all-ones-int 3) 0)
    (is (= (sl-int-value all-ones-int) -9) "Clearing bit 3 of -1 should be -9"))
  
  ;; Test with all bits cleared
  (let ((all-zeros-uint (uint 0 :bits 8))
        (all-zeros-int (int 0 :bits 8)))
    
    ;; All zeros uint
    (is (= (bit all-zeros-uint 0) 0) "Bit 0 of 0 should be 0")
    (is (= (bit all-zeros-uint 7) 0) "Bit 7 of 0 should be 0")
    
    ;; All zeros int
    (is (= (bit all-zeros-int 0) 0) "Bit 0 of 0 should be 0")
    (is (= (bit all-zeros-int 7) 0) "Bit 7 of 0 should be 0")
    
    ;; Set a bit in all-zeros
    (setf (bit all-zeros-uint 3) 1)
    (is (= (sl-uint-value all-zeros-uint) 8) "Setting bit 3 of 0 should be a")
    
    (setf (bit all-zeros-int 3) 1)
    (is (= (sl-int-value all-zeros-int) 8) "Setting bit 3 of 0 should be 8")))

;;; Tests for uint-get-slice
(test uint-get-slice-tests
  "Test the uint-get-slice function"
  ;; Basic cases
  (is (= 1 (uint-get-slice 0 0 1 8)))	; Extract bit 0 from 1 -> 1
  (is (= 3 (uint-get-slice 1 0 7 8)))	; Extract bits 0-1 from 7 -> 3
  (is (= 2 (uint-get-slice 0 1 6 8))) ; Extract bits 0-1 from 6 (order doesn't matter) -> 2
  (is (= 7 (uint-get-slice 2 0 7 8))) ; Extract bits 0-2 from 7 -> 7
  (is (= 3 (uint-get-slice 3 2 12 8))) ; Extract bits 2-3 from 12 -> 3
  
  ;; Shifting correctly
  (is (= 1 (uint-get-slice 3 3 8 8))) ; Extract just bit 3 from 8 -> 1
  (is (= 3 (uint-get-slice 4 3 24 8))) ; Extract bits 3-4 from 24 -> 3
  
  ;; Masking correctly
  (is (= 3 (uint-get-slice 1 0 259 8)))	; Extract bits 0-1 from 259 (masked to 3) -> 3
  
  ;; Edge cases
  (is (= 0 (uint-get-slice 8 0 255 8)))	 ; Hi outside range -> 0
  (is (= 0 (uint-get-slice 7 8 255 8)))	 ; Lo outside range -> 0
  (is (= 0 (uint-get-slice 10 9 255 8))) ; Both outside range -> 0
  
  ;; Second return value tests for slice length
  (multiple-value-bind (val len) (uint-get-slice 5 2 42 8)
    (is (= 4 len))))                  ; Confirm shifting back works

;;; Tests for int-get-slice
(test int-get-slice-tests
  "Test the int-get-slice function"
  ;; Basic cases
  (is (= 1 (int-get-slice 0 0 1 8)))               ; Extract bit 0 from 1 -> 1
  (is (= 3 (int-get-slice 1 0 7 8)))               ; Extract bits 0-1 from 7 -> 3
  (is (= 2 (int-get-slice 0 1 6 8)))               ; Extract bits 0-1 from 6 (order doesn't matter) -> 2
  (is (= 7 (int-get-slice 2 0 7 8)))               ; Extract bits 0-2 from 7 -> 7
  (is (= 3 (int-get-slice 3 2 12 8)))              ; Extract bits 2-3 from 12 -> 3
  
  ;; Negative numbers
  (is (= 3 (int-get-slice 1 0 -1 8)))              ; Extract bits 0-1 from -1 -> 3
  (is (= 1 (int-get-slice 3 3 -8 8)))              ; Extract just bit 3 from -8 -> 1
  
  ;; Edge cases
  (is (= 0 (int-get-slice 8 0 -1 8)))              ; Hi outside range -> 0
  (is (= 0 (int-get-slice 7 8 -1 8)))              ; Lo outside range -> 0
  
  ;; Second return value tests for slice length
  (multiple-value-bind (val len) (int-get-slice 5 2 -42 8)
    (is (= 4 len))))                               ; Confirm slice length is calculated correctly

;;; Tests for uint-set-slice
(test uint-set-slice-tests
  "Test the uint-set-slice function"
  ;; Basic cases
  (is (= 1 (uint-set-slice 0 0 1 0 8)))            ; Set bit 0 to 1 in 0 -> 1
  (is (= 6 (uint-set-slice 2 1 3 0 8)))            ; Set bits 1-2 to 3 in 0 -> 6
  (is (= 6 (uint-set-slice 1 2 3 0 8)))            ; Set bits 1-2 to 3 in 0 (order doesn't matter) -> 6
  (is (= 5 (uint-set-slice 2 0 5 0 8)))            ; Set bits 0-2 to 5 in 0 -> 5
  (is (= 28 (uint-set-slice 4 2 7 0 8)))           ; Set bits 2-4 to 7 in 0 -> 28
  
  ;; Overwriting existing bits
  (is (= 1 (uint-set-slice 1 0 1 3 8)))            ; Set bits 0-1 to 1 in 3 -> 1
  (is (= 11 (uint-set-slice 3 2 2 3 8)))           ; Set bits 2-3 to 2 in 3 -> 11
  
  ;; Value masking
  (is (= 3 (uint-set-slice 1 0 7 0 8)))            ; Set bits 0-1 to 7 (masked to 3) in 0 -> 3
  
  ;; Edge cases
  (is (= 42 (uint-set-slice 8 0 255 42 8)))        ; Hi outside range -> unchanged
  (is (= 42 (uint-set-slice 7 8 255 42 8)))        ; Lo outside range -> unchanged
  (is (= 42 (uint-set-slice 10 9 255 42 8))))      ; Both outside range -> unchanged

;;; Tests for int-set-slice
(test int-set-slice-tests
  "Test the int-set-slice function"
  ;; Basic cases
  (is (= 1 (int-set-slice 0 0 1 0 8)))             ; Set bit 0 to 1 in 0 -> 1
  (is (= 6 (int-set-slice 2 1 3 0 8)))             ; Set bits 1-2 to 3 in 0 -> 6
  (is (= 6 (int-set-slice 1 2 3 0 8)))             ; Set bits 1-2 to 3 in 0 (order doesn't matter) -> 6
  (is (= 5 (int-set-slice 2 0 5 0 8)))             ; Set bits 0-2 to 5 in 0 -> 5
  (is (= 28 (int-set-slice 4 2 7 0 8)))            ; Set bits 2-4 to 7 in 0 -> 28
  
  ;; Affecting sign bit
  (is (= -128 (int-set-slice 7 7 1 0 8)))          ; Set bit 7 to 1 in 0 -> -128
  (is (= 127 (int-set-slice 7 7 0 -1 8)))          ; Set bit 7 to 0 in -1 -> 127
  (is (= -2 (int-set-slice 0 0 0 -1 8)))           ; Set bit 0 to 0 in -1 -> -2
  
  ;; With negative inputs
  (is (= -127 (int-set-slice 0 0 1 -128 8)))       ; Set bit 0 to 1 in -128 -> -127
  (is (= -124 (int-set-slice 2 1 2 -128 8)))       ; Set bits 1-2 to 2 in -128 -> -124
  
  ;; Edge cases
  (is (= 42 (int-set-slice 8 0 255 42 8)))         ; Hi outside range -> unchanged
  (is (= -42 (int-set-slice 7 8 255 -42 8)))       ; Lo outside range -> unchanged
  (is (= -42 (int-set-slice 10 9 255 -42 8))))     ; Both outside range -> unchanged

;;; Tests for potential bugs
(test bit-slice-bugs
  "Test for potential bugs in the slice functions"
  ;; Bug in int-get-slice using "cl:- lo" instead of "cl:- rlo"
  (is (= 3 (int-get-slice 1 0 7 8)))               ; Should correctly extract 3
  (is (= 3 (int-get-slice 0 1 7 8)))              ; Order flipped, should still extract 3
  
  ;; Bug in uint-set-slice/int-set-slice using lo instead of rlo
  (is (= 28 (uint-set-slice 4 2 7 0 8)))           ; Set bits 2-4 to 7 in 0 -> 28
  (is (= 28 (uint-set-slice 2 4 7 0 8)))           ; Order flipped, should still be 28
  (is (= 28 (int-set-slice 4 2 7 0 8)))            ; Set bits 2-4 to 7 in 0 -> 28
  (is (= 28 (int-set-slice 2 4 7 0 8))))           ; Order flipped, should still be 28

