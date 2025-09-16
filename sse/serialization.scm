(library (sse serialization)
  (export make-bytevector
          bytevector?
          bytevector-length
          string->utf8
          write-u8!    read-u8
          write-u16!   read-u16
          write-u32!   read-u32
          write-u64!   read-u64
          write-u128!  read-u128
          write-bytes! read-bytes)
  (import (sse utils)
          (rnrs base)
          (rnrs control)
          (rnrs bytevectors)
          (rnrs arithmetic bitwise))

  (define endianess% (endianness big))

  (define (write-u8! bytes pos val)
    (bytevector-u8-set! bytes pos val)
    (+ pos 1))

  (define (write-u16! bytes pos val)
    (bytevector-u16-set! bytes pos val endianess%)
    (+ pos 2))

  (define (write-u32! bytes pos val)
    (bytevector-u32-set! bytes pos val endianess%)
    (+ pos 4))

  (define (write-u64! bytes pos val)
    (bytevector-u64-set! bytes pos val endianess%)
    (+ pos 8))

  (define (write-u128! bytes pos val)
    (let ((v1 (>> val 64))
          (v2 (&& val (- (<< 1 64) 1))))
      (write-u64! bytes (+ pos 0) v1)
      (write-u64! bytes (+ pos 8) v2)
      (+ pos 16)))

  (define (read-u8 bytes pos)
    (values (bytevector-u8-ref bytes pos)
            (+ pos 1)))

  (define (read-u16 bytes pos)
    (values (bytevector-u16-ref bytes pos endianess%)
            (+ pos 2)))

  (define (read-u32 bytes pos)
    (values (bytevector-u32-ref bytes pos endianess%)
            (+ pos 4)))

  (define (read-u64 bytes pos)
    (values (bytevector-u64-ref bytes pos endianess%)
            (+ pos 8)))

  (define (read-u128 bytes pos)
    (let ((v1 (read-u64 bytes (+ pos 0)))
          (v2 (read-u64 bytes (+ pos 8))))
      (values (+ (<< v1 64) v2)
              (+ pos 16))))

  (define write-bytes!
    (case-lambda
      ((dest pos src) (write-bytes! dest pos src 0))
      ((dest pos src pos*)
       (let ((len (- (bytevector-length src) pos*)))
         (bytevector-copy! src pos* dest pos len)
         (+ pos len)))))


  (define read-bytes
    (case-lambda
      ((src start) (read-bytes src start (bytevector-length src)))
      ((src start stop)
       (let* ((len  (- stop start))
              (dest (make-bytevector len)))
         (bytevector-copy! src start dest 0 len)
         (values dest stop))))))
