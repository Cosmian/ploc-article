(library (sse crypto)
  (export rng sha3
          aes-gcm-256:init
          aes-gcm-256:keygen
          aes-gcm-256:encrypt
          aes-gcm-256:decrypt)
  (import (rnrs base) (gnutls) (sse serialization))

  (define (rng n) (gnutls-random random-level/key n))

  (define (sha3 bytes) (hash-direct digest/sha3-256 bytes))

  (define nonce-length 12)

  (define associated-data (make-bytevector 0))

  (define (aes-gcm-256:keygen) (rng 32))

  (define (aes-gcm-256:init key)
    (make-aead-cipher cipher/aes-256-gcm key))

  (define (aes-gcm-256:encrypt cipher ptx)
    (let* ((nonce  (rng nonce-length))
           (bytes  (aead-cipher-encrypt
                    cipher nonce associated-data 0 ptx)))
      (let ((ctx (make-bytevector (+ nonce-length (bytevector-length bytes)))))
        (write-bytes! ctx 0 nonce)
        (write-bytes! ctx nonce-length bytes)
        ctx)))

  (define (aes-gcm-256:decrypt cipher ctx)
    (let ((nonce  (read-bytes ctx 0 nonce-length))
          (bytes  (read-bytes ctx nonce-length)))
      (aead-cipher-decrypt cipher nonce associated-data 0 bytes))))
