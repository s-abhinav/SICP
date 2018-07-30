(define-module (custom-module stream)
  :export (cons-stream
	   stream-car
	   stream-cdr
	   stream-ref
	   stream-map
	   stream-enumerate-interval
	   add-streams
	   integers
	   ones
	   scale-stream
	   merge
	   ))

(define the-empty-stream '())

(define stream-null? null?)

(define-syntax
  cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

(define (stream-car stream) 
  (car stream))

(define (stream-cdr stream) 
  (force (cdr stream)))

(define false #f)
(define true #t)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car
                        argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr
                         argstreams))))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(define (add-streams s1 s2) 
  (stream-map + s1 s2))

(define integers 
  (cons-stream 1 (add-streams ones integers)))

(define ones (cons-stream 1 ones))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car (merge (stream-cdr s1) s2)))
		 ((> s1car s2car)
		  (cons-stream s2car (merge s1 (stream-cdr s2))))
		 (else
		  (cons-stream s1car
			       (merge (stream-cdr s1)
				      (stream-cdr s2)))))))))
