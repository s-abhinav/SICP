(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; In normal order evaluation, a procedure will evaluate its body first until only primitive procedures
;; are found in the body. In this case, gcd will evaluate its body. Then if is evaluated, but since it's
;; a primitive procedure, it will evaluate (= b 0) which is false. It will then evaluate
;; (gcd b (remainder a b)).
;; The process described above will repeat. This time, b is not 0, but (remainder a b). (remainder a b) gets evaluated for if.
;; The definition of remainder is:
;;
(define (remainder n d)
  (if (< n d)
      n
      (remainder (- n d) d)))

;; For this exercise, we have to evaluate (gcd 206 40). The gcd procedure will pass the same value to
;; remainder.
;; (remainder 206 40)
;; Again, in normal order evaluation, remainder will evaluate its body.
;; for (remainder 206 40), if will be evaluated.
(remainder (- n d) d))
;; if is a primitive procedure, hence its arguments get evaluated.
;; (if (< 206 40) is false, then the else case gets evaluated.
;; The process decribed above will get repeated but this time, the body of remainder gets evaluated again with the same process.
;; (if (< (- n d) d) which is, (if (< (- 206 40) 40) which is false
;; This was (remainder (- n d) d)
;; The next time it will become
(remainder (- (- n d) d) d)
;; The evaluation of this in the if becomes
(< (- (- n d) d) d)
;; which is
(< (- (- 206 40) 40) 40)
;; which is false
;; the next time, it will become
(remainder (- (- (- n d) d) d) d)
;; where if will evaluate
(< (- (- (- 206 40) 40) 40) 40)
;; which is again false
;; the next time, it will become
(remainder (- (- (- (- n d) d) d) d) d)
;; where if will evaluate
(< (- (- (- (- 206 40) 40) 40) 40) 40)
;; which is again false.
;; the next time, it will become
(remainder (- (- (- (- (- n d) d) d) d) d) d)
;; where if will evaluate
(< (- (- (- (- (- 206 40) 40) 40) 40) 40) 40) ; => 6 steps which is the ceiling value of 206 / 40
;; This time though, this becomes true
;; if will return (- (- (- (- (- 206 40) 40) 40) 40) 40) which evaluates to 6
;; It's essential to notice the pattern of evaluation of remainder. For every iteration
;; the normal order evaluation of remainder introduces (- n d) into n. It's easy to follow
;; with a string-replace in an editor. We can see that n gets expanded 5 times. This comes from the number of iteration it took 206 to be subtracted by 40 until it's not greater than 40 anymore.
;; To calculate the number of times this expansion occurs, we have to devide a by b.
;; Now that remainder returned a value, we get back to the second iteration in gcd
;; (gcd 206 40)
;; (if (= 6 0) is not true

;; a and b in gcd both undergo some transformation.
;; at any one point, a will get replaced with b and
;; b will get replaced with (remainder a b)

;; the rule of replacement that I've found to work is, replace all occurences of b with (remainder a b) then all a with b, but not the a inside (remainder a b)
;; 
(gcd b (remainder a b))
;; second pass
(gcd (remainder a b) (remainder b (remainder a b)))
;; third pass
(gcd (remainder b (remainder a b)) (remainder (remainder a b) (remainder b (remainder a b))))
;; fourth pass
(gcd
 (remainder (remainder a b) (remainder b (remainder a b)))
 (remainder (remainder b (remainder a b)) (remainder (remainder a b) (remainder b (remainder a b)))))
;; At this point, gcd will stop expanding because the second argument will be equal to 0, hence returning the first argument. Counting the occurrence of remainder results in 11.

;; For applicative order evaluation, the arguments will be evaluated before the body.
;; So, for (gcd 206 40), the else part will be evaluated
(gcd 40 (remainder 206 40))
;; at this point, (remainder 206 40) will be evaluated. The evaluation becomes
;; (gcd 40 6)
;; again, gcd will go into an iterative process
;; b is not 0 so it will be
;; (gcd 6 (remainder 40 6)) => (gcd 6 4)
;; again, 4 is not 0
;; (gcd 4 (remainder 6 4)) => (gcd 4 2)
;; (gcd 2 (remainder 4 2)) => (gcd 2 0)
;; b is 0, hence, the result is 2
;; gcd went into an iterative process and called remainder 4 times
