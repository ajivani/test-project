;;;; test-project.lisp

(in-package #:test-project)

;;; "test-project" goes here. Hacks and glory await!
;;; initialize git
;;; push it up to github
;;;https://www.youtube.com/watch?v=SPgjgybGb5o&list=PL2VAYZE_4wRIoHsU5cEBIxCYcbHzy4Ypj&index=2
;;(ql:quickload test-project)
;echo "# test-project" >> README.md
;git init
;git add README.md
;git commit -m "first commit"
;git remote add origin https://github.com/ajivani/test-project.git
;git push -u origin master


(defun hello ()
  ;(print (num-islands *grid*)))
  (print "hello"))

;;leetcode question islands 2d array find all islands
;;approach one have a list of all the places you've already been and mark the off
(defun num-islands (grid)
  (let ((count-islands 0)
	(unchecked-grid (make-array (array-dimensions grid) :initial-element nil)))
    (if (null grid)
	0
	(progn
	  (let ((rr 0)
		(cc 0))
	    (while (< rr (array-dimension grid 0))
	      (setf cc 0)
	      (while (< cc (array-dimension grid 1))
		(if (and (valid-point? grid rr cc)
			 (not (already-checked? unchecked-grid rr cc)))
		    (let ((curritem (aref grid rr cc)))		     
		      (if (eq 1 curritem) (incf count-islands))
		      (find-similar-values curritem rr cc grid unchecked-grid)))
		(incf cc))
	      (incf rr)))))
    count-islands))
	     
(defmacro while (test &body body)
  `(do ()                                                
       ((not ,test))
     ,@body))

(defun find-similar-values (initvalue r c grid unchecked-grid)
  (if (and (valid-point? grid r c) (not (already-checked? unchecked-grid r c)) (eq initvalue (aref grid r c)))
      (progn
	(setf (aref unchecked-grid r c) t)
	(find-similar-values initvalue (1+ r) c grid unchecked-grid); down
	(find-similar-values initvalue r (1+ c) grid unchecked-grid); right
	(find-similar-values initvalue (- r 1) c grid unchecked-grid); up
	(find-similar-values in  itvalue r (- c 1) grid unchecked-grid)))); left

(defun valid-point? (grid r c)
  (if (and (< r (array-dimension grid 0)) (< c (array-dimension grid 1))
	   (>= r 0) (>= c 0))
      t nil))

(defun already-checked? (grid r c)
  (if (null (aref grid r c)) nil t))

(defparameter *grid* #2A((1 1 1 0 0) (1 1 0 0 0) (0 0 1 0 0) (0 0 0 1 1)))

(num-islands *grid*)

;;solve using less memory assume no secondary array check off
;;add already seen stuff into a hash table
(defun num-islands2 (grid)
  (let ((count 0)
	(island-hash (make-hash-table :test #'equalp))
	(i 0)
	(j 0))	    
    (cond ((null grid) 0)
	  ((> 2 (length (array-dimensions grid))) 0)
	  (t 
	   (while (< i (array-dimension grid 0))
	     (setf j 0)
	     (while (< j (array-dimension grid 1))
	       (if (and (eq 1 (aref grid i j)) (null (gethash (list i j) island-hash)))
		   (find-adjacent-land i j grid island-hash (incf count)))
	       (incf j))
	     (incf i))))
    (print count)
    island-hash))
	       
								


(defun find-adjacent-land (r c grid hashtable islandnum)
  (if (and (valid-point? grid r c) (null (gethash (list r c) hashtable)) (eq 1 (aref grid r c)))
      (progn
	(setf (gethash (list r c) hashtable) islandnum)
	(find-adjacent-land (1+ r) c grid hashtable islandnum) ; right
	(find-adjacent-land r (1+ c) grid hashtable islandnum)))) ; down
      
(num-islands2 *grid*); 3 and returns the hash table
(maphash #'(lambda (k v) (format t "k: ~a v: ~a~%" k v)) (num-islands2 *grid*)); print all the mappings of th islands
(maphash #'(lambda (k v) (if (eq 1 v) (format t "k: ~a v: ~a~%" k v))) (num-islands2 *grid*)); show me the first island




;;coin collecting - given an amount and coin denimonations find smallest num of coins
;;assumpiton - largest coin to smallest will give you an answer
;;if largest to smallest sometimes wont work, then try smallest and repeat
(truncate 25 3); 8 1 - so we have the num of coins and the amount left

;;for 25 can use 11*2 + 1 * 3 = 25 => 12 coins, but we can do better
;;25 = 25 - 2 = 23, 23 - 2 = 21, (tuncate 21 3) = 7 remainder 0 => 9 coins

;;say coins = {5, 3, 2}

;;could make a recursive solution 
;;best say we have 7 as the number and coins 2 3, then we know that
;; it's num to 6 + num to 1 - not possible to get the one
;; or num to 5 and num to 2 - 5 = 2 coins 2 = 1 coin = 3 coins 
;;or num to 4 and num to 3  - 4 = 2 coins 3 = 1 coin = 3 coins 


;;and if we always subtract the smallest incrment we'll get the best result
;;now how do we get the min to start the table

;lets have an array that stores these values
; 2 3 5
;0 = nil
;2 = 1
;3 = 1
;4 = leastcoins(4) - 2 = leastcoins(2) + leastcoins(2)  = 1 + 1 = 2
;5 = 1 
;6 = min of getting to 6 = 2 or 6 = 4 + 2, see how if it's under the max value then no point and might as well try with the smaller number
;7 = 5 + 2 = 2
;8 = 5 + 3 = 2
;9 = 5 + 3 + 1 (impossible) OR 9 = 7 + 2 = 2 + 1 = 3
;10 = 2
;11 = 10 + 1 or 9 + 2 = 3 + 1 = 4
;12 = 2
;13 = 12 + 1
;;;doesn't work
;;think of a map that expands like a tree

;;;;TRY 2;;;;
;;take 2 3 9 10
;;know we can make 2 3 9 10 with n = 1 coin
;;with n = 2 (coins) => we can make 10 + 9 , 10 + 3, 10 + 2, 9 + 3, 9 + 2, 3 + 2
;; ((19 2) (13 2) (12 2) (11 2) (5 2)); then we do that again for 3 coins 
;;n = 3 coins
;;idea is create all the outcomes you can make with n coins and
;;then see if your number comes up

;;stop when all the numbers end up > than what you find in the hash
;;or stop when you find T you are looking for


(defun min-change (amount coins-arry)
  (let ((h (make-hash-table))
	(i 0)
	(coins (sort coins-arry #'>)))
    (while (< i (length coins))
      (setf (gethash (aref coins i) h) 1)
      (incf i))    
    (min-change-helper amount coins h 1)))

;;target, coins array, hashtable with the mapping, number of coins
;;so we're doing a sort of chain reaction - where we find worst case it's N^2 method but this one uses hash tables so is better
(defun min-change-helper (target coins hash n)
  (let ((list-min-targs nil))
    (maphash #'(lambda (k v) (if (eq n v) (push k list-min-targs))) hash); fill list-min-targs with the 
    (cond ((null list-min-targs) nil) ;means we didn't find the target
	  (t (dolist (elem list-min-targs)
	       (let ((i 0))
		 (while (< i (length coins))
		   (let ((subtarget  (+ elem (aref coins i))))
		     (cond ((eq elem target) (gethash elem hash));for the the coin itself 
			   ((and (<= subtarget target) ;say the min coins 10 + 9 = 19  and make sure it's less than the target of 20 (if the target was 18 we would have gone over)
				 (null (gethash subtarget hash))) ;and we haven't seen it before
			    (setf (gethash subtarget hash) (+ n 1)))
			   (t "should not be here"))
		     (incf i)))))
	     (if (gethash target hash)
		 (gethash target hash) ;put a (break) here and view the resulting hash table
		 (min-change-helper target coins hash (+ n 1)))))))
  

;;logxor
(defun 2s-complement (n)
  (+ 1 (lognot n)))

(defun bitwise-lowest1 (n)
  (logand n (2s-complement (- n 1))))

;x & ~(x - 1) ;;except the ~ is the 2s complement

(parse-integer "00101100" :radix 2); 44
(parse-integer "00101011" :radix 2); 43

(parse-integer (format nil "~b" (logand 44 43)) :radix 2); 40

(format nil "~b" (logand 44 43)); "101000" 

(logand 44 (2s-complement 40)); 8
(format nil "~b" (logand 44 (2s-complement 40))); "1000"
;;;the above isn't the lowest 1, so we've done something wrong

;;attempt 2 - x & ~(x-1) to get the 
(parse-integer "00101100" :radix 2);44 x
(parse-integer "00101011" :radix 2);43 (x - 1)

(parse-integer "01010100" :radix 2); 212 or -84 if first thing is a 1
(logand 44 212); 4 = "100" = correct answer

;;goal is to get to the 212 or the -84
(logand 44 (lognot (- 44 1))); 4 which is the answer
(logxor 44 4); 40
(format nil "~b" 40)

;;takes a number and returns the binary string for it
(defun print-binary-num (n &optional (stream nil))
  (format stream "~b" n))


;;finds the pos of lowest 1 in binary number
(defun lowest-1 (n)
  (logand n (lognot (- n 1))))

;;counts the number of 1s in a binary number 
(defun bitwise-count-1s (n &optional (acc 0))
  (let ((lowest-1-pos (lowest-1 n)))
    (cond ((<= lowest-1-pos 0) acc)
	  ((< n 0) (bitwise-count-1s (logxor (* -1 n) lowest-1-pos) (1+ acc)))
	  (t (bitwise-count-1s (logxor n lowest-1-pos) (1+ acc))))))

(mapcar #'(lambda (b) (format nil "~b" b)) '(15 14 12 8 0))

;;say you wanted to count the 1's in a 64 bit word - odd or even parity
(defun parity-check (n)
  (let ((pos1 (lowest-1 n)))
    (cond ((equalp pos1 0) 0)
	  (t (logxor 1 (parity-check (logxor n pos1)))))))

(defun print-parity-check (n)
  (format nil "number = ~b, parity = ~a" n (parity-check n)))


;;now you want to count all the 1's in several 64 bit ones - odd or even parity
;;need some sort of table
;1111 1110 1010 0110
;need a mapping table
;00 nil
;01 t - odd parity
;10 t - odd parity
;11 nil

;;doing some array manipulation 
;;pick an element and find all the lowest eql and hightest
;;#1A(1 10 9 8 2 3 3 ); and index - 2 ->  #1A(1 8 2 3 3 9 10) - just has to be sorted around the index not hte whole array sorted

;;say we only wanted to do this one time
(defparameter *test-arry* #1A(54 26 93 17 77 31 44 55 20)) 

(defun swap (arry i j)
  "destructive way to swap elements of an arry"
  (let ((temp (aref arry i)))
    (setf (aref arry i) (aref arry j))
    (setf (aref arry j) temp)))

(defun quickpartion (arry i)
  (let ((pivot (aref arry i))
	(letftmark 1)
	(rightmark (1- (length arry))))
    (swap arr i 0); puts pivot right at the front
    (while (< leftmark rightmark) 
      (let ((leftelem (aref arr leftmark))
	    (rightmark (aref arr rightmark)))
	(cond ((and (> leftelem pivot)
		    (< rightelem pivot))
	       (swap arry rightmark leftmark) ;swap them and then move left again
	       (incf leftmark))
	      ((> leftelem pivot) (incf leftmark))
	      ((< rightelem pivot) (decf rightmark)))))
    (swap arr leftmark 0)
	       
