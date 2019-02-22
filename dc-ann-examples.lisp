(in-package :dc-ann-examples)

(defun xor-ann ()
  (make-instance 't-net :id :xor :topology '(2 5 1)))

(defun xor-training ()
  "A single output where a value closer to 1 is true and a value closer to 0 is false."
  (loop for a from 0 to 1 appending
       (loop for b from 0 to 1 collect 
            (list (vector a b) (vector (boole boole-xor a b))))))

(defun xor-ann-1hs ()
  (make-instance 't-net :id :xor-1hs :topology '(2 5 2)))

(defun xor-training-1hs ()
  "Two outputs, with the first one representing true and the second representing false."
  (loop for a from 0 to 1 collect
       (loop for b from 0 to 1
          for xor = (boole boole-xor a b)
          collect (list (vector a b)
                        (if (zerop xor)
                            (vector 0 1)
                            (vector 1 0))))))

(defun circle-training (count &key stats)
  "Creates a training set for the dots-in-circle problem"
  (if stats
      (loop for a from 1 to count
         for x = (* (if (zerop (random 2)) 1 0) (random 1.0))
         for y = (* (if (zerop (random 2)) 1 0) (random 1.0))
         for c = (sqrt (+ (* x x) (* y y)))
         maximizing x into maxx
         maximizing y into maxy
         minimizing x into minx
         minimizing y into miny
         summing c into sumc
         finally (return (list :max-x maxx :max-y maxy
                               :min-x minx :min-y miny
                               :average-diameter (/ sumc count))))
      (loop with diameter = 0.429
         for a from 1 to count
         for x = (* (if (zerop (random 2)) 1 0) (random 1.0))
         for y = (* (if (zerop (random 2)) 1 0) (random 1.0))
         for c = (sqrt (+ (* x x) (* y y)))
         collect (list (vector x y) (vector (if (> c diameter) 0 1))))))

(defun circle-training-1hs (count)
  "Creates a training set for the dots-in-circle problem.  However, this training set is for an architecture that includes 2 outputs, where the first output represents 'yes' and second output represents 'no'"
  (loop with diameter = 0.429
     for a from 1 to count
     for x = (* (if (zerop (random 2)) 1 0) (random 1.0))
     for y = (* (if (zerop (random 2)) 1 0) (random 1.0))
     for c = (sqrt (+ (* x x) (* y y)))
     collect (list (vector x y)
                   (if (> c diameter)
                       (vector 0.0 1.0)
                       (vector 1.0 0.0)))))

(defun count-ann (count-limit)
  (make-instance 't-net :id "count"
                 :topology (list count-limit
                                 (* 2 count-limit)
                                 (* 2 count-limit)
                                 (1+ count-limit))))

(defun count-training (sample-count count-limit)
  (loop for a from 1 to sample-count
     for n = (random (1+ count-limit))
     collect (list 
              (vector-with-n-items count-limit n)
              (vector-with-item-n-lit (1+ count-limit) n))))

(defun vector-with-n-items (length n)
  (apply #'vector 
         (loop with lit-bits = (choose-from-list (range 0 (1- length)) n)
            for a from 0 below length
            collect (if (member a lit-bits) 1 0))))

(defun vector-with-item-n-lit (length n)
  (apply #'vector (loop for a from 0 below length
                       collect (if (= n a) 1 0))))

(defun count-trained (count-limit sample-count)
  (let ((ann (count-ann count-limit)))
    (train ann 
           (tset-to-file ann (count-training sample-count count-limit))
           :randomize-weights '(:min -0.9 :max 0.9))
    ann))

(defun count-test (net count-limit test-size)
  (evaluate-one-hotshot-training
   net
   (count-training test-size count-limit)))

(defun variables-training (count-limit sample-count)
  (loop for a from 1 to sample-count
     for n1 = (random count-limit)
     for n2 = (random count-limit)
     for n3 = (random count-limit)
     for i1 = (vector-with-n-items count-limit n1)
     for i2 = (vector-with-n-items count-limit n2)
     for i3 = (vector-with-n-items count-limit n3)
     for o1 = (vector-with-item-n-lit (1+ count-limit) n1)
     for o2 = (vector-with-item-n-lit (1+ count-limit) n2)
     for o3 = (vector-with-item-n-lit (1+ count-limit) n3)
     collect (list (concatenate 'vector i1 i2 i3)
                   (concatenate 'vector o1 o2 o3))))

(defun variables-ann (count-limit)
  (make-instance 't-net :id "vars"
                 :topology (list (* 3 count-limit)
                                 (* 8 count-limit)
                                 (* 7 count-limit)
                                 (* 3 (1+ count-limit)))))

(defun variables-trained (count-limit sample-count)
  (let ((ann (variables-ann count-limit)))
    (train ann
           (tset-to-file ann (variables-training count-limit sample-count))
           :randomize-weights '(:min -0.9 :max 0.9))
    ann))

(defun variables-test (net count-limit test-size)
  (loop for test in (variables-training count-limit test-size)
     for target-outputs = (map 'list 'identity (second test))
     for outputs = (map 'list 'identity (feed net (first test)))
     when (equal outputs target-outputs)
     sum 1 into pass
     finally (return (format nil "Pass: ~,,2f%" (/ pass test-size)))))
                                          
(defun variables-interpret-sample (sample)
  (let* ((input-length (length (car (car sample))))
         (output-length (length (second (car sample))))
         (input-part-size (/ input-length 3))
         (output-part-size (/ output-length 3)))
    (loop for row in sample collect
         (list :input (mapcar (lambda (part) (reduce '+ part))
                              (partition (car row) input-part-size))
               :output (mapcar (lambda (part) (index-of-max part))
                               (partition (second row) output-part-size))))))
