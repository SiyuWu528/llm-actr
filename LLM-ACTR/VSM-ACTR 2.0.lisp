;;reward function
(defvar *reward* 0) ; Initializes *reward* to 0

(defun update-reward ()
  (print "update-reward called") ; Print to confirm function is called

  ; Retrieve strategy and state values using buffer-slot-value
  (let ((strategy (buffer-slot-value 'goal 'section))
        (state (buffer-slot-value 'goal 'cost)))  ; Corrected slot name
      (format t "Strategy: ~a, State: ~a~%" strategy state)

					; Update reward based on strategy and state
      (cond ((eq strategy 'expert) (setf *reward* 4))
            ((eq strategy 'intermediate) (setf *reward* 2))
	    ((eq strategy 'novice) (setf *reward* 0)))

      ; Update reward based on state
      (cond ((eq state 'inefficient) (setf *reward* (- *reward* 2)))
            ((eq state 'efficient) (setf *reward* (+ *reward* 2))))

  ; Print updated reward
  (format t "Updated Reward: ~a~%" *reward*)
   *reward*))

;;cost eval
;;decide cost efficient
(defun calculate-cost (diff)
  (if (> diff 0) 'efficient 'inefficient))


;;ACT-R

(clear-all)

;;save model output
(define-model vsm
(defun run-model-and-save-output (n)
  "Run the model N times and save each output to a separate file."
  (dotimes (i n)
    (reset) ;; assuming the model should be run from the beginning each time
    (sgp-fct (list :v (format nil "c:/Users/sfw5621/Desktop/model-output2/model-output-~a.txt" i)))
    (run 10)
    (sgp :v nil) ;; make sure the output stream is closed
    ))
;; parameters
;(sgp :seed (10000 100))
(sgp :v t :esc t :trace-detail low  :ul t :ult t :egs 4 :MODEL-WARNINGS NIL)

;chunk-type
(chunk-type decision strategy state time OEE1 OEE2 CT1 CT2 cost section)
(chunk-type decision-metrics preweight weight defect-rate1 defect-rate2 dif diff)


  
;goal-chunk
 (define-chunks
    (goer isa decision state choose-strategy time blank OEE1 blank OEE2 blank CT1 blank CT2 blank cost blank)
   (choose-strategy)(decide-strategy)(intermediate-strategy) (intermediate-decision)(brute-strategy)(brute-decision) (perceive)(weight)(next-weight) (preassemble)(assemble-perceive)(assemble)(compare)(decide)(next-decide)(headcount1)(headcount2)(result)(evalcost)(stop)(blank) (expert isa decision state section)
   (novice isa decision state section)
   (intermediate isa decision state section))


;start goal buffer
(goal-focus goer)

;prodcution rules
;;start the production of choose strategy, one good strategy and the other is distrated strategy
(p choose-strategy
   =goal>
   isa decision
   state choose-strategy
==>
   =goal>
   isa decision
   state decide-strategy
   )
  
 
    
 ( p expert-strategy
   =goal>
     isa decision
     state  decide-strategy
 ==>
     =goal>
     isa decision
     state perceive
     
  )

;; perceive the variables, to prepare for the weight calculate
(p perceive
   =goal>
   isa     decision
   state   perceive
 ==>
   =goal>
   isa     decision
   state   weight
   time   4
   OEE1   0.93
   CT1 51
   OEE2  0.98
   CT2  46
   section expert
   )


  
  ;;calculate the weights of preassemble
 (p preassemble-weight
    =goal>
    isa    decision
    state  weight
    time  =time
    OEE1  =OEE1
    CT1   =CT1
    OEE2  =OEE2
    CT2   =CT2
    section =section
    ?imaginal>
   buffer empty
    state  free
==>
    =goal>
    !bind! =aval (/ (* =CT1 =OEE2) (+ (* =CT1 =OEE2) (* =CT2 =OEE1)))
   ; !bind! =bval (/ (* =CT2 =OEE1) (+ (* =CT1 =OEE2) (* =CT2 =OEE1)))
    !output! (=aval)
    !output! ("caculate the preassemble defect decision weight")
   ; !output! (=bval)
   ; !output! ("calculate the assemble defect decision weight")
     +Imaginal>
    preweight   =aval
    weight      nil
 =goal>
   isa     decision
   state   next-weight
    )

;;calculate the weight of assemble
(p assemble-weight
   =goal>
    isa  decision
    state  next-weight
    time   =time
    OEE1  =OEE1
    CT1   =CT1
    OEE2  =OEE2
    CT2   =CT2
    =imaginal>
    preweight   =aval
    weight      nil
==>
     !bind!   =bval (/ (* =CT2 =OEE1) (+ (* =CT1 =OEE2) (* =CT2 =OEE1)))
     !output!  (=bval)
     !output!  ("calculate the assemble defect decision weight")
   =goal>
     isa  decision
     state  preassemble
    +Imaginal>
    preweight   =aval
    weight      =bval
    )

;; calculate preassemble defect rate1 and put into imaginal
(p preassemble
  =goal>
   isa   decision
   state   preassemble
   time    =time
   OEE1    =OEE1
   CT1      =CT1
   OEE2    =OEE2
   CT2    =CT2
 =imaginal>
   preweight   =aval
   weight      =bval
   defect-rate1  nil
 ==>
  !bind!    =cval(* (*  =time (- 1 =OEE1) ) =aval)
   !output! (=cval)
   !output! ("calculate the final preassemble defect rate")
  ;!bind!    =dval(* (*  =time (- 1 =OEE2) ) =bval)
   +Imaginal>
  ; preweight   =aval
   weight      =bval
   defect-rate1   =cval
   defect-rate2   nil
 =goal>
   isa     decision
  state   assemble-perceive
   )
  
;;perceive the assemble decision merits nd calculate the defect rate increase
(p assemble
  =Imaginal>
  ; preweight   =aval
   weight      =bval
   defect-rate1   =cval
   defect-rate2   nil
  =goal>
   isa    decision
   state  assemble-perceive
   time    =time
   OEE2    =OEE2
   CT2    =CT2
 ==>
    !bind!    =dval(* (*  =time (- 1 =OEE2) ) =bval)
   !output! (=dval)
   !output!  ("calclate the assemble defect rate") 
  +imaginal>
    defect-rate1   =cval
    defect-rate2   =dval
 =goal>
   isa     decision
   state  compare
   )

;; compare the defect rate 
(p compare
 =goal>
   isa    decision
   state  compare
   =imaginal>
   defect-rate1   =cval
   defect-rate2   =dval
==>
 =goal>
   isa decision
   state  decide
   !bind!  =val(- =cval =dval)
   !output! (=val)
  =imaginal>
   dif    =val
   )

;;Decide which sector to reduce
(p decide
 =goal>
   isa    decision
   state   decide
 =imaginal>
   < dif  0
==>
 =Goal>
   isa      decision
   state    headcount1
   !output!  ("choose preassemble has better stable output!")

   )

;; compare the OEE for the judgemental of headcout cost efficiency
;;a drity hack of not using any buffer
(p headcount
  =goal>
   isa decision
   state headcount1
   OEE1 =OEE1
   OEE2  =OEE2
   section =section
   ==>
   !bind!  =fval(- =OEE1 =OEE2)
   !output! (=fval)
   !bind! =cost(calculate-cost =fval)
  =goal>
   isa decision
   state result
   cost  =cost
   section =section
  ; +imaginal>
  ; diff  =fval
   
   )
  

(p redecide
 =goal>
   isa    decision
   state  decide
 =imaginal>
   > dif    0
  
==>
  =goal>
   isa      decision
   state    headcount2
   !output!  ("choose assemble has better stable output!")
   )

(p reheadcount
  =goal>
   isa decision
   state headcount2
   OEE1 =OEE1
   OEE2  =OEE2
  section =section
   ==>
   !bind!  =gval(- =OEE2 =OEE1)
   !output! (=gval)
   !bind! =cost(calculate-cost =gval)
  =goal>
   isa decision
   state result
   cost  =cost
   section =section
  ; +imaginal>
  ; diff  =fval
  
   )
  
(p stop
  =goal>
  isa decision
   state result
   cost  =cost
==>
   =goal>
   isa decision
   state choose-strategy
   !output!  ("this is the end of one decision making")
    !eval! (trigger-reward (update-reward))
   )
  
 (P decide-intermediate
   =goal>
   isa decision
   state decide-strategy
==>
   =goal>
   isa decision
   state intermediate-strategy
    time  4
    OEE1   0.93
    CT1 51
    OEE2  0.98
    CT2  46
   section intermediate
   )

(p intermediate-strategy
   =goal>
    isa  decision
    state intermediate-strategy
    time   =time
    OEE1  =OEE1
    CT1   =CT1
    OEE2  =OEE2
   CT2   =CT2
   section =section
     ?imaginal>
     buffer  empty
     state   free
==>
     !bind!  =val(- =OEE1 =OEE2)
     !output! (=val)
    =goal>
     isa decision
     state intermediate-decision
     +imaginal>
   dif  =val
   ; !eval! (trigger-reward  (update-reward))
)

  ;(trigger-reward  (update-reward))
  
  (p intermediate-choice
     =goal>
   isa    decision
   state  intermediate-decision
 =imaginal>
   < dif    0
==>
  =goal>
   isa      decision
   state    headcount1
    !output!  ("choose preassemble has better stable output!")
   
 )
  
 (p inermediate-choice2
     =goal>
   isa    decision
   state  intermediate-decision
 =imaginal>
   > dif    0
==>
  =goal>
   isa      decision
   state    headcount2
    !output!  ("choose assemble has better stable output!")
   
    )
(p decide-brute  
  =goal>
   isa decision
   state decide-strategy
==>
   =goal>
   isa decision
   state brute-decision
    time  4
    OEE1   0.93
    CT1 51
    OEE2  0.98
    CT2  46
   section novice
     )
  
  (p brute-decision
     =goal>
     isa decision
     state brute-decision
     time   =time
    OEE1  =OEE1
    CT1   =CT1
    OEE2  =OEE2
   CT2   =CT2
   section =section
 ==>
     =goal>
     isa decision
     state headcount2
     !output! ("assembly is always a good place to reduce time!")
     )
 (spp decide-brute :u 3)
 (spp expert-strategy :u 0)


 ; (spp compare :reward 50)
 ; (spp headcount :reward -10)
)
