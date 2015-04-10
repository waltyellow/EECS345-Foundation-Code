;zxh108
;Zhenglin Huang
;optimized version, Part 2

;(load "functionParser.scm")

;takes a file and run the call tree generated from it
(define interpret
  (lambda (file)
    (run (parser file) newenvironment stdreturn stdreturn stdreturn)))

;--------------------
(define call-main
  (lambda (state)
 (M_return '(return (funcall main)) state)))


;--------------------

;returns a state that is modified by the call tree
;Input: queue is the list of statement, state is the state to operate on, 
;return is the function that takes our output state, when return is called 
;break is the function that takes output state when break is called
;continue is the function that takes output state when break is called
(define run
  (lambda (queue state return break continue)
    ;if the tree is empty
    (if (null? queue)
        ;then returns the result
        (return state)
        ;else we have different cases
        (cond
          ;if the first statement of the tree is null, then skip it, and execute the rest
          ((null? (car queue)) (run (cdr queue) state return break continue))
          ;else, let Mstate execute the current function. 
          (else (Mstate-cps (car queue) state (lambda (v) (run (cdr queue) v return break continue)) break continue))))))

;standard return function
(define stdreturn
  (lambda (v) v))

;building block of "run"
;input: takes a single-statement and execute it. Along with state and the 3 return functions
;ex. takes (begin (...) (...) (...))
;ex. takes (= x 5)
;output: modified state in a cps manner
(define Mstate-cps
  (lambda (statement state return break continue)
    (cond
      ;if the statement is empty, do nothing
      ((null? statement) (return state))
      ;if the first word is var, it is an declaration
      ((eq? 'var (operator statement)) (Mstate_declaration-cps statement state return))
      ;if the first word is "=", it is an assignment
      ((eq? '= (operator statement)) (Mstate_assignment-cps statement state return))
      ;if it starts with "return", we retrive the boolean or numerical value of the next item, the expression
      ((eq? 'return (operator statement)) (break (M_return statement state)))
      ;if it is an if statement
      ((eq? 'if (operator statement)) (Mstate_if-cps statement state return break continue))
      ;if it is a while loop
      ((eq? 'while (operator statement)) (M_while statement state return ))
      ;if we see a begin, then (cdr statement) (inside a block) is a tree
      ((eq? 'begin (operator statement)) (M_block_begin statement state return break continue))
      ((eq? 'continue (operator statement)) (continue state))
      ((eq? 'break (operator statement))  (break state))
      ;Added function call without return
      ((eq? 'funcall (operator statement)) (begin (Mfuncall (cdr statement)) (return state)))
      (else (error 'statement_not_recognized)))))

;takes a block-begin-end statement and run it with "run" function
;for the 3 return functions, exit_block is built-in. Meaning that the block will blow off top layer state automatically.
;returns the state in cps style
(define M_block_begin
  (lambda (statement state return break continue)
    (run 
     (cdr statement) 
     (enter_block state) 
     (lambda (result) (exit_block result return)) 
     (lambda (snapshot) (exit_block snapshot break)) 
     (lambda (snapshot) (exit_block snapshot continue))
     )))

;takes a while statement, and execute it
;this is the layer to create a breakpoint
(define M_while
  (lambda (statement state return)
         (M_while_loop statement state return return)))

;building block for while
;takes a while loop, evaluate the condition and decide whether to continue the loop
(define M_while_loop
  (lambda (statement state return break)
    ;if the condition is true
    (if (Mboolean (condstmt statement) state)
        (Mstate-cps 
         ;execute the body
         (thenstmt statement)
         ;with current state
         state 
         ;how we deal with executed return state => run the loop again
         (lambda (result_state) (M_while_loop statement result_state return break )) 
         ;preserve breakpoint
         break 
         ;stated passed in by continue => run the loop again
         (lambda (continue_state) (M_while_loop statement continue_state return break)))
        ;else the condition is not true, return the current state as break
        (break state))))

;a filter layer to create proper return value
(define M_return
  (lambda (statement state)
    ((lambda (print)
       (cond
         ((eq? print #t) 'true)
         ((eq? print #f) 'false)
         (else print)))
     (Mexpression (secondterm statement) state))))

;takes an if statement and a state, and generates a new state in a cps manner
(define Mstate_if-cps
  (lambda (statement state return break continue)
    ; if it is true
    (if (Mboolean (condstmt statement) state)
        ;do this as then
        (Mstate-cps (thenstmt statement) state return break continue)
        ;if it falls to else
        (if (null? (cdddr statement))
            (return state)  ;<-else does not exist, do nothing
            ;if else does exist as below-
            (Mstate-cps (elsestmt statement) state return break continue)))))

(define condstmt cadr)
(define thenstmt caddr)
(define elsestmt cadddr)

;takes an declaration and returns a modified state, in a cps manner
(define Mstate_declaration-cps
  (lambda (statement state return)
    ;if it is a (var x) style
    (if (null? (aftersecondterm statement))
        ;adds a new one, not initialized. Remove the same name variable if there is any
        (return (add_declare_only (lhs statement) (remove_from_top_layer (lhs statement) state)))
        ;adds a new one with declared value. Removed the same variable previously first
        (return (add_with_assignment (lhs statement) (Mexpression (rhs statement) state) (remove_from_top_layer (lhs statement) state))))))

;-----------------Assignment-------------
;takes an assignment statement
(define Mstate_assignment
  (lambda (stmt state)
    (Mstate_assignment-cps stmt state stdreturn)))

;cps
(define Mstate_assignment-cps
  (lambda (statement state return)
    ;if the variable is declared
    (return (update_var (lhs statement) (Mexpression (rhs statement) state) state))))

(define lhs cadr)
(define rhs caddr)


;-------------EVALUATION OF MVALUE---------------------
;
;
;//////Mexpression, Mvalue and Mboolean
;
;
;-------------FUNCTION EXECUTION HAPPEN HERE-----------
;takes an expression, and returns its value either in boolean or integer
;functions:
;1. understand true and false atonds
;2. lookup proper boxes
;3. send task to either Mvalue and Mboolean

(define Mexpression
  (lambda (expression state)
    (cond
      ((boolean? expression) (Mboolean expression state))
      ((number? expression) (Mvalue expression state))
      ((eq? expression 'true) (Mboolean #t state))
      ((eq? expression 'false) (Mboolean #f state))
      ((atom? expression) (Mexpression (lookup expression state) state))
      ((and (pair? expression) (eq? (car expression) 'function)) (lookup expression state))
      ;((and (pair? expression) (eq? (car expression) 'closure)) "hit test point")
      ;check for numerical calculation
      ((member? (operator expression) '(+ - * / %)) (Mvalue expression state))
      ;function calls
      ((eq? (operator expression 'funcall)) (Mfuncall (cdr expression)))
      ;check for boolean calculation
      ((member? (operator expression) '(|| && ! == != > < <= >=)) (Mboolean expression state))
      ;if it is not boolean or numerical, we can't solve it for now
      (else (error 'expression_not_supported)))))

;-------------Function calls (experimental)----------------------
(define get_func_name cadr)
(define get_actual_par cddr)
;Mfuncall takes a function call in form of (gcd x y).
;Input: a name and parameters
;Output: the result of function execution
(define Mfuncall
  (lambda (call state)
    ((lambda (closure)
       ;run the function in a special environment, returns a value or returns no value;
      (run (look_up_closure_body closure) 
           ;create the function runtime environment
           (call-by-value_initialize_environment
                 ;find formal parameters list
                 (look_up_formal_parameters closure)
                 ;extract actual parameters from the call
                 (get_actual_par call)
                 ;create the raw function call environment from current state
                 ((look_up_state_creation_function closure) state)
                 ;the current state that actual parameters live in
                 state)
           ;3 standard return function
           stdreturn stdreturn stdreturn))
           ;returns the closure of a function
           (lookup (cons 'function (cons (get_func_name call) '())) state))))

;create parameter binding in a call-by-value manner
(define call-by-value_initialize_environment
  ;takes 4 inputs, formal parameter list, actual param list, function runtime environment to be returned
  ;and outer environment actual parameters live in
  ;outputs the new function runtime environment with parameter binding created
  (lambda (formal actual func_environment outer_environment)
    (cond
      ;if both of them becomes null, we funished all binding
      ((and (null? formal) (null? actual)) func_environment)
      ;if one of them but not both becomes null, there is an arity mismatch
      ((or (null? formal) (null? actual)) (error 'arity-mismatch))
      ;else, we are going on
      (else (call-by-value_initialize_environment 
             ;add_to_top_layer, bind the first of formal to its actual par value
             
             ;continue to process the remaining of the remainings
             (cdr formal)
             (cdr actual)
             ;create binding of name  <= first item in remaining formal parameter.
             ;                  value <= first item in remaining actual parameters, evaluated.
             ;   added to environment <= the function call runtime environment
             ;returns a modiefied environment
             (add_with_assignment (first_item formal) (Mexpression (first_item actual) outer_environment) func_environment)
             outer_environment)))))

;retrieve the numerical value of an expression
(define Mvalue
  (lambda (expression state)
    (cond
      ((boolean? expression) (error 'type_incompatible_number_expected_but_boolean_given))
      ((number? expression) expression)
      ((eq? expression 'true) (Mboolean #t state))
      ((eq? expression 'false) (Mboolean #f state))
      ((atom? expression) (Mvalue (lookup_value expression state) state))
      ((eq? '+ (operator expression)) (+ (Mvalue (leftoperand expression) state)
                                         (Mvalue (rightoperand expression) state)))
      ((eq? '- (operator expression)) (if (null? (cddr expression))
                                          (- 0 (Mvalue (leftoperand expression) state))
                                          (- (Mvalue (leftoperand expression) state)
                                             (Mvalue (rightoperand expression) state))))
      ((eq? '* (operator expression)) (* (Mvalue (leftoperand expression) state)
                                         (Mvalue (rightoperand expression) state)))
      ((eq? '/ (operator expression)) (quotient (Mvalue (leftoperand expression) state)
                                                (Mvalue (rightoperand expression) state)))
      ((eq? '% (operator expression)) (remainder (Mvalue (leftoperand expression) state)
                                                 (Mvalue (rightoperand expression) state)))
      (else (error 'bad_operator)))))

;retrieves a boolean number by executing the expression
(define Mboolean
  (lambda (expression state)
    (cond
      ((number? expression) (error 'type_incompatible_boolean_expected_but_number_given))
      ((boolean? expression) expression)
      ((eq? expression 'true) (Mboolean #t state))
      ((eq? expression 'false) (Mboolean #f state))
      ((atom? expression) (Mboolean (lookup expression state) state))
      ((eq? '|| (operator expression)) (if (Mboolean (leftoperand expression) state)
                                           true
                                           (Mboolean (rightoperand expression) state)))
      ((eq? '&& (operator expression)) (if (Mboolean (leftoperand expression) state) 
                                           (Mboolean (rightoperand expression) state)
                                           false))
      ((eq? '!  (operator expression)) (not (Mboolean (leftoperand expression) state)))
      ((eq? '== (operator expression)) (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '!= (operator expression)) (not (eq? (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state))))
      ((eq? '>  (operator expression)) (> (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '<  (operator expression)) (< (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '<= (operator expression)) (<= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? '>= (operator expression)) (>= (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      (else (error 'bad-operator)))))

;------------------Helpers--------------------------

;abstractions for getting the three things right
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)
(define aftersecondterm cddr)
(define secondterm cadr)
(define thirdterm caddr)
(define atom? (lambda (v) (not (pair? v))))









;--------------------See below for environmental manipulations---------------------------










;--------------Environment manipulations that is seperated from the code --------------
;Features of the environment
;	Add: takes a state and info to add, returns a state with binding added to top layer
;		Add_plain: add on a base layer
;	Remove: Remove can only remove the variable that is on the top layer
;		Remove_plain: preform removal on a plain layer without other layer on top of
;	Lookup:
;		Search top layer down, if found, immediately return to call/cc break. If not found, go to the the next layer.
;	Update: Lookup with a value intended. Once found, swap the box content

;------ Add ----------------------------
;method to add a new variable and assign its value, show that it is initialized
(define add_with_assignment
  (lambda (name value state)
    (add_to_top_layer name (box value) state)))

(define addvalue add_with_assignment)

;method to only declare a variable, show that it is initialized
(define add_declare_only
  (lambda (name state)
    (add_to_top_layer name (box '(null)) state)))

(define addvar add_declare_only)

;takes a layered state, and add the binding to the top layer
;Input: x1=name to be added x2=value too be added state
;Output: a state with such binding added to its top layer
(define add_to_top_layer
  (lambda (name value state)
    (switch_top_layer (add_without_block name value (fetch_top_layer state)) state)))

;takes a state without block, and add the binding to the state
;Input: x1=name to be added x2=value too be added, state to be modified
;Output: a state with such binding added

(define add_without_block
  (lambda (name value state)
    (combine (cons name (firstlist state)) (cons value (secondlist state)))))





;-------Remove ---------
;method to remove a variable from the an environment, and all attributes associated with it.

;remove a binding with name from top layer
;Input: a name for target removal, a state 
;Output: the targeted variable is removed from the top layer, if any
(define remove_from_top_layer
  (lambda (name state)
    (switch_top_layer (remove_without_block name (fetch_top_layer state)) state)))

(define remove_without_block
  (lambda (n s)
    (remove_without_block-cps n s stdreturn)))

;Input: a name, a state without blocks, and a return function
;Output: the state with binding corresponding to the name removed, if any
;---CPS-----
(define remove_without_block-cps
  (lambda (name state return)
    (cond
      ;nothing is left in the state, return the empty state
      ((null? (var_name_of state)) (return state))
      ;the first one is the target, remove it and return the rest
      ((eq? name (first_item (var_name_of state))) (return (remove_first state)))
      ;add the first back, after remove is performed at the rest
      (else (remove_without_block-cps name (remove_first state) (lambda (result) (return (add_without_block (first_item (var_name_of state)) (first_item (var_value_of state)) result))))))))

(define first_item car)



;------Lookup--------
;takes a name, a state
;intepret the box returned by the helper
; if the return box is '(null)
; the value to look up for is not assigned -> error
; if the return box look up returned a #f instead of a box
; the value to loop up for is not declared -> error
;---CPS cited-----
(define lookup_value
  (lambda (name state)
    ((lambda (box)
       (if (not box)
           (error 'notdeclared)
           ((lambda 
                (value)
                (if (and (pair? value) (eq? (car value) 'null))
                  (error 'notassigned)
                  value))
            (unbox box))))
     (lookup_box_general name state))))

(define lookup lookup_value)
;takes a name to look up for, and the state to loop up from
;returns the corresponding box from the inner most layer it was found
(define lookup_box_general
  (lambda (name2 state2)
    (call/cc
     (lambda (break)
       (letrec ((lookup_box (lambda (name state return)
                              (cond
                                ((hasblock? state) (if (not (lookup_box name (get_top_layer state) stdreturn))
                                                       (lookup_box name (except_top_layer state) stdreturn)
                                                       ))
                                (else 
                                 (if (member? name (var_name state))
                                     (break (find name (var_name state) (var_value state)))
                                     (return #f))))))) (lookup_box name2 state2 stdreturn))))))



;--------Update-------
;environment specific interfaces, strictly three lists and attributes are hard coded
(define update_var
  (lambda (name value state)
    ((lambda (box)
       (if (not box)
           (error 'notdeclared)
           (begin (set-box! box value) state)))
     (lookup_box_general name state))))

;note: init? does not exist anymore
;the way to retrieve name, value and init? from the environment
(define v_name car)
(define var_name car)
(define firstlist car)
(define var_name_of car)
(define v_value cadr)
(define var_value cadr)
(define var_value_of cadr)
(define secondlist cadr)
(define v_init? caddr)


;Advanced Layering Tools
;------------------------

(define count
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (+ 1 (count (cdr l)))))))
;count_layers: 
;IN: a state
;OUT: a number indicating layers on top of base layer.
;State= Base layer/Global => 0
;State is inside main method: =>1

(define count_layers
  (lambda (state)
    (- (count state) 2)))

;Remove_d_layers:
;IN: a number D, and a state
;OUT: A state with the top most D layers removed.
(define remove_d_layers
  (lambda (d state)
    (cond
      ((zero? d) state)
      (else (remove_d_layers (- d 1) (blow_top_layer state))))))

;Keep_n_layers
;IN: a number n and a state
;OUT: A state with the bottom most n layers only.
;Implementation: 
;Check for the layers of the input state, m
;Calculate d=m-n
;Remove d layers, remaining r=m-d=n layers
;Output the state with d layers removed

(define keep_n_layers
  (lambda (n state)
    ((lambda (m)
       (cond
         ((> n m) (error 'not_enough_layers_to_keep))
         (else (remove_d_layers (- m n) state)))) (count_layers state))))


;------ Layer helpers: manipulations of the layers -----

;IN: a state
;OUT: #t if a state has blocks inside
(define hasblock?
  (lambda (state)
    (not (null? (deletefirsttwo state)))))

;IP2 helper
(define deletefirstthree cdddr)
(define deletefirsttwo cddr)
(define get_top_layer car)
(define except_top_layer cdr)

;in: a state
;out: the input state with top layer removed
(define blow_top_layer
  (lambda (state)
    (blow_top_layer-cps state stdreturn)))

;enter block is one-operation non-cps
;in: a state
;out: the state with a new top layer added
(define enter_block
  (lambda (state)
    (cons newenvironment state)))

;in: a state, a return location
;out: the input state with top layer removed, feeded to the return function
;---CPS-----
(define blow_top_layer-cps
  (lambda (state return)
    (cond
      ((hasblock? state) (return (cdr state)))
      (else (return state)))))

(define exit_block
  blow_top_layer-cps)

;in: a state
;out: the top layer of the state, if none, then the state itself
(define fetch_top_layer
  (lambda (state)
    (cond
      ((hasblock? state) (fetch_top_layer (get_top_layer state)))
      (else state))))

;in: a replacement layer, and a state
;out: the state with top layer replaced by the replacement layer
;---CPS-----
(define switch_top_layer
  (lambda (replacement state)
    (switch_top_layer-cps replacement state stdreturn)))

(define switch_top_layer-cps
  (lambda (replacement state return)
    (cond
      ((hasblock? state) (return (cons replacement (cdr state))))
      (else (return replacement)))))


;find an item x from key(list), and return the corresponding item in the target(list)
(define find
  (lambda (x key target)
    (cond
      ((null? key) '(notfound))
      ((null? target) (error 'targetistooshort))
      ;if x is a list, then use listeq to evaluate
      ((and (and (list? (car key)) (list? x)) (listeq? x (car key))) (car target))
      ((eq? x (car key)) (car target))
      (else (find x (cdr key) (cdr target))))))

;takes two lists of atoms
(define listeq?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((eq? (car l1) (car l2)) (listeq? (cdr l1) (cdr l2)))
      (else #f))))

;create a state with 3 properties, empty
(define newenvironment '(()()))

;takes two sublists
;returns a state consists of 2 lists
(define combine
  (lambda (l1 l2)
    (cons l1 (cons l2 '()))))

;pass in a state with the first variable removed
(define remove_first
  (lambda (s)
    (combine (cdr (v_name s)) (cdr (v_value s)))))

;takes an atom and a list, to check whether the atom is in the list
(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((eq? a (car l)) #t)
      ((and (and (list? (car l)) (list? a)) (listeq? a (car l))) #t)
      (else (member? a (cdr l))))))

;testing tools
(define test
  (lambda (number)
    (cond
      ((< x 10) (interpret (string-append "t30" (number->string number) ".txt")))
      (else (interpret (string-append "t3" (number->string number) ".txt"))))))

(define testbatch
   (lambda (list key)
     (cond
       ((null? list) 'done)
       (else (checkanswer (car list) (find (car list) fullset fullsetanswers)) (testbatch (cdr list) (cdr key))))))
   
(define checkanswer
  (lambda (number key)
    (if (eq? (test number) key)
        (string-append "test" (number->string number) "=>good //")
        (string-append "test" (number->string number) "=>INCORRECT: expected" (number->string key) ", given:" (number->string (test number))))))



(define fullset        '( 1  2  3  4 5 6   8  9 10 11 13 14 15 16))
(define fullsetanswers '(10 14 45 55 1 115 20 24 2  35 90 69 87 64))

;(define autotest
 ; (testbatch fullset fullsetanswers))

