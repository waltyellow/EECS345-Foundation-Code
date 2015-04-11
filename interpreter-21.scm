;Kevin Wang (kxw233), Interpreter III, 4/10/2015
;Collaborated and Part II provided by Walter (Zhenglin) Huang (zxh108).

(load "functionParser.scm")


;interpret takes a file and executes it by calling the main method on the environment after all function declarations.
;INPUT: file name
;OUTPUT:result of calling the main method
(define interpret
  (lambda (file)
     (call-main (run (parser file) newenvironment stdreturn stdreturn stdreturn))))

;--------------------
;call-main 
(define call-main
  (lambda (state)
 (M_return '(return (funcall main)) state)))


          
;--------------------

;INPUT: a list of statements, a current state, a function to output when return, break, and continue are called
;OUTPUT: a state after execution of a statement.
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


;Mstate-cps takes a single-statement and execute it, and produces a modified state after the execution of a statement.
;INPUT: a single statement, a current state, and functions to deal with return, break, continue.
;OUTPUT: a modified state after statement's execution

(define Mstate-cps
  (lambda (statement state return break continue)
    (cond
      ;if the statement is empty, return the current state
      ((null? statement) (return state))
      ;if the first part of the statement is var, then go to the respective Mstate function for variable declaration
      ((eq? 'var (operator statement)) (Mstate_declaration-cps statement state return))
     ;if the first part of the statement is =, then go to the respective Mstate function for assignment
      ((eq? '= (operator statement)) (Mstate_assignment-cps statement state return))
      ;if the first part of the statement is return, then go to the respective function to find the return value
      ((eq? 'return (operator statement)) (break (M_return statement state)))
      ;if the first part of the statement is if, then go to the respective Mstate function to execute an if statement
      ((eq? 'if (operator statement)) (Mstate_if-cps statement state return break continue))
      ;if the first part of the statement is while, then go to the respective Mstate function for a while loop
      ((eq? 'while (operator statement)) (M_while statement state return))
      ;;if the first part of the statement is begin, then the statement is a new "layer"
      ((eq? 'begin (operator statement)) (M_block_begin statement state return break continue))
      ; If the first part of the statement is continue, executes a continue (breaks then returns to loop condition)
      ((eq? 'continue (operator statement)) (continue state))
      ;If the first part of the statemetn is break, executes a break (exits the layer/loop).
      ((eq? 'break (operator statement))  (break state))
      ;If the first part of the statement is a function declaration, add a closure of that function to the state.
      ((eq? 'function (operator statement)) (return (add_function_closure statement state)))
      ;If the first part of the statement is a function call, execute that function call.
      ((eq? 'funcall (operator statement)) (begin (Mfuncall statement state) (return state)))
      ;Otherwise, it is not a valid command.
      (else (error 'statement_not_recognized)))))

;Takes a statement with the word begin and creates a new layer, then executes that block of code.
;INPUT: A statement, state, and functions for return, break, and continue.
;OUTPUT: A new state with a new "layer" in which variables are only visible to that layer.

(define M_block_begin
  (lambda (statement state return break continue)
    (run 
     (cdr statement) 
     (enter_block state) 
     (lambda (result) (exit_block result return)) 
     (lambda (snapshot) (exit_block snapshot break)) 
     (lambda (snapshot) (exit_block snapshot continue))
     )))

;Executes the change in state for a while loop
;INPUT:A statement corresponding to a while loop, state, and return function
;OUTPUT: The statement after the execution of a while loop.
(define M_while
  (lambda (statement state return)
         (M_while_loop statement state return return)))

;A helper function to deal with the recursive call of a while loop to continue until condition becomes false.
;INPUT: statement corresponding to a while loop statement
;OUTPUT: state at the end of the a loop iteration.

(define M_while_loop
  (lambda (statement state return break)
    ;if the condition  of the loop is true
    (if (Mboolean (condstmt statement) state)
        (Mstate-cps 
         ;execute the body of the loop
         (thenstmt statement)
         ;with the current state
         state 
         ;recursive call, run the loop again as the loop condition is true here
         (lambda (result_state) (M_while_loop statement result_state return break )) 
         ;handle break statements
         break 
         ;if continue called, break the loop and return to the loop condition, and if that evaluates true, run the loop again
         (lambda (continue_state) (M_while_loop statement continue_state return break)))
        ;else the condition is not false so continue is equivalent to break
        (break state))))

;Converts a boolean return value to a literal one.
;INPUT: A statement and a given state
;OUTPUT: Either true or false literals if return value is a boolean value
(define M_return
  (lambda (statement state)
    ((lambda (print)
       (cond
         ;if true, print the literal true
         ((eq? print #t) 'true)
         ;if false, print the literal false
         ((eq? print #f) 'false)
         (else print)))
     (Mexpression (secondterm statement) state))))

;Takes an if statement and returns the state after the execution of that if statement.
;INPUT: An if statement and state, return, break, continue functions.
;OUTPUT: A new state after the execution of the if statement.
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

;Abstraction for defining the parts of an if statement 
(define condstmt cadr)
(define thenstmt caddr)
(define elsestmt cadddr)

;Takes an declaration statement and returns the state after the new declaration.
;INPUT: A declaration statement, state, and return functions.
;OUTPUT: A new state after the execution of the declaration, the declared variable is now put into the state.
(define Mstate_declaration-cps
  (lambda (statement state return)
    ;if it is a (var x) style
    (if (null? (aftersecondterm statement))
        ;adds a new one, not initialized. Remove the same name variable if there is any
        (return (add_declare_only (lhs statement) (remove_from_top_layer (lhs statement) state)))
        ;adds a new one with declared value. Removed the same variable previously first
        (return (add_with_assignment (lhs statement) (Mexpression (rhs statement) state) (remove_from_top_layer (lhs statement) state))))))

;Takes an assignment statement and returns the state after the assignment statement.
;INPUT: A assignment statement, state.
;OUTPUT: A new state after the execution of the assignment statement.
(define Mstate_assignment
  (lambda (stmt state)
    (Mstate_assignment-cps stmt state stdreturn)))


;Takes an assignment statement and returns the state after the assignment statement. A cps version of the assignment statement.
;INPUT: A assignment statement, state.
;OUTPUT: A new state after the execution of the assignment statement.

(define Mstate_assignment-cps
  (lambda (statement state return)
    ;if the variable is declared
    (return (update_var (lhs statement) (Mexpression (rhs statement) state) state))))

;Abstraction for the lefthand and righthand side of the assignment statement
(define lhs cadr)
(define rhs caddr)



;Mexpression takes an expression, or an statement with a return value, and returns the corresponding function to calculate the value.
;INPUT: An expression and a state.
;OUTPUT: A function.
(define Mexpression
  (lambda (expression state)
    (cond
      ;handle boolean or number values
      ((boolean? expression) (Mboolean expression state))
      ((number? expression) (Mvalue expression state))
      ;handle boolean values that are literals.s
      ((eq? expression 'true) (Mboolean #t state))
      ((eq? expression 'false) (Mboolean #f state))
      ;handle variable values
      ((atom? expression) (Mexpression (lookup expression state) state))
      ((and (pair? expression) (eq? (car expression) 'function)) (lookup expression state))
      ;handle numeric calculations
      ((member? (operator expression) '(+ - * / %)) (Mvalue expression state))
      ;handle function calls
      ((eq? (operator expression) 'funcall) (Mfuncall expression state))
      ;check for boolean operators
      ((member? (operator expression) '(|| && ! == != > < <= >=)) (Mboolean expression state))
      ;Operator is not supported by the current code values.
      (else (error 'expression_not_supported)))))


;-------------FUNCTION CLOSURES--------------

;Looks up a the function closure for a given function name in the state.
;INPUT: A function name and a state.
;OUTPUT: The respective closure of the function for the function name.
(define look_up_function_closure 
  (lambda (name state)
    ;creates a binding pair (function, name) to search for the function in the state.
    (lookup_value (cons 'function (cons name '()) state))))


; These abstractions are used to create the closure, they represent the respective parts of the function declaration statement.
(define func_name cadr)
(define parameter_list caddr)
(define func_body cadddr)


;Creates a closure, or the minimum things a function needs for declaration as a tuple of a (function name, parameter lists, function body, and a function to access the state at which it was stored)
;INPUT: A function declaration statement and a state.
;OUTPUT: The respective closure of the function, which can be put into the state.
(define create_closure
  (lambda (funcdecl state)
  ; (cons (func_name funcdecl) (cons (parameter_list funcdecl) (cons (func_body funcdecl) (cons (lambda (v) (keep_n_layers (count_layers state) v))'()))))))
  (append (cdr funcdecl) (cons (lambda (v) (keep_n_layers (count_layers state) v))'()))))

;These abstractions are used in the closure to extract parts of the closure for use
(define closure_func_name car)
(define closure_parameter_list cadr)
(define closure_body caddr)
(define closure_state_creation_function cadddr)


;Extracts the body of a function from its closure.
;INPUT: A function closure.
;OUTPUT: The respective body statement of that function closure.
(define look_up_closure_body 
  (lambda (closure)
    (closure_body closure)))


;Extracts the formal parameters of a function from its closure.
;INPUT: A function closure.
;OUTPUT: The respective formal parameters of that function closure.
(define look_up_formal_parameters
  (lambda (closure)
    (closure_parameter_list closure)))


;Extracts the function determining the state of the function when it was declared.
;INPUT: A function closure.
;OUTPUT: The respective function determining the state.
(define look_up_state_creation_function
  (lambda (closure) 
    (closure_state_creation_function closure)))



;Adds the function closure created into the state.
;INPUT: A function declaration, a state.
;OUTPUT: The state with the function closure added to it.
(define add_function_closure
  (lambda (functiondecl state)
    (add_with_assignment (cons 'function (cons (funcname functiondecl) '())) (create_closure functiondecl state) state)))

;Abstraction to determine the name of the function.
(define funcname cadr)

   

    


;-------------FUNCTION CALLS----------------------
(define get_func_name cadr)
(define get_actual_par cddr)
;Mfuncall takes a function call and produces its output.
;INPUT: A name and the parameters, as well as the current state
;OUTPUT: The result of function execution
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

;Takes an expression and calculates the value of that expression
;INPUT: An expression
;OUTPUT: A return value of the expression.
(define Mvalue
  (lambda (expression state)
    (cond
      ((boolean? expression) (error 'type_incompatible_number_expected_but_boolean_given))
      ((number? expression) expression)
      ((eq? expression 'true) (Mboolean #t state))
      ((eq? expression 'false) (Mboolean #f state))
      ((atom? expression) (Mvalue (lookup_value expression state) state))
      ((eq? 'funcall (operator expression)) (Mfuncall expression state))
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

;Returns the boolean value of an boolean expression
;INPUT: An boolean expression, a state
;OUTPUT: A boolean value
(define Mboolean
  (lambda (expression state)
    (cond
      ((number? expression) (error 'type_incompatible_boolean_expected_but_number_given))
      ((boolean? expression) expression)
      ((eq? expression 'true) (Mboolean #t state))
      ((eq? expression 'false) (Mboolean #f state))
      ((atom? expression) (Mboolean (lookup expression state) state))
      ((eq? (operator expression) 'funcall) (Mfuncall expression state))
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

;Abstractions for expressions
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)
(define aftersecondterm cddr)
(define secondterm cadr)
(define thirdterm caddr)
(define atom? (lambda (v) (not (pair? v))))








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
;Add_with_assignment creates a binding of a name to a value, and stores it into the state
;INPUT: A name, a corresponding value, and a state
;OUTPUT: The state after the assignment was completed
(define add_with_assignment
  (lambda (name value state)
    (add_to_top_layer name (box value) state)))

(define addvalue add_with_assignment)


;Add_with_assignment creates a binding of a name to a value, and stores it into the state
;INPUT: A name and a state
;OUTPUT: The state after the assignment was completed
(define add_declare_only
  (lambda (name state)
    (add_to_top_layer name (box '(null)) state)))

(define addvar add_declare_only)


;Adds to the top layer of a layer, or the outermost layer. Creates a binding of a name to a value, and stores it into the state
;INPUT: A name, a corresponding value, and a state
;OUTPUT: The state after the assignment was completed
(define add_to_top_layer
  (lambda (name value state)
    (switch_top_layer (add_without_block name value (fetch_top_layer state)) state)))

;Adds to a state without blocks. Creates a binding of a name to a value, and stores it into the state
;INPUT: A name, a corresponding value, and a state
;OUTPUT: The state after the assignment was completed
(define add_without_block
  (lambda (name value state)
    (combine (cons name (firstlist state)) (cons value (secondlist state)))))





;-------Remove ---------
;method to remove a variable from the an environment, and all attributes associated with it.

;Removes to the top layer of a layer, or the outermost layer. Takes a name and deletes it form the state
;INPUT: A name, and a state
;OUTPUT: The state after the deletion was completed.
(define remove_from_top_layer
  (lambda (name state)
    (switch_top_layer (remove_without_block name (fetch_top_layer state)) state)))


;Removes from a state with no blocks or layers. Deletes the name and value from the state.
;INPUT: A name, and a state
;OUTPUT: The state after the deletion was completed.
(define remove_without_block
  (lambda (n s)
    (remove_without_block-cps n s stdreturn)))

;Removes from a state with no blocks or layers. Deletes the name and value from the state. Same as above but with cps.
;INPUT: A name, and a state
;OUTPUT: The state after the deletion was completed.

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

;Looks up the value of the name in the state. If the return is ('null), it means that the variable was not assigned any value, which is an error. 
;If the return is #f, it means that the variable was not declared, which is an error.
;INPUT: A name, and a state
;OUTPUT: The value retrived from the name.
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

;Same as lookup but retrives the first instance of the name.
;INPUT: A name, and a state
;OUTPUT: The value retrived from the name.
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

;Abstractions

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


;Counts the number of layers in a state, zero indexed (base layer is layer 0).
;INPUT: A state.
;OUTPUT: The number of layers in the state.
(define count_layers
  (lambda (state)
    (- (count state) 2)))


;A function to remove the topmost d layers.
;INPUT: A state, and a number d to remove.
;OUTPUT: A state with the top most D layers removed.
(define remove_d_layers
  (lambda (d state)
    (cond
      ((zero? d) state)
      (else (remove_d_layers (- d 1) (blow_top_layer state))))))

;Keep_n_layers
;INPUT: a number n and a state
;OUTPUT: A state with the bottom most n layers only.
;Implementation: 
;Check for the layers of the input state, m
;Calculate d=m-n
;Remove d layers, remaining r=m-d=n layers
;Output the state with d layers removed

;A function to remove the bottom most n layers.
;INPUT: A state, and a number n of layers to keep.
;OUTPUT: A state with the bottom most n layers.
(define keep_n_layers
  (lambda (n state)
    ((lambda (m)
       (cond
         ((> n m) (error 'not_enough_layers_to_keep))
         (else (remove_d_layers (- m n) state)))) (count_layers state))))



;Function to determine whether there are blocks inside a state.
;INPUT: a state
;OUTPUT: #t if a state has blocks inside
(define hasblock?
  (lambda (state)
    (not (null? (deletefirsttwo state)))))

;Abstraction helpers
(define deletefirstthree cdddr)
(define deletefirsttwo cddr)
(define get_top_layer car)
(define except_top_layer cdr)

;Function to remove the top layer of a state.
;INPUT: A state.
;Output: A state with the top layer removed.
(define blow_top_layer
  (lambda (state)
    (blow_top_layer-cps state stdreturn)))


;Adds a new top layer to a state.
;INPUT: A state.
;OUTPUT: the state with a new top layer added.
(define enter_block
  (lambda (state)
    (cons newenvironment state)))

;Function to remove the top layer of a state, cps style.
;INPUT: A state.
;Output: A state with the top layer removed.
(define blow_top_layer-cps
  (lambda (state return)
    (cond
      ((hasblock? state) (return (cdr state)))
      (else (return state)))))

(define exit_block
  blow_top_layer-cps)


;Function to remove the top layer of a state.
;INPUT: A state.
;Output: The top layer of the state, if none, the state itself.
(define fetch_top_layer
  (lambda (state)
    (cond
      ((hasblock? state) (fetch_top_layer (get_top_layer state)))
      (else state))))

;in: a replacement layer, and a state
;out: the state with top layer replaced by the replacement layer
;---CPS-----

;Function to replace the top layer of the state.
;INPUT: A state.
;Output: A new state with the top layer of the state switched with a replacement layer.
(define switch_top_layer
  (lambda (replacement state)
    (switch_top_layer-cps replacement state stdreturn)))

;cps version of above
(define switch_top_layer-cps
  (lambda (replacement state return)
    (cond
      ((hasblock? state) (return (cons replacement (cdr state))))
      (else (return replacement)))))


;Function to find a target, or value, from a key.
;INPUT: A particular key, a list of a keys, a list of values.
;OUTPUT: The value corresponding to the particular key.
(define find
  (lambda (x key target)
    (cond
      ((null? key) '(notfound))
      ((null? target) (error 'targetistooshort))
      ;if x is a list, then use listeq to evaluate
      ((and (and (list? (car key)) (list? x)) (listeq? x (car key))) (car target))
      ((eq? x (car key)) (car target))
      (else (find x (cdr key) (cdr target))))))

;Determines whether two lists are identical.
;INPUT: Two lists.
;OUTPUT: A boolean to whether the lists are equal with each other.
(define listeq?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((eq? (car l1) (car l2)) (listeq? (cdr l1) (cdr l2)))
      (else #f))))

;New environment state
(define newenvironment '(()()))

;Attaches two lists together.
;INPUT: Two lists.
;OUTPUT: Two lists combined together.
(define combine
  (lambda (l1 l2)
    (cons l1 (cons l2 '()))))


(define remove_first
  (lambda (s)
    (combine (cdr (v_name s)) (cdr (v_value s)))))


;Function to see if an atom is in a list.
;INPUT: A list.
;OUTPUT: True or false, if the atom is in the list or not.
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
      ((< number 10) (interpret (string-append "t30" (number->string number) ".txt")))
      (else (interpret (string-append "t3" (number->string number) ".txt"))))))

  (define checkanswer
  (lambda (number)
    (if (eq? (test number) (find number fullset fullsetanswers))
        (string-append "test" (number->string number) "=PASSED expected " (number->string (find number fullset fullsetanswers)) " , given:" (number->string (test number)))
        (string-append "test" (number->string number) "=>INCORRECT: expected " (number->string (find number fullset fullsetanswers)) " , given:" (number->string (test number))))))



(define fullset        '( 1  2  3  4  6   8  9 10 11 13 14 15 16))
(define fullsetanswers '(10 14 45 55  115 20 24 2  35 90 69 87 64))

;(checkanswer 1) (checkanswer 2) (checkanswer 3) 
;(checkanswer 4) ;(checkanswer 5) 
;(checkanswer 6) (checkanswer 8) (checkanswer 9)
;(checkanswer 10) (checkanswer 11) (checkanswer 13) (checkanswer 14) (checkanswer 16)
                                                                                                              
