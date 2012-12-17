module FUTURES
  configuration <T color="purple">
                  <futures color="green">
                    <future multiplicity="*" color="pink">
                      <k color="blue"> $PGM:Exp </k>
                      <env color="gray"> .Map </env>
                      <id color="magenta"> 0 </id>
                      <retval color="brown"> void </retval>
                    </future>
                  </futures>
                  <store color="red"> .Map </store>
                </T>

  syntax AExp   ::= Int
                  | Id 
                  | "(" "+" Exp Exp ")"       [left, strict]
                  | "(" "-" Exp Exp ")"       [left, strict]
                  | "(" "*" Exp Exp ")"       [left, strict]
                  | "(" "/" Exp Exp ")"       [left, strict]
                  | "(" "if" Exp Exp Exp ")"  [strict(1)]
  syntax BExp   ::= Bool
                  | "#t"
                  | "#f"
                  | Id
                  | "(" "<=" AExp AExp ")"    [left, strict]
                  | "(" "not" BExp ")"        [strict]
                  | "(" "and" BExp BExp ")"   [left, strict]
  
  //For now, `let' can only bind one id
  syntax Exps  ::= List{Exp,""}
  syntax LExp   ::= "(" "let" "(" "[" Id Exp "]" ")" Exps ")" [strict(2)]
  syntax SetExp ::= "(" "set!" Id Exp ")"     [strict(2)]
  syntax Void   ::= "void"

  //Future-specific stuff
  syntax Thunk  ::= "(" "lambda" "()" Exp ")" 
  syntax FExp   ::= "(" "future" Thunk ")"    
  syntax FDesc  ::= "fd" Nat                  
  syntax Barr   ::= "barricade"
  syntax TExp   ::= "(" "touch" Id ")"        [strict] 
                  | "(" "touch" FExp ")" 
                  | "(" "touch" FDesc ")"     [strict]
  syntax Exp    ::= Id
                  | AExp 
                  | BExp
                  | FExp
                  | TExp
                  | LExp
                  | SetExp
                  | Void
                  | FDesc
  syntax Val    ::= Int | Bool | Void | FDesc 
  syntax KResult ::= Val

  rule #t => true
  rule #f => false
  rule (not true) => false
  rule (not false) => true
  rule (+ I1:Int I2:Int ) => I1 +Int I2
  rule (- I1:Int I2:Int ) => I1 -Int I2
  rule (* I1:Int I2:Int ) => I1 *Int I2
  rule <k> (/ I1:Int 0) ...</k> => <k> "error: div0" </k>
  //rule (/ I1:Int I2:Int) => I1 /Int I2 
  rule (if true E1:AExp _) => E1

  //Variable assignment
  rule  <k> (set! X:Id V:Val) => void ... </k>
        <env> ... X |-> N ... </env>
        <store> ... N |-> (_ => V) ... </store>

  //Variable lookup
  rule  <k> X:Id => I ... </k>
        <env> ... X |-> ADDR ... </env>
        <store> ... ADDR |-> I ... </store>

  //Variable declaration
  rule  <k> (let ([X:Id V:Val]) Body:Exps) => Body ... </k>
        <env> Rho:Map => Rho[N/X] </env>
        <store> ... . => N |-> V </store>
    when fresh(N:Nat)

  //If a value at the front of the chain, 
  //stash as the current 'return value'
  rule  <k> V:Val ~> Es:Exps => Es ... </k>
        <retval> _ => V </retval>

  rule  <k> . => V:Val </k>
        <retval> V </retval>

  rule .Exps => .
  rule E:Exp ES:Exps => E ~> ES [structural]

  //Application
  rule (lambda () E:Exp) => E 
  
  //If all futures are sequential...
  //rule (future T:Thunk) => T
  
  rule  <k> (future T:Thunk) => fd N ... </k> <env> Rho </env>
        (. => <future> ... <k> T </k> 
                <env> Rho </env>
                <id> N </id>
              </future>) 
    when fresh(N:Nat)

  //Always execute (touch (future ...))
  //sequentially?
  rule (touch F:FExp) => (F)

  //Touch a future which has finished 
  //evaluating its thunk
  rule <k> (touch fd N:Nat) => V:Val ... </k> 
    <future> ... <k> V </k> <id> N </id> ... </future>

endmodule
