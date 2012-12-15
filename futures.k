module FUTURES
  configuration <T color="green">
                  <k color="blue"> $PGM:Exp </k>
                  <env color="gray"> .Map </env>
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
  syntax LBody  ::= Exp
                  > Stmt Exp                   [strict(1)]
  syntax LExp   ::= "(" "let" "(" "[" Id Exp "]" ")" LBody ")" [strict(2)]
  syntax Stmt   ::= "(" "set!" Id Exp ")"
                  | "skip"
                  > Stmt Stmt                 [left]

  //Future-specific stuff
  syntax Thunk  ::= "(" "lambda" "()" Exp ")" 
  syntax FExp   ::= "(" "future" Thunk ")"    [strict]
  syntax TExp   ::= "(" "touch" Id ")"  
                  | "(" "touch" FExp ")"
  syntax Exp    ::= Id
                  | AExp 
                  | BExp
                  | FExp
                  | TExp
                  | LExp
  syntax Val    ::= Int | Bool | FExp 
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
  rule  <k> (set! X:Id V:Val) => . ... </k>
        <env> ... X |-> N ... </env>
        <store> ... N |-> (_ => V) ... </store>

  //Variable lookup
  rule  <k> X:Id => I ... </k>
        <env> ... X |-> ADDR ... </env>
        <store> ... ADDR |-> I ... </store>

  //Variable declaration
  rule  <k> (let ([X:Id V:Val]) Body:LBody) => Body ... </k>
        <env> Rho:Map => Rho[N/X] </env>
        <store> ... . => N |-> V </store>
    when fresh(N:Nat)

  rule S1 S2 => S1 ~> S2 [structural]

  //Sequential for now
  rule (lambda () E:Exp) => E 
  rule (future T:Thunk) => T

  //Always execute (touch (future ...))
  //sequentially?
  rule (touch F:FExp) => (F)

endmodule
