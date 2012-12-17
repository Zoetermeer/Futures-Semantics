module FUTURES
  configuration <T color="purple">
                  <futures color="green">
                    <future multiplicity="*" color="pink">
                      <k color="blue"> $PGM:Exp </k>
                      <env color="gray"> .Map </env>
                      <id color="magenta"> "runtime" </id>
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
  syntax Exps   ::= List{Exp,""}
  syntax LExp   ::= "(" "let" "(" "[" Id Exp "]" ")" Exps ")" [strict(2)]
  syntax SetExp ::= "(" "set!" Id Exp ")"     [strict(2)]
  syntax Void   ::= "void"

  //Future-specific stuff
  syntax ThkExp ::= "(" "lambda" "()" Exps ")"
  syntax Thunk  ::= "thunk" Exps
  syntax FExp   ::= "(" "future" ThkExp ")"    
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

  rule <k> (/ I1:Int 0) => barricade ~> (/ I1:Int 0) ... </k>

  rule <k> barricade ~> (/ I1:Int 0) => "error: div0" </k>
        <future> ... <id> "runtime" </id> ... </future> 

  rule (/ I1:Int I2:Int) => I1 /Int I2 when I2 =/=Int 0
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

  //Evaluating multiple expressions
  rule  <k> V:Val ~> Es:Exps => Es ... </k>
        <retval> _ => V </retval>

  rule  <k> . => V:Val </k>
        <retval> V </retval>

  rule .Exps => .                           [structural]
  rule E:Exp ES:Exps => E ~> ES             [structural]

  //Application
  rule (lambda () Es:Exps) => thunk Es
  rule (thunk Es:Exps) => Es
  
  //If all futures are sequential...
  //rule (future T:Thunk) => (T)
  
  rule  <k> (future TE:ThkExp) => fd N ... </k> <env> Rho </env>
        (. => <future> ... <k> (TE) </k> 
                <env> Rho </env>
                <id> N </id>
              </future>) 
    when fresh(N:Nat)

  //Always execute (touch (future ...)) sequentially?
  rule (touch (future TE:ThkExp)) => (TE)

  //Touch a future which has finished 
  //evaluating its thunk
  rule <k> (touch fd N:Nat) => V:Val ... </k> 
    <future> ... <k> V </k> <id> N </id> ... </future>

  //Touching on a worker thread barricades
  rule  <k> (touch fd N) => barricade ~> (touch fd N:Nat) ... </k>
        <future> ... <id> N:Nat </id> ... </future> 

  //Barricaded ft: need to match on id of the 
  //current config ("runtime"), and also match on the k-cell
  //of the ft config with id N.  How?
  rule <k> (touch fd N:Nat) => Es:Exps ... </k>
    <future> ... <k> barricade ~> Es ... </k> <id> N </id> ... </future>


endmodule
