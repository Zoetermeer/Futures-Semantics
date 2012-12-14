module FUTURES
  syntax AExp   ::= Int 
                  | "(" "+" AExp AExp ")"       [left, strict]
                  | "(" "-" AExp AExp ")"       [left, strict]
                  | "(" "*" AExp AExp ")"       [left, strict]
                  | "(" "/" AExp AExp ")"       [left, strict]
                  | "(" "if" BExp AExp AExp ")" [strict(1)]
  syntax BExp   ::= Bool
                  | "#t"
                  | "#f"
                  | "(" "<=" AExp AExp ")"      [left, strict]
                  | "(" "not" BExp ")"          [strict]
                  | "(" "and" BExp BExp ")"     [left, strict]
  syntax LExp   ::= "(" "let" "(" 
  syntax Thunk  ::= "(" "lambda" "()" Exp ")" 
  syntax FExp   ::= "(" "future" Thunk ")"
  syntax TExp   ::= "(" "touch" Id ")"  
                  | "(" "touch" FExp ")"
  syntax Exp    ::= AExp 
                  | BExp
                  | FExp
                  | TExp
  //syntax Stmt   ::= "(" Id Exp ")"              
  //                | "[" Id Exp "]"              
  //syntax Pgm ::= "(" "let" "(" Stmt ")" Exp ")" 

  syntax KResult ::= Int | Bool

  rule #t => true
  rule #f => false
  rule (not true) => false
  rule (not false) => true
  rule (+ I1:Int I2:Int ) => I1 +Int I2
  rule (- I1:Int I2:Int ) => I1 -Int I2
  rule (* I1:Int I2:Int ) => I1 *Int I2
  rule <k> (/ I1:Int 0) ...</k> => <k> "error" </k>
  //rule (/ I1:Int I2:Int) => I1 /Int I2 
  rule (if true E1:AExp _) => E1

  //Sequential for now
  rule (lambda () E:Exp) => E 
  rule (future T:Thunk) => T

  //Is this always true?
  rule (touch F:FExp) => (F)


endmodule
