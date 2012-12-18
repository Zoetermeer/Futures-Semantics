module FUTURES
  configuration <T color="purple">
                  <futures color="green">
                    <future multiplicity="*" color="pink">
                      <k color="blue"> $PGM:Exp </k>
                      <env color="gray"> .Map </env>
                      <id color="magenta"> "runtime" </id>
                    </future>
                  </futures>
                  <store color="red"> .Map </store>
                  <out stream="stdout"> .List </out>
                </T>

  syntax Exp   ::=  Int
                  | Id 
                  | Bool
                  | String
                  | "{" "+" Exp Exp "}"                               [seqstrict]
                  | "{" "-" Exp Exp "}"                               [seqstrict]
                  | "{" "*" Exp Exp "}"                               [seqstrict]
                  | "{" "/" Exp Exp "}"                               [seqstrict]
                  | "{" "if" Exp Exp Exp "}"                          [strict(1)]
                  | "{" "<=" Exp Exp "}"                              [seqstrict]
                  | "{" "not" Exp "}"                                 [strict]
                  | "{" "and" Exp Exp "}"                             [strict(1)]
                  | "{" "let" "{" "[" Id Exp "]" "}" Exp "}"          [strict(2)]
                  | "{" "set!" Id Exp "}"                             [strict(2)]
                  | Void
                  | "{" "begin" Exp Exp "}"                           [strict(1)]
                  | "{" "lambda" "{}" Exp "}"
                  | "{" Exp "}"                                       [strict]
                  | "{" "#%app" Thunk "}"                             [strict]
                  | "{" "future" Exp "}"                              [strict]
                  | "{" "touch" Exp "}"                               [strict]
                  | "{" "debug-print" Exp "}"                         [strict]
  syntax Void     ::= "{" "void" "}"

  syntax Thunk  ::= "thunk" Exp
  syntax Updatef ::= "updatef" Nat Exp                                [strict(2)]  
  syntax FDesc  ::= "fd" Nat                  
  syntax Barr   ::= "barricade"

  syntax Val    ::= Int | Bool | String | Void | FDesc | Thunk
  syntax KResult ::= Val

  rule <k> {debug-print I:Int} => {void} ... </k>
       <out> ... . => ListItem(I) ListItem("\n") </out>               [print-int] 
  rule <k> {debug-print true} => {void} ... </k>
       <out> ... . => ListItem("#t") ListItem("\n") </out>            [print-true]
  rule <k> {debug-print false} => {void} ... </k>
       <out> ... . => ListItem("#f") ListItem("\n") </out>            [print-false]
  rule <k> {debug-print S:String} => {void} ... </k>
       <out> ... . => ListItem(S) ListItem("\n") </out>               [print-string]
  rule <k> {debug-print V:Void} => {void} ... </k>
       <out> ... . => ListItem("#<void>") ListItem("\n") </out>       [print-void]
  rule <k> {debug-print F:FDesc} => {void} ... </k>
       <out> ... . => ListItem("#<future>") ListItem("\n") </out>     [print-future]
  rule <k> {debug-print T:Thunk} => {void} ... </k>
       <out> ... . => ListItem("#<procedure>") ListItem("\n") </out>  [print-thunk]

  rule {not B:Bool} => notBool B
  rule {+ I1:Int I2:Int } => I1 +Int I2
  rule {- I1:Int I2:Int } => I1 -Int I2
  rule {* I1:Int I2:Int } => I1 *Int I2
  rule {/ I1:Int I2:Int } => I1 /Int I2 when I2 =/=Int 0
  rule <future>...
          <k> {/ I1:Int 0} => barricade ~> {/ I1:Int 0} ... </k>
          <id> N:Nat </id>
       </future>
  rule {if true E1:Exp _} => E1
  rule {if false _ E2:Exp} => E2
  rule {<= I1:Int I2:Int} => I1 <=Int I2
  rule {not B:Bool} => notBool B
  rule {and false _} => false
  rule {and true E:Exp} => E
  rule <k> {let {[X:Id V:Val]} E:Exp} => E ... </k>
       <env> Rho:Map => Rho[N/X] </env>
       <store> ... . => N |-> V </store> 
    when fresh(N:Nat)

  rule <k> {set! X:Id V:Val} => {void} ... </k>
       <env> ... X |-> N ... </env>
       <store> ... N |-> (_ => V) ... </store>                        [assignment]

  //Lookup
  rule <k> X:Id => V ... </k>
       <env> ... X |-> ADDR ... </env>
       <store> ... ADDR |-> V ... </store>

  rule {begin V:Val E:Exp} => E
  rule {lambda {} E:Exp} => thunk E
  rule { (thunk E:Exp) } => E 

  rule <k> {future (thunk E:Exp)} => fd N ... </k> 
       <env> Rho </env>
       (. =>  <future>...
                <k> E </k>
                <env> Rho </env>
                <id> N </id>
              </future>)
    when fresh(N:Nat)

  rule <future>...
          <k> {touch (fd N:Nat)} => V:Val ... </k>
          <id> "runtime" </id>
       </future>
       <future>...
          <k> V </k>
          <id> N </id>
       </future>                                                      [touch-completed]

  rule <future>...
          <k> {touch (fd N:Nat)} => updatef N E ... </k>
          <id> "runtime" </id>
       </future>
       <future>...
          <k> barricade ~> E ... </k>
          <id> N </id>
       </future>                                                      [touch-barricaded]

  rule <future>
          <k> updatef N:Nat V:Val => V ... </k>
          <id> "runtime" </id>
       </future>
       <future>
          <k> _ ... </k> => <k> V </k>
          <id> N </id>
       </future>

  rule <future>...
          <k> {/ I1:Int 0} ... </k> => <k> "error: divBy0" </k>
          <id> "runtime" </id>
       </future>

endmodule
