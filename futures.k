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
                  | "{" "let" "{" "{" Id Exp "}" "}" Exp "}"          [strict(2)]
                  | "{" "set!" Id Exp "}"                             [strict(2)]
                  | Void
                  | "{" "begin" Exp Exp "}"                           [strict(1)]
                  | "{" "lambda" "{}" Exp "}"
                  | "{" Exp "}"                                       [strict]
                  | "{" "future" Exp "}"                              [strict]
                  | "{" "touch" Exp "}"                               [strict]
                  | "{" "debug-print" Exp "}"                         [strict]
  syntax Void     ::= "{" "void" "}"

  syntax Closure  ::= "closure" Exp Map
  syntax Updatef ::= "updatef" Nat Exp                                [strict(2)]  
  syntax FDesc  ::= "fd" Nat                  
  syntax Barr   ::= "barricade"

  syntax Val    ::= Int | Bool | String | Void | FDesc | Closure
  syntax KResult ::= Val

  rule <k> {debug-print I:Int} => {void} ... </k>
       <out> ... . => ListItem(I) ListItem("\n") </out>               [transition] 
  rule <k> {debug-print true} => {void} ... </k>
       <out> ... . => ListItem("#t") ListItem("\n") </out>            [transition]
  rule <k> {debug-print false} => {void} ... </k>
       <out> ... . => ListItem("#f") ListItem("\n") </out>            [transition]
  rule <k> {debug-print S:String} => {void} ... </k>
       <out> ... . => ListItem(S) ListItem("\n") </out>               [transition]
  rule <k> {debug-print V:Void} => {void} ... </k>
       <out> ... . => ListItem("#<void>") ListItem("\n") </out>       [transition]
  rule <k> {debug-print F:FDesc} => {void} ... </k>
       <out> ... . => ListItem("#<future>") ListItem("\n") </out>     [transition]
  rule <k> {debug-print C:Closure} => {void} ... </k>
       <out> ... . => ListItem("#<procedure>") ListItem("\n") </out>  [transition]

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

  syntax K ::= restoreEnv(Map)

  rule <k> {let {{X:Id V:Val}} E:Exp} => E ~> restoreEnv(Rho) ... </k>
       <env> Rho:Map => Rho[ADDR/X] </env>
       <store> ... . => ADDR |-> V </store> 
    when fresh(ADDR:Nat)

  rule <k> V:Val ~> restoreEnv(Rho) => V ... </k>
       <env> _ => Rho </env>

  rule <k> {set! X:Id V:Val} => {void} ... </k>
       <env> ... X |-> ADDR ... </env>
       <store> ... ADDR |-> (_ => V) ... </store>                        [transition]

  //Lookup
  rule <k> X:Id => V ... </k>
       <env> ... X |-> ADDR ... </env>
       <store> ... ADDR |-> V ... </store>

  rule {begin V:Val E} => E
  rule <k> {lambda {} E} => closure E Rho ... </k>
       <env> Rho </env>
  rule <k> { (closure E Rho) } => E ... </k>
       <env> _ => Rho </env>

  rule <k> {future (closure E Rho)} => fd FID ... </k> 
       (. =>  <future>...
                <k> E </k>
                <env> Rho </env>
                <id> FID </id>
              </future>)
    when fresh(FID:Nat)

  rule <future>...
          <k> {touch (fd FID:Nat)} => V:Val ... </k>
       </future>
       <future>...
          <k> V </k>
          <id> FID </id>
       </future>                                                      [transition]

  rule <future>...
          <k> {touch (fd FID:Nat)} => updatef FID E ... </k>
          <id> "runtime" </id>
       </future>
       <future>...
          <k> barricade ~> E ... </k>
          <id> FID </id>
       </future>                                                      [transition]

  rule <future>
          <k> updatef FID:Nat V:Val => V ... </k>
          <id> "runtime" </id>
       </future>
       <future>
          <k> _ ... </k> => <k> V </k>
          <id> FID </id>
       </future>

  rule <future>...
          <k> {/ I1:Int 0} ... </k> => <k> "error: divBy0" </k>
          <id> "runtime" </id>
       </future>

endmodule
