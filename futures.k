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
                  | "{" "+" Exp Exp "}"                       [seqstrict]
                  | "{" "-" Exp Exp "}"                       [seqstrict]
                  | "{" "*" Exp Exp "}"                       [seqstrict]
                  | "{" "/" Exp Exp "}"                       [seqstrict]
                  | "{" "if" Exp Exp Exp "}"                  [strict(1)]
                  | "{" "<=" Exp Exp "}"                      [seqstrict]
                  | "{" "not" Exp "}"                         [strict]
                  | "{" "and" Exp Exp "}"                     [strict(1)]
                  | "{" "let" "{" "[" Id Exp "]" "}" Exp "}"  [strict(2)]
                  | "{" "set!" Id Exp "}"                     [strict(2)]
                  | Void
                  | "{" "begin" Exp Exp "}"                   [strict(1)]
                  | "{" "lambda" "{}" Exp "}"
                  | "{" Exp "}"                               [strict]
                  | "{" "#%app" Thunk "}"                     [strict]
                  | "{" "future" Exp "}"                      [strict]
                  | "{" "touch" Exp "}"                       [strict]
                  | "{" "debug-print" Exp "}"                 [strict]
  syntax Void     ::= "{" "void" "}"

  syntax Thunk  ::= "thunk" Exp
  syntax Updatef ::= "updatef" Nat Exp                        [strict(2)]  
  syntax FDesc  ::= "fd" Nat                  
  syntax Barr   ::= "barricade"

  syntax Val    ::= Int | Bool | Void | FDesc | Thunk
  syntax KResult ::= Val

  rule <k> {debug-print I:Int} => {void} ... </k>
       <out> ... . => ListItem(I) ListItem("\n") </out>               [print-int] 
  rule <k> {debug-print true} => {void} ... </k>
       <out> ... . => ListItem("#t") ListItem("\n") </out>            [print-true]
  rule <k> {debug-print false} => {void} ... </k>
       <out> ... . => ListItem("#f") ListItem("\n") </out>            [print-false]
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
  rule <k> {/ I1:Int 0 } ... </k> => <k> "error: divBy0" </k>
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
       <store> ... N |-> (_ => V) ... </store>          [assignment]

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

  /*
  
  (begin 
    (future (lambda () (print "hello\n")))
    (print "world")) 


  rule <k> {/ I1:Int 0} => barricade ~> (/ I1:Int 0) ... </k>

  rule <k> barricade ~> {/ I1:Int 0} => "error: div0" </k>
        <future> ... <id> "runtime" </id> ... </future> 

  rule {/ I1:Int I2:Int} => I1 /Int I2 when I2 =/=Int 0
  rule {if true E1:AExp _} => E1

  //Variable assignment
  rule  <k> {set! X:Id V:Val} => void ... </k>
        <env> ... X |-> N ... </env>
        <store> ... N |-> {_ => V} ... </store>

  //Variable lookup
  rule  <k> X:Id => I ... </k>
        <env> ... X |-> ADDR ... </env>
        <store> ... ADDR |-> I ... </store>

  //Variable declaration
  rule  <k> {let ([X:Id V:Val]} Body:Exp) => Body ... </k>
        <env> Rho:Map => Rho[N/X] </env>
        <store> ... . => N |-> V </store>
    when fresh{N:Nat}

  //Evaluating multiple expressions
  rule  <k> V:Val ~> E Es:Exp => E ~> Es ... </k>
  rule <k> V:Val ~> .Exp => V ... </k>

  rule .Exp => .                           [structural]
  rule E:Exp ES:Exp => E ~> ES             [structural]

  //Application
  rule {lambda (} Es:Exp) => thunk Es
  rule {app (thunk Es:Exp}) => Es
  
  //If all futures are sequential...
  //rule {future T:Thunk} => (T)
  
  //Start a parallel future
  rule  <k> {future thunk Es} => fd N ... </k> <env> Rho </env>
        {. => <future> ... <k> Es </k> 
                <env> Rho </env>
                <id> N </id>
              </future>} 
    when fresh{N:Nat}

  //Always execute {touch (future ...}) sequentially?
  rule {touch (future TE:ThkExp}) => (TE)

  //Touch a future which has finished 
  //evaluating its thunk
  rule <k> {touch fd N:Nat} => V:Val ... </k> 
    <future> ... <k> V </k> <id> N </id> ... </future>

  //Touching on a worker thread barricades
  //rule  <k> {touch fd N:Nat} => barricade ~> (touch fd N:Nat) ... </k>
  //      <future> ... <id> N </id> ... </future>
  
  //Barricaded ft: need to match on id of the 
  //current config {"runtime"}, and also match on the k-cell
  //of the ft config with id N.  How?
  rule <future>
            <k> {touch fd N:Nat} => updatef N Es:Exp ... </k>
            <id> "runtime" </id>
       </future>
          <future>
            <k> barricade ~> Es ... </k>
            <id> N </id>
          </future>


  rule <future>
        <k> updatef N:Nat V:Val => V ... </k>
        <id> "runtime" </id>
       </future>
       <future>
        <k> barricade ~> Es:Exp ... </k> => <k> V </k>
        <id> N </id>
      </future>
  */
endmodule
