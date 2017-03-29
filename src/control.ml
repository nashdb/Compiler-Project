
open Ast

module Q = Quads

let getLabel maybeLabel =
  match maybeLabel with
  | None -> Label.fresh()
  | Some label -> label

let rec translate (Ast.Program procedures) =
  List.map translateProcedure procedures

and translateProcedure (Ast.Procedure {id; typ; formals; body}) =
  let name = Label.fromString (Symbol.format id) in
  let formals = List.map (fun bv -> bv.id) formals in
  let block' = translateStatement body
  in
  Q.Procedure {entry = name; formals = formals; code = block'}

and translateStatement ast = [] (* YOUR CODE HERE *)

and
  translateExpression expr = Q.Word {typ = Typ.Int; bits = 0} (* YOUR CODE HERE *)
