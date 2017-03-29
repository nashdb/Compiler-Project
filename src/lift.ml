(*
 * file: Lift.sml
 * author: Bob Muller
 * date: 1-1-2009.
 *
 * The Lift module implements a source-to-source transformation on
 * the nested let-expressions that may (or may not) have been introduced
 * in the naming phase. The transformation rule lifts inner let
 * expressions out. The transformation rule is:
 *
 * let x1 = (let x2 = e2 in e3) in e4
 *
 * is replaced by:
 *
 * let x2 = e2 in (let x1 = e3 in e4).
 *
 * Note that e2 may be a let-expression so the process iterates until
 * all let-expressions are lifted to top-level.
 *)

let rec translate (Ast.Program procedures) =
  Ast.Program (List.map translateProcedure procedures)

and translateProcedure (Ast.Procedure {id; formals; typ; body} as proc) = (* YOUR CODE HERE *)
  let newStatement = translateStatement body in
  let newProcedure = Ast.Procedure {id =id; formals= formals; typ = typ; body = newStatement} in
  newProcedure

and
  translateStatement statement = (* YOUR CODE HERE *)
  match statement with
  | Ast.Block {decls; statements} -> statement
  | Ast.Assign {id; expr} -> statement
  | Ast.While {expr; statement}->statement
  | _ -> statement
(*
  | IfS {expr; thn; els}->
  | Call {rator; rands} ->
  | Print term->
  | Return term->
  | Assign {id; expr} -> statement
  | While {expr; statement}->statement*)


and translateTerm term = (* YOUR CODE HERE *)
  match term with
  | Ast.Id t ->term
  | Ast.Literal {typ ; bits} ->term
  | Ast.App{rator ; rands} ->term
  | Ast.If{expr ; thn; els } ->term
  | Ast.And{left; right } ->term
  | Ast.Or {left; right } ->term
  | Ast.Let {decl; body } ->term
  | _ -> term
