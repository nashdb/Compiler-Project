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

and translateStatement statement = (* YOUR CODE HERE *)
  match statement with
  | Ast.Block {decls; statements} -> let newStates = listIterState statements in
    Ast.Block {decls = decls; statements = newStates}
  | Ast.While {expr; statement}->let newExpr = translateTerm expr in
    let newStatement = translateStatement statement in
    Ast.While{expr = newExpr; statement = newStatement}
  | Ast.IfS {expr; thn; els}-> let newExpr = translateTerm expr in
    let newThn = translateStatement thn in
    let newEls = translateStatement els in
    Ast.IfS {expr = newExpr; thn =newThn; els=newEls}
  | Ast.Call {rator; rands} -> let newRands = listIterTerm rands in
    Ast.Call{rator = rator; rands = newRands}
  | Ast.Print term-> let newTerm = translateTerm term in
    Ast.Print newTerm
  | Ast.Return term-> let newTerm = translateTerm term in
    Ast.Return newTerm
  | Ast.Assign {id; expr} -> let newExrp = translateTerm expr in
    Ast.Assign{id; expr = newExrp}
  | _ -> statement

and listIterTerm mylist =
  match mylist with
  | [] -> []
  | a :: newList -> let newA = translateTerm a in
    newA:: listIterTerm newList

and listIterState mylist =
  match mylist with
  | [] -> []
  | a :: newList -> let newA = translateStatement a in
    newA:: listIterState newList
 (*
   | IfS {expr; thn; els}->
   | Call {rator; rands} ->
   | Print term->
   | Return term->
   | Assign {id; expr} -> statement
   | While {expr; statement}->statement*)


and translateTerm term = (* YOUR CODE HERE *)
  match term with
  | (Ast.Let {decl; body} as let1)->
    let body1 = body in
    let decl1 = decl in
     (match decl1 with
     | Ast.ValBind{bv; defn} ->
       (match defn with
        | (Ast.Let {decl; body} as let2)-> let body2 = body in
          let decl2 = decl in
          let newDecl = Ast.ValBind{bv= bv; defn = body2} in
          let newBody = Ast.Let{decl = newDecl; body = body1} in
          let newAst = Ast.Let{decl= decl2; body = newBody} in
          translateTerm newAst
        | _ -> Ast.Let{decl=decl; body = translateTerm body})
    | _ -> Ast.Let{decl=decl; body = translateTerm body} (*){decl= ValBin { bv; def = Let}}*)
    )
  | Ast.Id t -> term
  | Ast.Literal {typ ; bits} -> term
  | Ast.App{rator ; rands} -> let newTerms = listIterTerm rands in
    Ast.App{rator; rands = newTerms}
  | Ast.If{expr ; thn; els } -> let newExpr = translateTerm expr in
    let newThn = translateTerm thn in
    let newEls = translateTerm els in
    Ast.If{expr = newExpr ; thn = newThn ; els= newEls}
  | Ast.And{left; right } -> let newLeft = translateTerm left in
    let newRight = translateTerm right in
    Ast.And{left = newLeft; right = newRight}
  | Ast.Or {left; right } -> let newLeft = translateTerm left in
    let newRight = translateTerm right in
    Ast.Or{left = newLeft; right = newRight}
  | _ -> term

(*  | Ast.Id t ->term
    | Ast.Literal {typ ; bits} ->term
    | Ast.App{rator ; rands} ->term
    | Ast.If{expr ; thn; els } ->term
    | Ast.And{left; right } ->term
    | Ast.Or {left; right } ->term*)
