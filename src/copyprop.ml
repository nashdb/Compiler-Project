(*
 * file: copyprop.sml
 * author: Bob Muller
 * date: 4-12-2015.
 *
 * The copyprop transformation does the following:
 *
 * let x = e1 in let y = x in e2 =copyprop=> let x = e1 in e2[y:=x]
 *)

let substitute x y term =
  let rec translateTerm term =
    match term with
    | (Ast.Id z as i) ->
      (match Symbol.compare y z with
       | 0 -> Ast.Id x
       | _ -> i)
    | Ast.Literal _ as w -> w

    | Ast.If {expr; thn; els} ->
      Ast.If {expr = translateTerm expr;
              thn = translateTerm thn;
              els = translateTerm els}

(* The following two cases can't happen because Or and And were eliminated in
   the naming phase.
*)
    | Ast.Or _  -> raise (Failure "Cannot happen.")
    | Ast.And _ -> raise (Failure "Cannot happen.")

    | Ast.App {rator; rands} ->
      Ast.App {rator = rator; rands = List.map translateTerm rands}

    | Ast.Let {decl = Ast.ValBind {bv; defn}; body} ->
      Ast.Let {decl = Ast.ValBind {bv = bv; defn = translateTerm defn};
               body = translateTerm body}

    | Ast.Let {decl = Ast.FunBind {id; formals; typ; body = fbody};
               body = body} ->
      Ast.Let {decl = Ast.FunBind {id = id;
                                   formals = formals;
                                   typ = typ;
                                   body = translateTerm fbody};
               body = translateTerm body}
  in
  translateTerm term

let rec translate (Ast.Program procedures as program) =
  Ast.Program (List.map translateProcedure procedures) (* YOUR CODE HERE *)

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

  and translateTerm term = (* YOUR CODE HERE *)
    match term with
    | (Ast.Let {decl; body} as let1)->
      let body1 = body in
      let decl1 = decl in
       (match decl1 with
         | Ast.ValBind{bv = {id; typ}; defn} ->
         (match defn with
           | Ast.Id id1 -> let thisId = id in
             let subs = substitute id1 thisId body1 in translateTerm subs
          | _ -> Ast.Let{decl=decl; body = translateTerm body}
         )
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
