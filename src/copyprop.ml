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

let rec translate (Ast.Program procedures as program) = program (* YOUR CODE HERE *)
