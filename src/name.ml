(*
 * file: name.ml
 * author: Bob Muller
 * date: January 5, 2009
 * revised: March, 2017
 *
 * The Name module implements a source-to-source transformation for
 * naming the values of subterms in miniC. In addition to naming values,
 * the Name module translates "or" and "and" forms into special-cases
 * of the conditional form nested within a let-form.
 *)

let rec translate (Ast.Program procedures as pgm) = (* YOUR CODE HERE. I think we got this one fam *)
  Ast.Program(List.map translateProcedure procedures)



and
  translateProcedure (Ast.Procedure {id; formals; typ; body}) = (* FREE CODE! *)
  Ast.Procedure {id = id;
                 formals = formals;
                 typ = typ;
                 body = translateStatement body}

and
  translateStatement statement =  (* YOUR CODE HERE. I think we got this one fam *)
  match statement with
  | Ast.Block {decls; statements} -> Ast.Block {decls = decls; statements = (List.map translateStatement statements)}
  | Ast.Assign {id; expr} -> Ast.Assign {id = id; expr = translateTerm expr}
  | Ast.While {expr; statement} -> Ast.While {expr = translateTerm expr; statement = translateStatement statement}
  | Ast.IfS {expr; thn; els} -> Ast.IfS {expr = translateTerm expr; thn = translateStatement thn; els = translateStatement els}
  | Ast.Call {rator; rands} -> Ast.Call {rator = rator; rands = (List.map translateTerm rands)}
  | Ast.Print term -> Ast.Print (translateTerm term)
  | Ast.Return term -> Ast.Return (translateTerm term)

and
  translateTerm term =
  match term with
  | Ast.Id _ as i -> i
  | Ast.Literal _ as w -> w
  | Ast.If {expr; thn; els} ->  (* YOUR CODE HERE *)
    let x = Symbol.fresh() in
    let xterm = Ast.Id x in
    let bv = {Ast.id = x; typ = Typ.Bool} in
    Ast.Let {decl = Ast.ValBind {bv = bv; defn = translateTerm expr};
             body = Ast.If {expr = xterm;
                            thn = translateTerm thn;
                            els = translateTerm els}}

  | Ast.Or {left; right} ->         (* FREE CODE Removes OR *)
    let x = Symbol.fresh() in
    let xterm = Ast.Id x in
    let bv = {Ast.id = x; typ = Typ.Bool}
    in
    Ast.Let {decl = Ast.ValBind {bv = bv;
                                 defn = translateTerm left};
             body = Ast.If {expr = xterm;
                            thn = xterm;
                            els = translateTerm right}}

  | Ast.And {left; right} ->      (* FREE CODE Removes AND *)
    let x = Symbol.fresh() in
    let xterm = Ast.Id x in
    let bv = {Ast.id = x; typ = Typ.Bool}
    in
    Ast.Let {decl = Ast.ValBind {bv = bv;
                                 defn = translateTerm left};
             body = Ast.If {expr = xterm;
                            thn = translateTerm right;
                            els = xterm}}

  | Ast.App {rator; rands} -> unwindRands rator[] (List.rev rands) (* YOUR CODE HERE *)

  | Ast.Let {decl = Ast.ValBind {bv; defn}; body} ->
    Ast.Let {decl = Ast.ValBind {bv = bv; defn = translateTerm defn}; body = translateTerm body}(* YOUR CODE HERE *)

  | Ast.Let {decl = Ast.FunBind {id; formals; typ; body = defBody};
             body = letBody} ->
    Ast.Let {decl = Ast.FunBind { id = id; formals = formals; typ = typ; body = translateTerm defBody};
             body = translateTerm letBody}    (* YOUR CODE HERE *)
and
  unwindRands rator nRands oRands =  match oRands with
  | [] -> let x = Symbol.fresh() in
    let xterm = Ast.Id x in
    let bv = {Ast.id = x; typ = Typ.Int} in
    let app = Ast.App {rator = rator; rands = nRands} in
    Ast.Let {decl = Ast.ValBind {bv = bv; defn = app}; body = xterm}
  | rand :: rands ->
    let x = Symbol.fresh() in
    let xterm = Ast.Id x in
    let bv = {Ast.id = x; typ = Typ.Int} in
    Ast.Let {decl = Ast.ValBind {bv = bv; defn = translateTerm rand};
             body = unwindRands rator (xterm :: nRands) rands}
