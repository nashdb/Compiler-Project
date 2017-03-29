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

let rec translate (Ast.Program procedures as pgm) = pgm (* YOUR CODE HERE *)

and
  translateProcedure (Ast.Procedure {id; formals; typ; body}) = (* FREE CODE! *)
  Ast.Procedure {id = id;
                 formals = formals;
                 typ = typ;
                 body = translateStatement body}

and
  translateStatement statement = statement (* YOUR CODE HERE *)

and
  translateTerm term =
  match term with
  | Ast.Id _ as i -> i
  | Ast.Literal _ as w -> w
  | Ast.If {expr; thn; els} -> term (* YOUR CODE HERE *)
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

  | Ast.App {rator; rands} -> term (* YOUR CODE HERE *)

  | Ast.Let {decl = Ast.ValBind {bv; defn}; body} -> term (* YOUR CODE HERE *)

  | Ast.Let {decl = Ast.FunBind {id; formals; typ; body = defBody};
             body = letBody} -> term (* YOUR CODE HERE *)
