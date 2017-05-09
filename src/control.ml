
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

and translateStatement ast =
  match ast with
  | Ast.Block {decls; statements} -> let newStateList = listIterState statements in newStateList
  | Ast.While {expr; statement}->let (newExpr, x) = translateExpression expr in
    let newState = translateStatement statement in
    let l1 = getLabel None in
    let noop1 = Q.Instruction{label = Some (l1); op = Q.Noop} in
    let l2 = getLabel None in
    let noop2 = Q.Instruction{label = Some (l2); op = Q.Noop} in
    let jmp1 = Q.Instruction{label = None; op = Q.JmpZero{cond= Q.Operand(x);dest =l2}}
    in
    let jmp2 = Q.Instruction{label = None; op = Q.Jmp l1} in
    let first = noop1::[] @ newExpr @ jmp1:: [] in
    let second = jmp2 :: noop2 :: [] in
    first @ newState @ second
    (*noop1 :: newExpr :: jmp1:: newState:: jmp2:: noop2::[]*)
  | Ast.IfS {expr; thn; els}->let (newExpr, x) = translateExpression expr in
    let newThn = translateStatement thn in
    let newEls = translateStatement els in
    let l1 = getLabel None in
    let noop1 = Q.Instruction{label = Some (l1); op = Q.Noop} in
    let l2 = getLabel None in
    let noop2 = Q.Instruction{label = Some (l2); op = Q.Noop} in
    let jmp1 = Q.Instruction{label = None; op = Q.JmpZero{cond=Q.Operand(x);dest =l1}}  in
    let jmp2 = Q.Instruction{label = None; op = Q.Jmp l2} in
    let first = newExpr @ jmp1:: [] in
    let second = jmp2:: noop1 :: [] in
    let third = noop2::[] in
    first @ newThn @ second @ newEls @ third
    (*newExpr:: jmp1:: newThn:: jmp2:: noop1:: newEls::noop2::[]*)
    (*newExpr :: Q.Instruction{label = None; op = Q.JmpZero{cond= Q.Operand(Q.Word {typ = Typ.Int; bits = 0}); dest= l2}} :: newThn :: Q.Instruction{label = None; op  = Q.Jmp l2}:: newEls :: Q.Instruction{label = l2; op = Q.Noop} :: []*)

  | Ast.Return term-> let (newExpr, x) = translateExpression term in
    newExpr @ Q.Instruction{label = None; op = Q.Ret (x)} :: []

  | Ast.Assign {id; expr} ->let oper = Q.Id id in
    let (newExp, x) = translateExpression expr in
    newExp @ Q.Instruction{label = None; op = Q.Gets{dst = oper; src = Q.Operand(x)}} :: []
  | Ast.Call {rator; rands} ->
    (match rands with
     | a :: b:: [] -> let (in1, a1) = translateExpression a in
       let (in2, a2) = translateExpression b in
       in1 @ in2 @ Q.Instruction{label = None; op = Q.Call{label = getLabel None; opnds = a1 :: a2:: []}} :: []
     | _ -> Q.Instruction{label = Some (getLabel None); op =Noop} :: [])
  | _ ->[] (* YOUR CODE HERE *)

and listIterState mylist =
  match mylist with
  | [] -> []
  | a :: newList -> let newA = translateStatement a in
    newA @ listIterState newList

and listIterTerm myTerms =
  match myTerms with
  | [] -> ([], [])
  | a :: myTerms -> let (newList, newWord) = translateExpression a in
    let (newList2, newWords2) = listIterTerm myTerms in
    (newList @ newList2, newWord :: newWords2)

and translateExpression expr =
  let instruct = Q.Instruction{label = None; op = Q.Noop} in
  let instructList = instruct :: instruct :: [] in
  let word = Q.Word {typ = Typ.Int; bits = 0} in
  match expr with
  | Ast.Id t -> let opera =  Q.Id t in
    ([], opera)
  | Ast.App {rator; rands} -> let symb = Symbol.fresh() in
    let isOp = Bases.isPrim rator in
    (match isOp with
     | true -> (match rands with
         | a :: b:: [] -> let (ins1, word1) = translateExpression a in
           let (ins2, word2) = translateExpression b in
           let binop = Q.BinPrimOp{op = rator; opnds = {src1 = word1;src2= word2}} in
           let thisIns = Q.Instruction{label = None; op = Q.Gets{dst= Q.Id symb; src = binop}}
           in (ins1 @ ins2 @ thisIns :: [], Q.Id symb)
         | a :: [] -> let (ins1, word1) = translateExpression a in
           let unop = Q.UnPrimOp{op = rator; opnd = word1} in
           let thisIns = Q.Instruction{label = None; op = Q.Gets{dst= Q.Id symb; src = unop}}
           in (ins1 @ thisIns :: [], Q.Id symb)
         | _ -> ([], word))
     | _ -> let (myInstructs, myWords) = listIterTerm rands in
       let str = Symbol.format (rator) in
       let funcall = Q.FunCall{label= Label.fromString (str); opnds = myWords} in
       let thisIns = Q.Instruction{label = None; op = Q.Gets{dst = Q.Id symb; src = funcall}} in
       (myInstructs @ thisIns :: [], Q.Id symb)(*)
    (match rands with
     | a :: b:: [] -> let (ins1, word1) = translateExpression a in
       let (ins2, word2) = translateExpression b in
       let binop = Q.BinPrimOp{op = rator; opnds = {src1 = word1;src2= word2}} in
       let thisIns = Q.Instruction{label = None; op = Q.Gets{dst= Q.Id symb; src = binop}}
       in (ins1 @ ins2 @ thisIns :: [], Q.Id symb)
     | a :: [] -> let (ins1, word1) = translateExpression a in
       let unop = Q.UnPrimOp{op = rator; opnd = word1} in
       let thisIns = Q.Instruction{label = None; op = Q.Gets{dst= Q.Id symb; src = unop}}
           in (ins1 @ thisIns :: [], Q.Id symb)
              | _ -> (instructList, word))*)
    )
  | Ast.If {expr; thn; els} ->let (newExpr, x) = translateExpression expr in
    let (newThn, y) = translateExpression thn in
    let (newEls, z) = translateExpression els in
    let l1 = getLabel None in
    let noop1 = Q.Instruction{label = Some (l1); op = Q.Noop} in
    let l2 = getLabel None in
    let noop2 = Q.Instruction{label = Some (l2); op = Q.Noop} in
    let jmp1 = Q.Instruction{label = None; op = Q.JmpZero{cond= Q.Operand (Q.Word {typ = Typ.Int; bits = 0});dest =l1}}  in
    let jmp2 = Q.Instruction{label = None; op = Q.Jmp l2} in
    let first = newExpr @ jmp1:: [] in
    let second = jmp2:: noop1 :: [] in
    let third = noop2::[] in
    (first @ newThn @ second @ newEls @ third, word)
  | Ast.Literal {typ; bits} ->let opera =  Q.Word{typ= typ; bits = bits} in
    ([], opera)
  | Ast.Let{decl; body} ->
    let (newBody, newBodAns) = translateExpression body in
    (match decl with
     | Ast.ValBind {bv={id; typ}; defn} ->
       let (insList, a) = translateExpression defn in
       (match insList with
        |  [] -> ( Q.Instruction{label = None; op = Q.Gets{dst = Q.Id id; src = Q.Operand(a)}} :: [] @ newBody, newBodAns)
        | _ -> (insList @ Q.Instruction{label = None; op = Q.Gets{dst = Q.Id id; src = Q.Operand(a)}} :: [] @ newBody, newBodAns))

     | Ast.FunBind {id; formals; typ; body} ->let (insList,a) = translateExpression body in
       let tempLab  = Label.fromString ("thisFunction") in
       let myget = Q.Gets{dst = Q.Id id; src = Q.FunCall{label= tempLab; opnds= Q.Id id :: []}} in
       (insList @ Q.Instruction{label = Some (getLabel None); op = myget} :: [], a)
    )
  | _ -> ([], word)
              (*)
  | Literal {typ; bits}-> (Q.Instruction{label = None; op = Q.Noop} :: [], None)
  | App {rator; rands}  -> (Q.Instruction{label = None; op = Q.Noop} :: [], None)  (* term is an identifier *)
  | If {expr; thn; els}->  (Q.Instruction{label = None; op = Q.Noop} :: [], None)
  | And {left; right}-> (Q.Instruction{label = None; op = Q.Noop} :: [], None)
  | Or {left; right}-> (Q.Instruction{label = None; op = Q.Noop} :: [], None)
  | Let  {decl; body} -> (Q.Instruction{label = None; op = Q.Noop} :: [], None) (* YOUR CODE HERE  Q.Word {typ = Typ.Int; bits = 0}pair (code, where it is)*)
              *)
