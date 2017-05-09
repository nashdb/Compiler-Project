(*
 * file: codegen.ml
 * author: Bob Muller
 * date: 3-14-2009
 * revised: April, 2015
 * revised; March, 2017
 *
 * This file contains code for inserting control in the compilation
 * of mini-C. This code translates from the language of quads to
 * MIPS assembly code. The calling protocols is as follows:
 *
 * Register Conventions:
 *  t0 - t9 : caller-save regs
 *  s0 - s7 : callee-save regs
 *  fp : frame pointer
 *  ra : return address
 *  a0 - a3 : argument registers for first 4 arguments
 *  v0 - v1 : return values
 *
 * 1. Stack Frame Layout:

       +----------------------------+
       |      Caller-Save Regs      |                 higher addresses
       +----------------------------+
       |       Value of Arg 1       |
       +--           :            --+
       |             :              |
       +--                        --+
       |       Value of Arg n       |
       +----------------------------+
       |       Return Address       |
       +----------------------------+
       |   Caller's Frame Pointer   | <-- fp
       +----------------------------+
       |      Local Variable 1      |
       +--            :           --+
       |              :             |
       +--                        --+
       |      Local Variable k      |
       +----------------------------+
       |   Callee Save Registers    |                 lower addresses
       +----------------------------+
                                      <-- sp

 *)

open Environment

(*
 * Some abbreviations for frequently used modules.
 *)
module Q = Quads
module E = Environment
module Opn = Mips.Operation
module Opnd = Mips.Operand
module I = Mips.Instruction
module CS = Mips.Codestream

module B = Bases.Compiler

let makeLabel = Label.fromString

let fIL = CS.fromInstructionList
let toIn = I.toInstruction

(* Conventions
 *
 *   This section binds names to enforce some code generator conventions.
 *
 * Word size is 4 bytes.
 *)
let wordSize = 4

(* Conventions for system calls on MARS. These codes go in $v0.
 *)
let syscallPrintInt = 1
let syscallReturn = 10

(* accumulator register used to accumulate results.
 *)
let accumulator =  Opnd.Value 0
let operand1Reg =  Opnd.Temp 1
let operand2Reg =  Opnd.Temp 2

(* dataBaseAddr is the register that is used to to hold the base
 * address of the data area. All variables accessed via indirect
 * addressing off of this register.
 *)
let dataBaseAddr = Opnd.Temp 1

(* targetLabelReg is the number of the register that is used to
 * hold the destination of a branch.
 *)
let targetLabelReg = Opnd.Temp 2

(* The registers to be used for operands of two-argument operations.
 *)
let firstOpndReg =  Opnd.Temp 3
let secondOpndReg = Opnd.Temp 4

(* The makeJumpStream function generates code for branching to
 * a label after testing a particular register.
 *
 * NB: The code overwrites register the targetLabelReg.
 *)
let makeJumpStream(testReg, destLabel) =
  let branch = toIn(None,
                    Opn.Beqz {rs = testReg; off18 = Opnd.Label destLabel},
                    Some "branch")
  in
    fIL [branch]

let push reg maybeLabel comment =
  let i1 = toIn(maybeLabel,
                Opn.Addi {rd = Opnd.Reg(Opnd.StackPointer);
                          rs = Opnd.Reg(Opnd.StackPointer);
                          const16 = Opnd.Const16 (-4)},
			          Some ("push " ^ comment)) in
  let i2 = toIn(None,
                Opn.Sw {rs = reg;
                        rt = Opnd.Indirect {offset = Some 0;
                                            reg = Opnd.StackPointer}},
                None)
  in
  fIL [i1; i2]

let pop dstreg maybeLabel comment=
  let i1 = toIn(None,
                Opn.Lw {rd = dstreg;
                        rs = Opnd.Indirect {offset = Some 0;
                                            reg = Opnd.StackPointer}},
			   Some ("pop " ^ comment)) in
  let i2 = toIn(None,
                Opn.Addi {rd = Opnd.Reg Opnd.StackPointer;
                          rs = Opnd.Reg Opnd.StackPointer;
                          const16 = Opnd.Const16 4},
                None)
  in
  fIL [i1; i2]

let makePrologue name nLocals =
  let pushFP = push (Opnd.Reg Opnd.FramePointer) (Some name) "fp" in
  let sp2fp = toIn(None, Opn.Move {rd = Opnd.Reg Opnd.FramePointer;
                                   rs = Opnd.Reg Opnd.StackPointer},
                   Some "fp <- sp") in
  let allocate = toIn(None,
                      Opn.Addi {rd = Opnd.Reg Opnd.StackPointer;
                                rs = Opnd.Reg Opnd.StackPointer;
                                const16 = Opnd.Const16 (wordSize * nLocals)},
                      Some "allocate locals")
  in
    CS.concat pushFP (fIL [sp2fp; allocate])

let makeEpilogue() =
  let i0 = toIn(None,
                Opn.Move {rd = Opnd.Reg (Opnd.Arg 0);
                          rs = Opnd.Reg accumulator},
                Some "$a0 gets answer for syscall") in
  let i1 = toIn(None,
                Opn.Li {rd = Opnd.Reg(Opnd.Value 0);
                        imm32 = Opnd.Const32 syscallPrintInt},
                Some "$v0 gets print_int code for syscall") in
  let i2 = toIn(None, Opn.Syscall, Some "Print the result") in
  let i3 = toIn(None,
                Opn.Li {rd = Opnd.Reg(Opnd.Value 0);
                        imm32 = Opnd.Const32 syscallReturn},
			          Some "Load return status") in
  let i4 = toIn(None, Opn.Syscall, None)
  in
  fIL [i0; i1; i2; i3; i4]

(* makeEnv constructs an environment for a given procedure. The
 * environment maps each identifier to its storage offset as suggested
 * by the picture at the top of the file. In particular, all variables
 * are accessed via indirect addressing using the frame pointer (fp) as
 * the base address. Formal parameters will have positive offsets while
 * local variables will have negative offsets.
 *)
let makeEnvOpnd env offset opnd =
  match opnd with
  | Q.Id id -> (match E.mem id env with
	| true  -> (env, offset)
  | false -> (E.add id offset env, offset - 1))
  | Q.Word _ -> (env, offset)

let makeEnvRHS env offset rhs =
  match rhs with
  | Q.Operand opnd -> makeEnvOpnd env offset opnd

  | Q.BinPrimOp {op; opnds = {Q.src1 = opnd1; src2 = opnd2}} ->
     let (env', offset') = makeEnvOpnd env offset opnd1
     in
     makeEnvOpnd env' offset' opnd2

  | Q.UnPrimOp {op; opnd} -> makeEnvOpnd env offset opnd

  | Q.FunCall {label; opnds} ->
    List.fold_right
      (fun opnd -> (fun (env, offset) -> makeEnvOpnd env offset opnd))
      opnds
      (env, offset)

let makeEnvOperation env offset opn =
  match opn with
  | Q.Gets {dst; src} ->
     let (env', offset') = makeEnvOpnd env offset dst
     in
     makeEnvRHS env' offset' src

  | Q.JmpZero {cond; dest} -> makeEnvRHS env offset cond

  | Q.Call {label; opnds} ->
     List.fold_right (fun opnd -> (fun (env, offset) -> makeEnvOpnd env offset opnd)) opnds (env, offset)

  | Q.Ret(opnd) -> makeEnvOpnd env offset opnd

  | other -> (env, offset)

let makeEnvInstruction env offset (Q.Instruction {label; op}) =
  makeEnvOperation env offset op

(* makeEnv makes an environment with an entry for each identifier. Identifiers that
   are formals are given positive offsets (see the picture above), identifiers that
   are local variables are given negative offsets.
*)

let makeEnvInstructions env os i =
  makeEnvInstruction env os i

let makeEnv formals instructions =  (* YOUR CODE HERE *)
  let rec listIter formals offset env=
    match formals with
    | [] -> env
    | a :: newList -> let newoff = offset + 1 in
      let newEnv = E.add a offset env in
      listIter newList newoff newEnv in
  let env = listIter (List.rev formals) 2 E.empty in
  List.fold_right (fun i -> (fun (env, os) -> makeEnvInstruction env os i)) instructions (env, -1)

let loadRegister reg env opnd maybeLabel =
  match opnd with
  | Q.Id name ->
    let o = E.find name env in
    let offset = wordSize * o
    in
    toIn(maybeLabel,
         Opn.Lw {rd = Opnd.Reg reg;
                 rs = Opnd.Indirect {offset = Some offset;
                                     reg = Opnd.FramePointer}},
         None)

  | Q.Word {typ; bits} ->
    toIn(maybeLabel,
         Opn.Li {rd = Opnd.Reg reg; imm32 = Opnd.Const32 bits},
         None)

let storeRegister reg env opnd maybeLabel =
  match opnd with
  | Q.Id name ->
    let o = E.find name env in
    let offset = wordSize * o
    in
    toIn(maybeLabel,
         Opn.Sw {rs = Opnd.Reg reg;
                 rt = Opnd.Indirect {offset = Some offset;
                                     reg = Opnd.FramePointer}},
         None)
  | _ -> failwith "storeRegister: bad store operand"

let translateOperand reg env opnd maybeLabel =
  loadRegister reg env opnd maybeLabel             (* YOUR CODE HERE *)



let translateRHS env rhs maybeLabel =
  match rhs with
  | Q.Operand op-> let newOp = translateOperand accumulator env op maybeLabel in
    fIL [newOp]
  | Q.BinPrimOp {op; opnds = {src1; src2}}->
    let newSrc1 = translateOperand operand1Reg env src1 maybeLabel in
    let newSrc2 = translateOperand operand2Reg env src2 maybeLabel in
    (match E.find op B.dynamicBasis with
     | B.CodeGenerator codegen -> let newOp = codegen [Opnd.Reg accumulator; Opnd.Reg operand1Reg; Opnd.Reg operand2Reg]
       in CS.concat (fIL [newSrc1; newSrc2]) newOp
     )
  | Q.UnPrimOp {op; opnd}-> let newOp = translateOperand operand1Reg env opnd maybeLabel in
    (match E.find op B.dynamicBasis with
        | B.CodeGenerator codgen ->
          let performOp = codgen [Opnd.Reg accumulator; Opnd.Reg operand1Reg]
          in CS.concat (fIL [newOp]) performOp
    )
  | Q.FunCall {label; opnds} ->
    let opPush op=
      let a = fIL[translateOperand operand1Reg env op None] in
      let b = push(Opnd.Reg operand1Reg) None ""
      in CS.concat a b
    in let mylist = List.map opPush opnds in
    let opPush1 = List.fold_left CS.concat CS.empty mylist in
    let return = push (Opnd.Reg Opnd.ReturnAddress) None "" in
    let jump = fIL[I.toInstruction(None, Opn.Jal (Opnd.Label label), None)] in
    let pop= pop(Opnd.Reg (Opnd.ReturnAddress)) None "" in
    let n = List.length opnds in
    let args = fIL[I.toInstruction(None, Opn.Addi{rd = Opnd.Reg Opnd.StackPointer; rs = Opnd.Reg Opnd.StackPointer; const16 = Opnd.Const16 (wordSize * n)}, None)] in
    CS.concat opPush1 (CS.concat return (CS.concat jump(CS.concat pop args)))
        (* YOUR CODE HERE *)

let translateOperation env opn maybeLabel =
  match opn with
  | Q.Gets {dst; src} -> let newSrc = translateRHS env src maybeLabel in
    let newDst = translateOperand accumulator env dst maybeLabel in
    CS.concat newSrc( fIL[newDst])
  | Q.Jmp label-> fIL [I.toInstruction(maybeLabel, Opn.J(Opnd.Label label), None)]
  | Q.JmpZero {cond; dest} -> let newrhs = translateRHS env cond maybeLabel in
    fIL [I.toInstruction(None, Opn.Nop, None)]
  | Q.Call {label; opnds} ->
    let opPush op=
       let a = fIL[translateOperand operand1Reg env op None] in
       let b = push(Opnd.Reg operand1Reg) None ""
       in CS.concat a b
    in let mylist = List.map opPush opnds in
    let opPush1 = List.fold_left CS.concat CS.empty mylist in
    let return = push (Opnd.Reg Opnd.ReturnAddress) None "" in
    let jump = fIL[I.toInstruction(None, Opn.Jal (Opnd.Label label), None)] in
    let pop= pop(Opnd.Reg (Opnd.ReturnAddress)) None "" in
    let n = List.length opnds in
    let args = fIL[I.toInstruction(None, Opn.Addi{rd = Opnd.Reg Opnd.StackPointer; rs = Opnd.Reg Opnd.StackPointer; const16 = Opnd.Const16 (wordSize * n)}, None)] in
    CS.concat opPush1 (CS.concat return (CS.concat jump(CS.concat pop args)))
  | Q.Ret op -> let newOp = translateOperand accumulator env op maybeLabel in
    let newIns = I.toInstruction(None, Opn.Move{rd = Opnd.Reg Opnd.StackPointer; rs = Opnd.Reg Opnd.FramePointer}, None) in
    let newIns2 = I.toInstruction(None, Opn.Jr(Opnd.Reg Opnd.ReturnAddress), None) in
    fIL [newOp; newIns; newIns2]
  | Q.Noop -> fIL [I.toInstruction(maybeLabel, Opn.Nop, Some "")]       (* YOUR CODE HERE *)

let translateInstruction env (Q.Instruction {label = maybeLabel;
                                             op = opn} : Q.instruction) : CS.t =
  translateOperation env opn maybeLabel                                       (* YOUR CODE HERE *)

let rec instrucIter code env=
  match code with
  | [] -> []
  | a :: morecode -> let ins = translateInstruction env a in
    ins :: (instrucIter morecode env)

let translateProcedure (Q.Procedure {entry; formals; code}) =
  let (env, locals) = makeEnv formals code in
  let empty = Debug.dumpCGEnv env in
  let mylist = instrucIter code env in
  let prologue = makePrologue entry locals
  in CS.concat prologue (List.fold_left CS.concat CS.empty
                           mylist)

 (* YOUR CODE HERE *)

(*
 * The main function. It leaves the result of evaluating and expression in
 * the accumulator.
*)
let rec procedureIter procedures =
  match procedures with
  | [] -> []
  | a :: b -> let newP = translateProcedure a in
    newP :: (procedureIter b)

let translate procedures =
  let thisOpen = I.toInstruction(None, Opn.Data, None) in
  let thisText = I.toInstruction(None, Opn.Text, None) in
  CS.concat(fIL[thisOpen; thisText]) (List.fold_left CS.concat CS.empty (procedureIter procedures))


(* YOUR CODE HERE *)
