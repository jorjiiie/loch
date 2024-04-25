open Printf
open Pretty
open Phases
open Exprs
open Assembly
open Errors

(* Documentation can be found at https://v2.ocaml.org/api/Set.S.html *)
module StringSet = Set.Make (String)

(* Documentation can be found at https://v2.ocaml.org/api/Map.S.html *)
module StringMap = Map.Make (String)
module IntMap = Map.Make (Int)

type 'a envt = (string * 'a) list

(* You might also find
        type 'a envt = 'a StringMap.t
   to be useful instead of a simple list of pairs. *)

let rec is_anf (e : 'a expr) : bool =
  match e with
  | EPrim1 (_, e, _) -> is_imm e
  | EPrim2 (_, e1, e2, _) -> is_imm e1 && is_imm e2
  | ELet (binds, body, _) ->
      List.for_all (fun (_, e, _) -> is_anf e) binds && is_anf body
  | EIf (cond, thn, els, _) -> is_imm cond && is_anf thn && is_anf els
  | _ -> is_imm e

and is_imm e =
  match e with
  | ENumber _ -> true
  | EBool _ -> true
  | EId _ -> true
  | _ -> false

let const_true = HexConst 0xFFFFFFFFFFFFFFFFL
let const_false = HexConst 0x7FFFFFFFFFFFFFFFL
let bool_mask = HexConst 0x8000000000000000L
let bool_tag = 0x000000000000000fL
let bool_tag_mask = 0x000000000000000fL
let num_tag = 0x0000000000000000L
let num_tag_mask = 0x0000000000000001L
let closure_tag = 0x0000000000000005L
let closure_tag_mask = 0x000000000000000fL
let tuple_tag = 0x0000000000000001L
let tuple_mask = 0x000000000000000fL
let thread_tag = 0x0000000000000009L
let thread_mask = 0x000000000000000fL
let mutex_tag = 0x000000000000000aL
let mutex_mask = 0x000000000000000fL
let pad_const = Const (Int64.of_string "0x69420f3f0")
let nil_value = 0x0000000000000001L
let const_nil = HexConst tuple_tag
let err_COMP_NOT_NUM = 1L
let err_ARITH_NOT_NUM = 2L
let err_LOGIC_NOT_BOOL = 3L
let err_IF_NOT_BOOL = 4L
let err_OVERFLOW = 5L
let err_GET_NOT_TUPLE = 6L
let err_GET_LOW_INDEX = 7L
let err_GET_HIGH_INDEX = 8L
let err_GET_NOT_NUM = 9L
let err_NIL_DEREF = 10L
let err_OUT_OF_MEMORY = 11L
let err_SET_NOT_TUPLE = 12L
let err_SET_LOW_INDEX = 13L
let err_SET_NOT_NUM = 14L
let err_SET_HIGH_INDEX = 15L
let err_EXPECTED_LOCK_LOCK = 16
let err_EXPECTED_LOCK_UNLOCK = 17
let err_EXPECTED_LOCK_SCOPED = 18
let err_EXPECTED_LAMBDA_SCOPED = 19
let err_EXPECTED_LAMBDA_THREAD = 20
let err_EXPECTED_THREAD_START = 21
let err_EXPECTED_THREAD_GET = 22
let first_six_args_registers = [ RDI; RSI; RDX; RCX; R8; R9 ]
let scratch_reg = R12 (* callee saved! *)
let scratch_reg2 = R13
let glob_name = "global"

(* You may find some of these helpers useful *)

let rec find ls x =
  match ls with
  | [] -> raise (InternalCompilerError (sprintf "Name %s not found" x))
  | (y, v) :: rest -> if y = x then v else find rest x

let count_vars e =
  let rec helpA e =
    match e with
    | ALet (_, bind, body, _) -> 1 + max (helpC bind) (helpA body)
    | ALetRec (binds, body, _) ->
        List.length binds
        + List.fold_left max (helpA body)
            (List.map (fun (_, rhs) -> helpC rhs) binds)
    | ASeq (e1, e2, _) -> max (helpC e1) (helpA e2)
    | ACExpr e -> helpC e
  and helpC e =
    match e with CIf (_, t, f, _) -> max (helpA t) (helpA f) | _ -> 0
  in
  helpA e

let rec replicate x i = if i = 0 then [] else x :: replicate x (i - 1)

let rec find_decl (ds : 'a decl list) (name : string) : 'a decl option =
  match ds with
  | [] -> None
  | (DFun (fname, _, _, _) as d) :: ds_rest ->
      if name = fname then Some d else find_decl ds_rest name

let rec find_one (l : 'a list) (elt : 'a) : bool =
  match l with [] -> false | x :: xs -> elt = x || find_one xs elt

let rec find_dup (l : 'a list) : 'a option =
  match l with
  | [] | [ _ ] -> None
  | x :: xs -> if find_one xs x then Some x else find_dup xs

let rename_and_tag (p : tag program) : tag program =
  let rec rename env p =
    match p with
    | Program (decls, body, tag) ->
        Program
          ( List.map (fun group -> List.map (helpD env) group) decls,
            helpE env body,
            tag )
  and helpD env decl =
    match decl with
    | DFun (name, args, body, tag) ->
        let newArgs, env' = helpBS env args in
        DFun (name, newArgs, helpE env' body, tag)
  and helpB env b =
    match b with
    | BBlank _ -> (b, env)
    | BName (name, allow_shadow, tag) ->
        let name' = sprintf "%s_%d" name tag in
        (BName (name', allow_shadow, tag), (name, name') :: env)
    | BTuple (binds, tag) ->
        let binds', env' = helpBS env binds in
        (BTuple (binds', tag), env')
  and helpBS env (bs : tag bind list) =
    match bs with
    | [] -> ([], env)
    | b :: bs ->
        let b', env' = helpB env b in
        let bs', env'' = helpBS env' bs in
        (b' :: bs', env'')
  and helpBG env (bindings : tag binding list) =
    match bindings with
    | [] -> ([], env)
    | (b, e, a) :: bindings ->
        let b', env' = helpB env b in
        let e' = helpE env e in
        let bindings', env'' = helpBG env' bindings in
        ((b', e', a) :: bindings', env'')
  and helpE env e =
    match e with
    | ESeq (e1, e2, tag) -> ESeq (helpE env e1, helpE env e2, tag)
    | ETuple (es, tag) -> ETuple (List.map (helpE env) es, tag)
    | EGetItem (e, idx, tag) -> EGetItem (helpE env e, helpE env idx, tag)
    | ESetItem (e, idx, newval, tag) ->
        ESetItem (helpE env e, helpE env idx, helpE env newval, tag)
    | EPrim1 (op, arg, tag) -> EPrim1 (op, helpE env arg, tag)
    | EPrim2 (op, left, right, tag) ->
        EPrim2 (op, helpE env left, helpE env right, tag)
    | EIf (c, t, f, tag) -> EIf (helpE env c, helpE env t, helpE env f, tag)
    | ENumber _ -> e
    | EBool _ -> e
    | ENil _ -> e
    | EMutex _ -> e
    | EId (name, tag) -> (
        try EId (find env name, tag) with InternalCompilerError _ -> e)
    | EApp (func, args, ct, tag) ->
        let func = helpE env func in
        let call_type = match ct with Native _ -> ct | _ -> Snake in
        EApp (func, List.map (helpE env) args, call_type, tag)
    | ELet (binds, body, tag) ->
        let binds', env' = helpBG env binds in
        let body' = helpE env' body in
        ELet (binds', body', tag)
    | ELetRec (bindings, body, tag) ->
        let revbinds, env =
          List.fold_left
            (fun (revbinds, env) (b, e, t) ->
              let b, env = helpB env b in
              ((b, e, t) :: revbinds, env))
            ([], env) bindings
        in
        let bindings' =
          List.fold_left
            (fun bindings (b, e, tag) -> (b, helpE env e, tag) :: bindings)
            [] revbinds
        in
        let body' = helpE env body in
        ELetRec (bindings', body', tag)
    | ELambda (binds, body, tag) ->
        let binds', env' = helpBS env binds in
        let body' = helpE env' body in
        ELambda (binds', body', tag)
  in
  rename [] p

let deepest_stack env =
  List.fold_left
    (fun mx (_, arg) ->
      match arg with
      | RegOffset (depth, RBP) -> max mx (max (-depth) 0)
      | _ -> mx)
    0 env

(* IMPLEMENT EVERYTHING BELOW *)

(* This data type lets us keep track of how a binding was introduced.
   We'll use it to discard unnecessary Seq bindings, and to distinguish
   letrec from let. Essentially, it accumulates just enough information
   in our binding list to tell us how to reconstruct an appropriate aexpr. *)
type 'a anf_bind =
  | BSeq of 'a cexpr
  | BLet of string * 'a cexpr
  | BLetRec of (string * 'a cexpr) list

let anf (p : tag program) : unit aprogram =
  let rec helpP (p : tag program) : unit aprogram =
    match p with
    | Program ([], body, _) -> AProgram (helpA body, ())
    | _ ->
        raise
          (InternalCompilerError
             "Top-level declarations should have been desugared away")
  and helpC (e : tag expr) : unit cexpr * unit anf_bind list =
    match e with
    | EPrim1 (op, arg, _) ->
        let arg_imm, arg_setup = helpI arg in
        (CPrim1 (op, arg_imm, ()), arg_setup)
    | EPrim2 (op, left, right, _) ->
        let left_imm, left_setup = helpI left in
        let right_imm, right_setup = helpI right in
        (CPrim2 (op, left_imm, right_imm, ()), left_setup @ right_setup)
    | EIf (cond, _then, _else, _) ->
        let cond_imm, cond_setup = helpI cond in
        (CIf (cond_imm, helpA _then, helpA _else, ()), cond_setup)
    | ELet ([], body, _) -> helpC body
    | ELet ((BBlank _, exp, _) :: rest, body, pos) ->
        let exp_ans, exp_setup = helpC exp in
        let body_ans, body_setup = helpC (ELet (rest, body, pos)) in
        (body_ans, exp_setup @ [ BSeq exp_ans ] @ body_setup)
    | ELet ((BName (bind, _, _), exp, _) :: rest, body, pos) ->
        let exp_ans, exp_setup = helpC exp in
        let body_ans, body_setup = helpC (ELet (rest, body, pos)) in
        (body_ans, exp_setup @ [ BLet (bind, exp_ans) ] @ body_setup)
    | ELet ((BTuple (_, _), _, _) :: _, _, _) ->
        raise
          (InternalCompilerError
             "Tuple bindings should have been desugared away")
    | ESeq _ -> raise (InternalCompilerError "ESeq gone")
    | EApp (func, args, ct, _) ->
        let new_exprs, new_setup = List.split (List.map helpI args) in
        let func_imm, func_setup = helpI func in
        (CApp (func_imm, new_exprs, ct, ()), List.concat new_setup @ func_setup)
    | ETuple (args, _) ->
        let new_exprs, new_setup = List.split (List.map helpI args) in
        (CTuple (new_exprs, ()), List.concat new_setup)
    | EGetItem (tup, idx, _) ->
        let tup_imm, tup_setup = helpI tup in
        let idx_imm, idx_setup = helpI idx in
        (CGetItem (tup_imm, idx_imm, ()), tup_setup @ idx_setup)
    | ESetItem (tup, idx, newval, _) ->
        let tup_imm, tup_setup = helpI tup in
        let idx_imm, idx_setup = helpI idx in
        let newval_imm, newval_setup = helpI newval in
        ( CSetItem (tup_imm, idx_imm, newval_imm, ()),
          List.concat [ tup_setup; idx_setup; newval_setup ] )
    | ELambda (binds, body, _) ->
        let bind_names =
          List.map
            (fun a ->
              match a with
              | BName (name, _, _) -> name
              | BBlank _ -> ""
              | BTuple _ ->
                  raise (InternalCompilerError "BTuple should be desugared"))
            binds
        in
        (CLambda (bind_names, helpA body, ()), [])
    | ELetRec (binds, body, _) ->
        let fn_setup, fn_binds =
          List.fold_left
            (fun (setup, blist) (bind, expr, _) ->
              match (bind, expr) with
              | BName (name, _, _), ELambda _ ->
                  let exp_ans, exp_setup = helpC expr in
                  (setup @ exp_setup, blist @ [ (name, exp_ans) ])
              | _ ->
                  raise
                    (InternalCompilerError
                       "Nonfunction or Nonname found in letrec"))
            ([], []) binds
        in
        let body_ans, body_setup = helpC body in
        (body_ans, fn_setup @ [ BLetRec fn_binds ] @ body_setup)
    | _ ->
        let imm, setup = helpI e in
        (CImmExpr imm, setup)
  and helpI (e : tag expr) : unit immexpr * unit anf_bind list =
    match e with
    | ENumber (n, _) -> (ImmNum (n, ()), [])
    | EBool (b, _) -> (ImmBool (b, ()), [])
    | EId (name, _) -> (ImmId (name, ()), [])
    | ENil _ -> (ImmNil (), [])
    | EMutex tag ->
        let tmp = sprintf "mutex_%d\n" tag in
        (ImmId (tmp, ()), [ BLet (tmp, CMutex ()) ])
    | ESeq _ -> raise (InternalCompilerError "ESeq gone")
    | ETuple (_, tag) ->
        let tmp = sprintf "tuple_%d\n" tag in
        let ans, setup = helpC e in
        (ImmId (tmp, ()), setup @ [ BLet (tmp, ans) ])
    | EGetItem (tup, idx, tag) ->
        let tmp = sprintf "getitem_%d" tag in
        let tup_imm, tup_setup = helpI tup in
        let idx_imm, idx_setup = helpI idx in
        ( ImmId (tmp, ()),
          tup_setup @ idx_setup
          @ [ BLet (tmp, CGetItem (tup_imm, idx_imm, ())) ] )
    | ESetItem (tup, idx, newval, tag) ->
        let tmp = sprintf "setitem_%d" tag in
        let tup_imm, tup_setup = helpI tup in
        let idx_imm, idx_setup = helpI idx in
        let newval_imm, newval_setup = helpI newval in
        ( ImmId (tmp, ()),
          List.concat [ tup_setup; idx_setup; newval_setup ]
          @ [ BLet (tmp, CSetItem (tup_imm, idx_imm, newval_imm, ())) ] )
    | EPrim1 (op, arg, tag) ->
        let tmp = sprintf "unary_%d" tag in
        let arg_imm, arg_setup = helpI arg in
        (ImmId (tmp, ()), arg_setup @ [ BLet (tmp, CPrim1 (op, arg_imm, ())) ])
    | EPrim2 (op, left, right, tag) ->
        let tmp = sprintf "binop_%d" tag in
        let left_imm, left_setup = helpI left in
        let right_imm, right_setup = helpI right in
        ( ImmId (tmp, ()),
          left_setup @ right_setup
          @ [ BLet (tmp, CPrim2 (op, left_imm, right_imm, ())) ] )
    | EIf (cond, _then, _else, tag) ->
        let tmp = sprintf "if_%d" tag in
        let cond_imm, cond_setup = helpI cond in
        ( ImmId (tmp, ()),
          cond_setup
          @ [ BLet (tmp, CIf (cond_imm, helpA _then, helpA _else, ())) ] )
    | EApp (func, args, ct, tag) ->
        let tmp = sprintf "app_%d" tag in
        let func_imm, func_setup = helpI func in
        let new_exprs, new_setup = List.split (List.map helpI args) in
        ( ImmId (tmp, ()),
          List.concat new_setup @ func_setup
          @ [ BLet (tmp, CApp (func_imm, new_exprs, ct, ())) ] )
    | ELet ([], body, _) -> helpI body
    | ELet ((BBlank _, exp, _) :: rest, body, pos) ->
        let _, exp_setup = helpI exp in
        (* MUST BE helpI, to avoid any missing final steps *)
        let body_ans, body_setup = helpI (ELet (rest, body, pos)) in
        (body_ans, exp_setup @ body_setup)
    | ELambda (_, _, t) ->
        let tmp = sprintf "lambda_%d" t in
        let exp_ans, exp_setup = helpC e in
        (ImmId (tmp, ()), exp_setup @ [ BLet (tmp, exp_ans) ])
    | ELet ((BName (bind, _, _), exp, _) :: rest, body, pos) ->
        let exp_ans, exp_setup = helpC exp in
        let body_ans, body_setup = helpI (ELet (rest, body, pos)) in
        (body_ans, exp_setup @ [ BLet (bind, exp_ans) ] @ body_setup)
    | ELet ((BTuple (_, _), _, _) :: _, _, _) ->
        raise
          (InternalCompilerError
             "Tuple bindings should have been desugared away")
    | ELetRec (binds, body, _) ->
        (* MUST do this all together*)
        let fn_setup, fn_binds =
          List.fold_left
            (fun (setups, blets) (bind, expr, _) ->
              match (bind, expr) with
              | BName (name, _, _), ELambda _ ->
                  let exp_ans, exp_setup = helpI expr in
                  (setups @ exp_setup, blets @ [ (name, CImmExpr exp_ans) ])
              | _ ->
                  raise
                    (InternalCompilerError
                       "Nonfunction or Nonname found in letrec"))
            ([], []) binds
        in
        let body_ans, body_setup = helpI body in
        (body_ans, fn_setup @ [ BLetRec fn_binds ] @ body_setup)
    (* Hint: use BLetRec for each of the binds, and BLet for the final answer *)
  and helpA e : unit aexpr =
    let ans, ans_setup = helpC e in
    List.fold_right
      (fun bind body ->
        (* Here's where the anf_bind datatype becomes most useful:
             BSeq binds get dropped, and turned into ASeq aexprs.
             BLet binds get wrapped back into ALet aexprs.
             BLetRec binds get wrapped back into ALetRec aexprs.
           Syntactically it looks like we're just replacing Bwhatever with Awhatever,
           but that's exactly the information needed to know which aexpr to build. *)
        match bind with
        | BLet (name, exp) -> ALet (name, exp, body, ())
        | BLetRec names -> ALetRec (names, body, ())
        | BSeq e -> ASeq (e, body, ()))
      ans_setup (ACExpr ans)
  in
  helpP p

let is_well_formed (p : sourcespan program) : sourcespan program fallible =
  let get_dup_id exp_list =
    let dup_errors, _ =
      List.fold_left
        (fun (dup_errors, seen_bindings) (name, pos) ->
          let gen_errors =
            List.map
              (fun dup_binding ->
                DuplicateId (fst dup_binding, pos, snd dup_binding))
              (List.filter (fun (id, _) -> id = name) seen_bindings)
          in
          (dup_errors @ gen_errors, seen_bindings @ [ (name, pos) ]))
        ([], []) exp_list
    in
    dup_errors
  in
  let get_shadow_id new_exps old_exps =
    let rec single_shadow exp old_exps =
      match old_exps with
      | [] -> []
      | (name, pos) :: rest ->
          (if exp = name then [ (name, pos) ] else []) @ single_shadow exp rest
    in
    List.map
      (fun (name, pos) -> ShadowId (name, pos, pos))
      (List.concat_map (fun exp -> single_shadow exp old_exps) new_exps)
  in
  let rec flatten_binding binds =
    match binds with
    | BBlank _ -> []
    | BTuple (blist, _) -> List.concat_map flatten_binding blist
    | BName (s, _, x) -> [ (s, x) ]
  in
  let rec wf_E (e : sourcespan expr) (ctx : string list) =
    match e with
    | ENumber (n, pos) ->
        if n > Int64.div Int64.max_int 2L || n < Int64.div Int64.min_int 2L then
          [ Overflow (n, pos) ]
        else []
    | EBool _ -> []
    | EId (id, t) -> if find_one ctx id then [] else [ UnboundId (id, t) ]
    | EPrim1 (_, e, _) -> wf_E e ctx
    | EPrim2 (_, e1, e2, _) -> wf_E e1 ctx @ wf_E e2 ctx
    | EIf (cond, thn, els, _) -> wf_E cond ctx @ wf_E thn ctx @ wf_E els ctx
    | EApp (fun_body, args, _, _) ->
        wf_E fun_body ctx @ List.concat_map (fun arg -> wf_E arg ctx) args
    | ELet (binds, body, _) ->
        let n_ctx, bind_errors =
          List.fold_left
            (fun (cctx, errs) (bind, expr, _) ->
              let c_binds = flatten_binding bind in
              let n_ctx = cctx @ List.map (fun (s, _) -> s) c_binds in
              (n_ctx, errs @ wf_E expr n_ctx))
            (ctx, []) binds
        in
        let flat_binds =
          List.concat_map flatten_binding
            (List.map (fun (bind, _, _) -> bind) binds)
        in
        let dup_errors = get_dup_id flat_binds in
        let shadow_errors = get_shadow_id ctx flat_binds in
        let all_errs =
          bind_errors @ dup_errors @ shadow_errors @ wf_E body n_ctx
        in
        all_errs
    | ETuple (exps, _) -> List.concat_map (fun e -> wf_E e ctx) exps
    | EGetItem (e1, e2, _) -> wf_E e1 ctx @ wf_E e2 ctx
    | ESetItem (e1, e2, e3, _) -> wf_E e1 ctx @ wf_E e2 ctx @ wf_E e3 ctx
    | ESeq (e1, e2, _) -> wf_E e1 ctx @ wf_E e2 ctx
    | ENil _ -> []
    | EMutex _ -> []
    | ELambda (bindings, body, _) ->
        let extra_ctx =
          List.map
            (fun binding ->
              match binding with
              | BName (n, _, pos) -> (n, pos)
              | _ ->
                  raise
                    (InternalCompilerError
                       "Found non-name bindings inside Lambda"))
            bindings
        in
        let bind_dup = get_dup_id extra_ctx in
        wf_E body (ctx @ List.map (fun (s, _) -> s) extra_ctx) @ bind_dup
    | ELetRec (bindings, body, _) ->
        let n_ctx_wpos, bind_errors_nodup =
          List.fold_left
            (fun (ctx, errs) (bind, _, pos) ->
              match bind with
              | BName (name, _, pos) -> ((name, pos) :: ctx, errs)
              | _ ->
                  ( ctx,
                    errs
                    @ [
                        Unsupported
                          ("Non-name bindings are not supported in LetRec", pos);
                      ] ))
            ([], []) bindings
        in
        let bind_dup = get_dup_id n_ctx_wpos in
        let shadow_errors = get_shadow_id ctx n_ctx_wpos in
        let n_ctx = ctx @ List.map (fun (s, _) -> s) n_ctx_wpos in
        let bind_errors = bind_errors_nodup @ bind_dup in
        if bind_errors <> [] || shadow_errors <> [] then
          bind_errors @ shadow_errors
        else
          let body_errors =
            List.fold_left
              (fun errs (bind, expr, pos) ->
                match expr with
                | ELambda _ -> errs @ wf_E expr n_ctx
                | _ -> errs @ [ LetRecNonFunction (bind, pos) ])
              [] bindings
          in
          body_errors @ wf_E body n_ctx
  in
  match p with
  | Program (decls, body, _) -> (
      if List.length decls <> 0 then
        raise (InternalCompilerError "No Decl anymore")
      else
        let body_errors = wf_E body [] in
        let all_errs = body_errors in
        match all_errs with [] -> Ok p | _ -> Error all_errs)

let desugar1 (p : sourcespan program) : sourcespan program =
  let gensym =
    let next = ref 0 in
    fun name ->
      next := !next + 1;
      sprintf "%s_%d" name !next
  in
  let rec helpE (e : sourcespan expr) :
      sourcespan expr (* other parameters may be needed here *) =
    let rec helpBind (b : sourcespan bind) exp : sourcespan binding list =
      match b with
      | BBlank t -> [ (BBlank t, helpE exp, t) ]
      | BTuple (binds, t) ->
          let tup_name = gensym "tup_bind_expr" in
          [ (BName (tup_name, false, t), helpE exp, t) ]
          @ List.flatten
              (List.mapi
                 (fun i bind ->
                   helpBind bind
                     (EGetItem
                        (EId (tup_name, t), ENumber (Int64.of_int i, t), t)))
                 binds)
      | BName (s, b, t) -> [ (BName (s, b, t), helpE exp, t) ]
    in
    match e with
    | ESeq (e1, e2, t) -> ESeq (helpE e1, helpE e2, t)
    | ENumber _ | EBool _ | ENil _ | EId _ | EMutex _ -> e
    | ETuple (exp, t) -> ETuple (List.map helpE exp, t)
    | ELet (binds, exp, t) ->
        let new_binds = List.concat_map (fun (b, e, _) -> helpBind b e) binds in
        let new_exp = helpE exp in
        ELet (new_binds, new_exp, t)
    | EGetItem (e1, e2, t) -> EGetItem (helpE e1, helpE e2, t)
    | ESetItem (e1, e2, e3, t) -> ESetItem (helpE e1, helpE e2, helpE e3, t)
    | EIf (cond, thn, els, t) -> EIf (helpE cond, helpE thn, helpE els, t)
    | EApp (fun_body, args, c_type, t) ->
        EApp (fun_body, List.map helpE args, c_type, t)
    | EPrim1 (op, e, t) -> EPrim1 (op, helpE e, t)
    | EPrim2 (And, e1, e2, t) ->
        let left = helpE (EPrim1 (Not, e1, t)) in
        let right = helpE (EPrim1 (Not, EPrim1 (Not, e2, t), t)) in
        EIf (left, EBool (false, t), right, t)
    | EPrim2 (Or, e1, e2, t) ->
        let left = helpE (EPrim1 (Not, e1, t)) in
        let right = helpE (EPrim1 (Not, EPrim1 (Not, e2, t), t)) in
        EIf (left, right, EBool (true, t), t)
    | EPrim2 (op, e1, e2, t) -> EPrim2 (op, helpE e1, helpE e2, t)
    | ELambda (bindings, body, s) ->
        let new_args, new_binds =
          List.fold_left
            (fun (t_binds, f_binds) bind ->
              match bind with
              | BBlank t -> (t_binds @ [ BBlank t ], f_binds)
              | BName (s, b, t) -> (t_binds @ [ BName (s, b, t) ], f_binds)
              | BTuple (binds, t) ->
                  let new_name = gensym "tup_bind_lambda" in
                  ( t_binds @ [ BName (new_name, false, t) ],
                    f_binds @ [ (BTuple (binds, t), EId (new_name, t), t) ] ))
            ([], []) bindings
        in
        ELambda (new_args, helpE (ELet (new_binds, body, s)), s)
    | ELetRec (bindings, body, t) -> ELetRec (bindings, helpE body, t)
  and helpDecl (d : sourcespan decl) : sourcespan binding =
    match d with
    | DFun (name, binds, e, s) ->
        let func_name = BName (name, false, s) in
        let func_lambda = helpE (ELambda (binds, e, s)) in
        (func_name, func_lambda, s)
  in
  match p with
  (* TODO: WRAP THESE IN DEFAULT LAMBDAS*)
  | Program (decls, body, t) ->
      let binding_lists =
        List.map (fun decl_group -> List.map helpDecl decl_group) decls
      in
      let top_letrec =
        List.fold_right
          (fun bindings e -> ELetRec (bindings, e, t))
          binding_lists (helpE body)
      in
      Program ([], top_letrec, t)

let add_native_lambda (p : sourcespan program) : sourcespan program =
  match p with
  | Program (decls, body, t) ->
      let new_body =
        ELetRec
          ( [
              ( BName ("equal", false, t),
                ELambda
                  ( [ BName ("x", false, t); BName ("y", false, t) ],
                    EApp
                      ( EId ("equal", t),
                        [ EId ("x", t); EId ("y", t) ],
                        Native "equal",
                        t ),
                    t ),
                t );
              ( BName ("input", false, t),
                ELambda ([], EApp (EId ("input", t), [], Native "input", t), t),
                t );
            ],
            body,
            t )
      in
      Program ([], new_body, t)

let dummy_span = (Lexing.dummy_pos, Lexing.dummy_pos)

let desugar2 (p : sourcespan program) : sourcespan program =
  let gensym =
    let next = ref 0 in
    fun name ->
      next := !next + 1;
      sprintf "%s_%d" name !next
  in
  let rec helpP (p : sourcespan program) =
    match p with
    | Program (decls, body, tag) ->
        (* This particular desugaring will convert declgroups into ELetRecs *)
        let merge_sourcespans ((s1, _) : sourcespan) ((_, s2) : sourcespan) :
            sourcespan =
          (s1, s2)
        in
        let wrap_G g body =
          match g with
          | [] -> body
          | f :: r ->
              let span =
                List.fold_left merge_sourcespans (get_tag_D f)
                  (List.map get_tag_D r)
              in
              ELetRec (helpG g, body, span)
        in
        Program ([], List.fold_right wrap_G decls (helpE body), tag)
  and helpG g = List.map helpD g
  and helpD d =
    match d with
    | DFun (name, args, body, tag) ->
        let helpArg a =
          match a with
          | BTuple (_, tag) ->
              let name = gensym "argtup" in
              let newbind = BName (name, false, tag) in
              (newbind, [ (a, EId (name, tag), tag) ])
          | _ -> (a, [])
        in
        let newargs, argbinds = List.split (List.map helpArg args) in
        let newbody = ELet (List.flatten argbinds, body, tag) in
        (BName (name, false, tag), ELambda (newargs, helpE newbody, tag), tag)
  and helpBE bind =
    let b, e, btag = bind in
    let e = helpE e in
    match b with
    | BTuple (binds, ttag) -> (
        match e with
        | EId _ -> expandTuple binds ttag e
        | _ ->
            let newname = gensym "tup" in
            (BName (newname, false, ttag), e, btag)
            :: expandTuple binds ttag (EId (newname, ttag)))
    | _ -> [ (b, e, btag) ]
  and expandTuple binds tag source : sourcespan binding list =
    let tupleBind i b =
      match b with
      | BBlank btag -> []
      | BName (_, _, btag) ->
          [
            ( b,
              EGetItem (source, ENumber (Int64.of_int i, dummy_span), tag),
              btag );
          ]
      | BTuple (binds, tag) ->
          let newname = gensym "tup" in
          let newexpr = EId (newname, tag) in
          ( BName (newname, false, tag),
            EGetItem (source, ENumber (Int64.of_int i, dummy_span), tag),
            tag )
          :: expandTuple binds tag newexpr
    in
    let size_check =
      EPrim2
        ( CheckSize,
          source,
          ENumber (Int64.of_int (List.length binds), dummy_span),
          dummy_span )
    in
    let size_check_bind = (BBlank dummy_span, size_check, dummy_span) in
    size_check_bind :: List.flatten (List.mapi tupleBind binds)
  and helpE e =
    match e with
    | ESeq (e1, e2, tag) -> ELet ([ (BBlank tag, helpE e1, tag) ], helpE e2, tag)
    | ETuple (exprs, tag) -> ETuple (List.map helpE exprs, tag)
    | EGetItem (e, idx, tag) -> EGetItem (helpE e, helpE idx, tag)
    | ESetItem (e, idx, newval, tag) ->
        ESetItem (helpE e, helpE idx, helpE newval, tag)
    | EId (x, tag) -> EId (x, tag)
    | ENumber (n, tag) -> ENumber (n, tag)
    | EBool (b, tag) -> EBool (b, tag)
    | ENil (t, tag) -> ENil (t, tag)
    | EMutex (t, tag) -> EMutex (t, tag)
    | EPrim1 (op, e, tag) -> EPrim1 (op, helpE e, tag)
    | EPrim2 (op, e1, e2, tag) -> EPrim2 (op, helpE e1, helpE e2, tag)
    | ELet (binds, body, tag) ->
        let newbinds = List.map helpBE binds in
        List.fold_right
          (fun binds body -> ELet (binds, body, tag))
          newbinds (helpE body)
    | ELetRec (bindexps, body, tag) ->
        (* ASSUMES well-formed letrec, so only BName bindings *)
        let newbinds =
          List.map (fun (bind, e, tag) -> (bind, helpE e, tag)) bindexps
        in
        ELetRec (newbinds, helpE body, tag)
    | EIf (cond, thn, els, tag) -> EIf (helpE cond, helpE thn, helpE els, tag)
    | EApp (name, args, native, tag) ->
        EApp (helpE name, List.map helpE args, native, tag)
    | ELambda (binds, body, tag) ->
        let expandBind bind =
          match bind with
          | BTuple (_, btag) ->
              let newparam = gensym "tuparg" in
              ( BName (newparam, false, btag),
                helpBE (bind, EId (newparam, btag), btag) )
          | _ -> (bind, [])
        in
        let params, newbinds = List.split (List.map expandBind binds) in
        let newbody =
          List.fold_right
            (fun binds body -> ELet (binds, body, tag))
            newbinds (helpE body)
        in
        ELambda (params, newbody, tag)
  in

  helpP p

let free_vars (e : 'a aexpr) : string list =
  let rec helpA e =
    match e with
    | ALet (name, exp, body, _) ->
        let fv1 = helpA body in
        let fv2 = helpC exp in
        let fv = StringSet.union fv1 fv2 in
        StringSet.remove name fv
    | ACExpr c_expr -> helpC c_expr
    | ALetRec (defs, body, _) ->
        let fv = helpA body in
        List.fold_left (fun acc (name, _) -> StringSet.remove name acc) fv defs
    | ASeq (e1, e2, _) ->
        let fv1 = helpC e1 in
        let fv2 = helpA e2 in
        StringSet.union fv1 fv2
  and helpC e =
    match e with
    | CIf (c, t, f, _) ->
        let c_fv = helpI c in
        let t_fv = helpA t in
        let f_fv = helpA f in
        StringSet.union c_fv (StringSet.union t_fv f_fv)
    | CPrim1 (_, i, _) -> helpI i
    | CPrim2 (_, i1, i2, _) -> StringSet.union (helpI i1) (helpI i2)
    | CApp (immexpr, args, _, _) -> (
        match immexpr with
        | ImmId (name, _) ->
            StringSet.union (StringSet.singleton name)
              (List.fold_left
                 (fun acc i -> StringSet.union acc (helpI i))
                 StringSet.empty args)
        | _ -> raise (InternalCompilerError "CApp without immexpr fun"))
    | CTuple (vals, _) ->
        List.fold_left
          (fun acc i -> StringSet.union acc (helpI i))
          StringSet.empty vals
    | CGetItem (t, x, _) -> StringSet.union (helpI t) (helpI x)
    | CSetItem (t, x, v, _) ->
        StringSet.union (helpI t) (StringSet.union (helpI x) (helpI v))
    | CLambda (args, body, _) ->
        let fv = helpA body in
        List.fold_left (fun acc i -> StringSet.remove i acc) fv args
    | CMutex _ -> StringSet.empty
    | CImmExpr i -> helpI i
  and helpI e =
    match e with
    | ImmId (name, _) -> StringSet.singleton name
    | _ -> StringSet.empty
  in
  let fv = helpA e in
  StringSet.elements fv

(* we want envt to be int -> (string * arg) list*)
let rec naive_stack_allocation (prog : tag aprogram) :
    tag aprogram * arg envt envt =
  let rec append_l envs ftag to_add =
    match envs with
    | [] -> [ (ftag, to_add) ]
    | (n, l) :: r ->
        if n = ftag then (n, to_add @ l) :: r
        else (n, l) :: append_l r ftag to_add
  in
  let rec merge env1 env2 =
    match env1 with
    | [] -> env2
    | (n, env) :: r -> append_l (merge r env2) n env
  in
  let rec allocate_cexpr exp si ftag =
    match exp with
    | CIf (_, t, f, _) ->
        merge (allocate_aexpr t si ftag) (allocate_aexpr f si ftag)
    | CLambda (args, body, t) ->
        let new_tag = sprintf "%d" t in
        let fv = List.sort String.compare (free_vars (ACExpr exp)) in
        let inner_envs = allocate_aexpr body (1 + List.length fv) new_tag in
        let arg_vars =
          List.mapi
            (fun idx a -> (a, RegOffset (word_size * (idx + 3), RBP)))
            args
        in
        let closed_vars =
          List.mapi
            (fun idx f -> (f, RegOffset (word_size * (~-idx - 1), RBP)))
            fv
        in
        append_l inner_envs new_tag (arg_vars @ closed_vars)
    | _ -> []
  and allocate_aexpr e si ftag =
    match e with
    | ALet (id, bindexpr, body, _) ->
        let e1 = allocate_cexpr bindexpr (si + 1) ftag in
        let e2 = allocate_aexpr body (si + 1) ftag in
        let new_id = [ (id, RegOffset (~-word_size * si, RBP)) ] in
        append_l (merge e1 e2) ftag new_id
    | ACExpr e -> allocate_cexpr e si ftag
    | ASeq (e1, e2, _) ->
        let env1 = allocate_cexpr e1 (si + 1) ftag in
        let env2 = allocate_aexpr e2 (si + 1) ftag in
        merge env1 env2
    | ALetRec (binds, body, _) ->
        let envs, ns =
          List.fold_left
            (fun (acc, s) (name, exp) ->
              ( append_l
                  (merge acc (allocate_cexpr exp (s + 1) ftag))
                  ftag
                  [ (name, RegOffset (~-word_size * s, RBP)) ],
                s + 1 ))
            ([], si) binds
        in
        let body_env = allocate_aexpr body ns ftag in
        merge envs body_env
  in
  match prog with
  | AProgram (body, _) ->
      (prog, append_l (allocate_aexpr body 1 glob_name) glob_name [])

let rec compile_fun (func : tag immexpr) (args : tag immexpr list)
    (env : arg envt) : instruction list =
  let offset =
    if (List.length args + 1) mod 2 = 0 then [] else [ IPush pad_const ]
  in
  [
    IMov (Reg scratch_reg, compile_imm func env);
    IAnd (Reg scratch_reg, Const closure_tag_mask);
    ICmp (Reg scratch_reg, Const closure_tag);
    IMov (Reg RAX, compile_imm func env);
    IJne (Label "not_closure");
    IMov (Reg RAX, compile_imm func env);
    IMov (Reg scratch_reg, Reg RAX);
    (* check arity! *)
    ISub (Reg scratch_reg, Const closure_tag);
    IMov (Reg scratch_reg2, RegOffset (0, scratch_reg));
    ICmp (Reg scratch_reg2, Const (Int64.of_int (List.length args)));
    IMov (Reg RAX, Reg scratch_reg2);
    IJne (Label "closure_arity");
  ]
  (*TODO: CHeck arity!*)
  @ offset
  @ List.flatten
      (List.map
         (fun a ->
           [
             IMov (Reg scratch_reg2, compile_imm a env);
             IPush (Reg scratch_reg2);
           ])
         (List.rev args))
  @ [
      IMov (Reg RAX, compile_imm func env);
      ILineComment "Push self onto stack";
      IPush (Reg RAX);
      ICall (RegOffset (word_size, scratch_reg));
    ]
  @ List.map (fun _ -> IPop (Reg scratch_reg2)) args
  @ [ IPop (Reg scratch_reg) ] (*for func*)
  @ List.map (fun _ -> IPop (Reg scratch_reg2)) offset

and compile_aexpr (e : tag aexpr) (env : arg envt envt) (ftag : string)
    (_ : int) (_ : bool) : instruction list =
  let current_env = find env ftag in
  match e with
  | ALet (name, bind, body, _) ->
      let set_bind =
        compile_cexpr bind env ftag 0 false
        @ [ IMov (find current_env name, Reg RAX) ]
      in
      set_bind @ compile_aexpr body env ftag 0 false
  | ACExpr c_expr -> compile_cexpr c_expr env ftag 0 false
  | ASeq (e1, e2, _) ->
      let a1 = compile_cexpr e1 env ftag 0 false in
      let a2 = compile_aexpr e2 env ftag 0 false in
      a1 @ a2
  | ALetRec (fns, body, _) ->
      let pre_alloc =
        List.concat_map
          (fun (name, e) ->
            let fv = List.sort String.compare (free_vars (ACExpr e)) in
            let sz = 3 + List.length fv in
            let lambda_sz = sz + (sz mod 2) in
            [
              ILineComment
                (sprintf "Allocating %s, fill in with zeros if you want" name);
            ]
            @ reserve lambda_sz
            @ [
                IAdd (Reg RAX, Const closure_tag);
                IMov (find current_env name, Reg RAX);
              ])
          fns
      in
      let comp_lambdas =
        List.concat_map
          (fun (name, e) ->
            (* move the SNAKEVAL to scratch_reg*)
            [
              IInstrComment
                ( IMov (Reg scratch_reg, find current_env name),
                  sprintf "Move prealloc back %s" name );
              ISub (Reg scratch_reg, Const closure_tag);
            ]
            @ compile_clambda e env ftag true)
          fns
      in
      let body_code = compile_aexpr body env ftag 0 false in
      pre_alloc @ comp_lambdas @ body_code

and compile_clambda (e : tag cexpr) (envs : arg envt envt) (ftag : string)
    (allocd : bool) =
  let env = find envs ftag in
  match e with
  | CLambda (bindings, body, tag) ->
      (* if alloc'd, its from a letrec, and has already been allocated*)
      let start_label = sprintf "lambda_%d" tag in
      let end_label = sprintf "lambda_%d_end" tag in
      let n_tag = sprintf "%d" tag in
      let body_instructions = compile_aexpr body envs n_tag 0 false in
      let fv = List.sort String.compare (free_vars (ACExpr e)) in
      let sz = List.length fv in
      let lambda_sz = 3 + sz + ((3 + sz) mod 2) in
      let tmp_sz = deepest_stack (find envs n_tag) in
      let aligned_sz = tmp_sz + (tmp_sz mod 16) in
      [ IJmp (Label end_label) ]
      @ [ IInstrComment (ILabel start_label, "Lambda Body") ]
        (* put the yield call here! *)
      @ [
          ILineComment "Adjust stack pointer";
          IPush (Reg RBP);
          IMov (Reg RBP, Reg RSP);
          ILineComment (sprintf "ISub RSP, %d" aligned_sz);
        ]
      @ List.map
          (fun _ -> IPush (Const 0L))
          (List.init (aligned_sz / 8) (fun _ -> 0))
      (* @ [
           ILineComment "Yield";
           IMov (Reg RDI, Reg RBP);
           IMov (Reg RSI, Reg RSP);
           ICall (Label "_loch_yield");
         ] *)
      @ [
          IInstrComment
            ( IMov (Reg scratch_reg, RegOffset (16, RBP)),
              "Move closure from first arg" );
        ]
      @ List.flatten
          (List.mapi
             (fun i x ->
               [
                 IInstrComment
                   ( IMov
                       ( Reg scratch_reg2,
                         RegOffset ((word_size * (3 + i)) - 5, scratch_reg) ),
                     sprintf "Free var %s" x );
                 IMov (find (find envs n_tag) x, Reg scratch_reg2);
               ])
             fv)
      @ [ ILineComment "Body Begin" ]
      @ body_instructions
      @ [ ILineComment "Body End" ]
      @ [
          IMov (Reg RSP, Reg RBP);
          IPop (Reg RBP);
          IRet;
          ILineComment "Creation of closure:";
        ]
      @ [ ILabel end_label ]
      @ (if allocd then [ ILineComment "Already Allocated" ]
         else reserve lambda_sz @ [ IMov (Reg scratch_reg, Reg RAX) ])
      @ [
          IInstrComment
            ( IMov
                ( Sized (QWORD_PTR, RegOffset (0, scratch_reg)),
                  Const (Int64.of_int (List.length bindings)) ),
              "Arity" );
          ILea (Reg scratch_reg2, Label start_label);
          IInstrComment
            ( IMov
                ( Sized (QWORD_PTR, RegOffset (word_size, scratch_reg)),
                  Reg scratch_reg2 ),
              "code pointer" );
          IMov
            ( Sized (QWORD_PTR, RegOffset (2 * word_size, scratch_reg)),
              Const (Int64.of_int sz) );
          ILineComment "Move free variables into closure below";
        ]
      @ List.flatten
          (List.mapi
             (fun i x ->
               [
                 IInstrComment
                   (IMov (Reg scratch_reg2, find env x), sprintf "Free var %s" x);
                 IMov
                   ( Sized
                       (QWORD_PTR, RegOffset ((i + 3) * word_size, scratch_reg)),
                     Reg scratch_reg2 );
               ])
             fv)
      @ [ IAdd (Reg RAX, Const closure_tag) ]
  | _ -> raise (InternalCompilerError "compile_clambda: expected CLambda")

and compile_cexpr (e : tag cexpr) (envs : arg envt envt) (ftag : string)
    (_ : int) (_ : bool) =
  let env = find envs ftag in
  let test_int label =
    [
      IMov (Reg scratch_reg, Reg RAX);
      IAnd (Reg RAX, Const 1L);
      ICmp (Reg RAX, Const 1L);
      IMov (Reg RAX, Reg scratch_reg);
      IJe (Label label);
    ]
  in
  let test_bool label =
    [
      IMov (Reg scratch_reg, Reg RAX);
      IAnd (Reg RAX, Const 15L);
      ICmp (Reg RAX, Const 15L);
      IMov (Reg RAX, Reg scratch_reg);
      IJne (Label label);
    ]
  in
  let test_int2 ex1 ex2 label env =
    let reg_ex1 = compile_imm ex1 env in
    let reg_ex2 = compile_imm ex2 env in
    ( [ IMov (Reg RAX, reg_ex1) ]
      @ test_int label
      @ [ IMov (Reg RAX, reg_ex2) ]
      @ test_int label,
      reg_ex1,
      reg_ex2 )
  in
  let test_bool2 ex1 ex2 label env =
    let reg_ex1 = compile_imm ex1 env in
    let reg_ex2 = compile_imm ex2 env in
    ( [ IMov (Reg RAX, reg_ex1) ]
      @ test_bool label
      @ [ IMov (Reg RAX, reg_ex2) ]
      @ test_bool label,
      reg_ex1,
      reg_ex2 )
  in
  let test_tuple label =
    (* test RAX *)
    [
      IMov (Reg scratch_reg, Reg RAX);
      IAnd (Reg RAX, Const tuple_mask);
      ICmp (Reg RAX, Const tuple_tag);
      IMov (Reg RAX, Reg scratch_reg);
      IJne (Label label);
    ]
  in
  let test_bounds mn mx less greater =
    (* tests if rax is in [mn, mx) *)
    (* test RAX - RAX *must* be an int*)
    [
      IMov (Reg scratch_reg, mn);
      ICmp (Reg RAX, Reg scratch_reg);
      IJl (Label less);
      IMov (Reg scratch_reg, mx);
      ICmp (Reg RAX, Reg scratch_reg);
      IJge (Label greater);
    ]
  in
  let test_nil =
    [ ICmp (Reg RAX, Const nil_value); IJe (Label "want_tuple_nil") ]
  in
  match e with
  | CPrim1 (p1, ex, t) -> (
      match p1 with
      | Add1 ->
          let comp_prev = compile_imm ex env in
          [ IMov (Reg RAX, comp_prev) ]
          @ test_int "want_int_arith"
          @ [ IAdd (Reg RAX, Const 2L); IJo (Label "overflow") ]
      | Sub1 ->
          let comp_prev = compile_imm ex env in
          [ IMov (Reg RAX, comp_prev) ]
          @ test_int "want_int_arith"
          @ [ ISub (Reg RAX, Const 2L); IJo (Label "overflow") ]
      | Print ->
          let comp_prev = compile_imm ex env in
          [ IMov (Reg RAX, comp_prev) ]
          @ [ IMov (Reg RDI, Reg RAX); ICall (Label "print") ]
      | IsBool ->
          let true_label = sprintf "is_bool_%d" t in
          let done_label = sprintf "is_bool_done_%d" t in
          let comp_prev = compile_imm ex env in
          let bool_instr =
            [
              IAnd (Reg RAX, Const bool_tag_mask);
              ICmp (Reg RAX, Const bool_tag);
              IJe (Label true_label);
              IMov (Reg RAX, const_false);
              IJmp (Label done_label);
              ILabel true_label;
              IMov (Reg RAX, const_true);
              ILabel done_label;
            ]
          in
          [ IMov (Reg RAX, comp_prev) ] @ bool_instr
      | IsNum ->
          let comp_prev = compile_imm ex env in
          let int_instr =
            [
              IShl (Reg RAX, Const 63L);
              IMov (Reg scratch_reg, const_true);
              IXor (Reg RAX, Reg scratch_reg);
            ]
          in
          [ IMov (Reg RAX, comp_prev) ] @ int_instr
      | Not ->
          let comp_prev = compile_imm ex env in
          [ IMov (Reg RAX, comp_prev) ]
          @ test_bool "want_bool_logic"
          @ [
              IMov (Reg scratch_reg, bool_mask); IXor (Reg RAX, Reg scratch_reg);
            ]
      | IsTuple ->
          let comp_prev = compile_imm ex env in
          let true_label = sprintf "tuple_%d" t in
          let done_label = sprintf "tuple_done_%d" t in
          [
            IMov (Reg RAX, comp_prev);
            IAnd (Reg RAX, Const tuple_mask);
            ICmp (Reg RAX, Const tuple_tag);
            IJe (Label true_label);
            IMov (Reg RAX, const_false);
            IJmp (Label done_label);
            ILabel true_label;
            IMov (Reg RAX, const_true);
            ILabel done_label;
          ]
      | PrintStack -> failwith "not implemented in loch :c"
      | Thread ->
          (* doesn't block*)
          let lam = compile_imm ex env in
          [
            IMov (Reg RAX, lam);
            IMov (Reg R11, Const closure_tag_mask);
            IAnd (Reg RAX, Reg R11);
            ICmp (Reg RAX, Const closure_tag);
            IJne (Label "want_closure_thread");
            IMov (Reg RDI, lam);
            ICall (Label "_loch_thread_create");
            IAdd (Reg RAX, Const thread_tag);
          ]
      | Get ->
          (* possibly block *)
          let th = compile_imm ex env in
          [
            IMov (Reg RAX, th);
            IMov (Reg R11, Const thread_mask);
            IAnd (Reg RAX, Reg R11);
            ICmp (Reg RAX, Const thread_tag);
            IJne (Label "want_thread_get");
            IMov (Reg RDI, th);
            IMov (Reg RSI, Reg RBP);
            IMov (Reg RDX, Reg RSP);
            ICall (Label "_loch_thread_get");
          ]
      | Start ->
          (* doesn't block*)
          let th = compile_imm ex env in
          [
            IMov (Reg RAX, th);
            IMov (Reg R11, Const thread_mask);
            IAnd (Reg RAX, Reg R11);
            ICmp (Reg RAX, Const thread_tag);
            IJne (Label "want_thread_start");
            IMov (Reg RDI, th);
            ICall (Label "_loch_thread_start");
          ]
      | Lock ->
          (* absolutely blocks*)
          let mtx = compile_imm ex env in
          let loop_label = sprintf "lock_loop_%d" t in
          let acq_label = sprintf "lock_acq_%d" t in
          [
            IMov (Reg RAX, mtx);
            IMov (Reg R11, Const mutex_mask);
            IAnd (Reg RAX, Reg R11);
            ICmp (Reg RAX, Const mutex_tag);
            IJne (Label "want_mutex_lock");
            ILabel loop_label;
            IMov (Reg R12, mtx);
            ISub (Reg R12, Const mutex_tag);
            IMov (Reg RAX, Const 1L);
            IXchg (Reg RAX, RegOffset (0, R12));
            ICmp (Reg RAX, Const 0L);
            IJe (Label acq_label);
            IMov (Reg RDI, Reg RSP);
            IMov (Reg RSI, Reg RBP);
            ICall (Label "_loch_yield");
            IJmp (Label loop_label);
            ILabel acq_label;
          ]
      | Unlock ->
          let mtx = compile_imm ex env in
          [
            IMov (Reg RAX, mtx);
            IMov (Reg R11, Const mutex_mask);
            IAnd (Reg RAX, Reg R11);
            ICmp (Reg RAX, Const mutex_tag);
            IJne (Label "want_mutex_unlock");
            IMov (Reg R12, mtx);
            ISub (Reg R12, Const mutex_tag);
            IMov (Reg RAX, Const 0L);
            IXchg (Reg RAX, RegOffset (0, R12));
          ])
  | CPrim2 (p2, ex1, ex2, t) -> (
      match p2 with
      | CheckSize -> failwith "no bueno"
      | Plus ->
          let test_instructions, reg_ex1, reg_ex2 =
            test_int2 ex1 ex2 "want_int_arith" env
          in
          test_instructions
          @ [
              IMov (Reg RAX, reg_ex1);
              IMov (Reg scratch_reg, reg_ex2);
              IAdd (Reg RAX, Reg scratch_reg);
              IJo (Label "overflow");
            ]
      | Minus ->
          let test_instructions, reg_ex1, reg_ex2 =
            test_int2 ex1 ex2 "want_int_arith" env
          in
          test_instructions
          @ [
              IMov (Reg RAX, reg_ex1);
              IMov (Reg scratch_reg, reg_ex2);
              ISub (Reg RAX, Reg scratch_reg);
              IJo (Label "overflow");
            ]
      | Times ->
          let test_instructions, reg_ex1, reg_ex2 =
            test_int2 ex1 ex2 "want_int_arith" env
          in
          test_instructions
          @ [
              IMov (Reg RAX, reg_ex1);
              ISar (Reg RAX, Const 1L);
              IMov (Reg scratch_reg, reg_ex2);
              IMul (Reg RAX, Reg scratch_reg);
              IJo (Label "overflow");
            ]
      | And ->
          let test_instructions, reg_ex1, reg_ex2 =
            test_bool2 ex1 ex2 "want_bool_logic" env
          in
          test_instructions
          @ [
              IMov (Reg RAX, reg_ex1);
              IMov (Reg scratch_reg, reg_ex2);
              IAnd (Reg RAX, Reg scratch_reg);
            ]
      | Or ->
          let test_instructions, reg_ex1, reg_ex2 =
            test_bool2 ex1 ex2 "want_bool_logic" env
          in
          test_instructions
          @ [
              IMov (Reg RAX, reg_ex1);
              IMov (Reg scratch_reg, reg_ex2);
              IOr (Reg RAX, Reg scratch_reg);
            ]
      | Greater ->
          let test_instructions, reg_ex1, reg_ex2 =
            test_int2 ex1 ex2 "want_int_comp" env
          in
          let jmp_label = sprintf "greater_%d" t in
          test_instructions
          @ [
              IMov (Reg RAX, reg_ex1);
              IMov (Reg scratch_reg, reg_ex2);
              ICmp (Reg RAX, Reg scratch_reg);
              IMov (Reg RAX, const_true);
              IJg (Label jmp_label);
              IMov (Reg RAX, const_false);
              ILabel jmp_label;
            ]
      | GreaterEq ->
          let test_instructions, reg_ex1, reg_ex2 =
            test_int2 ex1 ex2 "want_int_comp" env
          in
          let jmp_label = sprintf "greatereq_%d" t in
          test_instructions
          @ [
              IMov (Reg RAX, reg_ex1);
              IMov (Reg scratch_reg, reg_ex2);
              ICmp (Reg RAX, Reg scratch_reg);
              IMov (Reg RAX, const_true);
              IJge (Label jmp_label);
              IMov (Reg RAX, const_false);
              ILabel jmp_label;
            ]
      | Less ->
          let test_instructions, reg_ex1, reg_ex2 =
            test_int2 ex1 ex2 "want_int_comp" env
          in
          let jmp_label = sprintf "less_%d" t in
          test_instructions
          @ [
              IMov (Reg RAX, reg_ex1);
              IMov (Reg scratch_reg, reg_ex2);
              ICmp (Reg RAX, Reg scratch_reg);
              IMov (Reg RAX, const_true);
              IJl (Label jmp_label);
              IMov (Reg RAX, const_false);
              ILabel jmp_label;
            ]
      | LessEq ->
          let test_instructions, reg_ex1, reg_ex2 =
            test_int2 ex1 ex2 "want_int_comp" env
          in
          let jmp_label = sprintf "lesseq_%d" t in
          test_instructions
          @ [
              IMov (Reg RAX, reg_ex1);
              IMov (Reg scratch_reg, reg_ex2);
              ICmp (Reg RAX, Reg scratch_reg);
              IMov (Reg RAX, const_true);
              IJle (Label jmp_label);
              IMov (Reg RAX, const_false);
              ILabel jmp_label;
            ]
      | Eq ->
          let reg_ex1 = compile_imm ex1 env in
          let reg_ex2 = compile_imm ex2 env in
          let jmp_label = sprintf "eq_%d" t in
          [
            IMov (Reg RAX, reg_ex1);
            IMov (Reg scratch_reg, reg_ex2);
            ICmp (Reg RAX, Reg scratch_reg);
            IMov (Reg RAX, const_true);
            IJe (Label jmp_label);
            IMov (Reg RAX, const_false);
            ILabel jmp_label;
          ]
      | ScopedLock -> failwith "scoped lock not implemented yet")
  | CIf (cond, e1, e2, t) ->
      let comp_cond = compile_imm cond env in
      let comp_e1 = compile_aexpr e1 envs ftag 0 false in
      let comp_e2 = compile_aexpr e2 envs ftag 0 false in
      let test_b = test_bool "want_bool_if" in
      let else_label = sprintf "if_else_%d" t in
      let done_label = sprintf "if_done_%d" t in
      [ IMov (Reg RAX, comp_cond) ]
      @ test_b
      @ [
          IMov (Reg scratch_reg, const_true);
          ICmp (Reg RAX, Reg scratch_reg);
          IJne (Label else_label);
        ]
      @ comp_e1
      @ [ IJmp (Label done_label); ILabel else_label ]
      @ comp_e2 @ [ ILabel done_label ]
  | CImmExpr imm_e -> [ IMov (Reg RAX, compile_imm imm_e env) ]
  | CTuple (exprs, t) ->
      (* allocate the memory and then return the pointer *)
      let size = List.length exprs in
      let alloc_size = size + 1 + ((size + 1) mod 2) in
      (* pad to the right word size*)
      reserve alloc_size
      @ [
          IMov (Reg scratch_reg, Reg RAX);
          IMov
            ( Sized (QWORD_PTR, RegOffset (0, scratch_reg)),
              Const (Int64.of_int (2 * size)) );
        ]
      (* we store size as a SNAKEVAL LOL*)
      @ List.concat
          (List.mapi
             (fun i e ->
               let ans = compile_imm e env in
               [
                 IMov (Reg scratch_reg2, ans);
                 IMov
                   ( Sized
                       (QWORD_PTR, RegOffset ((i + 1) * word_size, scratch_reg)),
                     Reg scratch_reg2 );
               ])
             exprs)
      @ [ IMov (Reg RAX, Reg scratch_reg); IOr (Reg RAX, Const tuple_tag) ]
  | CGetItem (e1, e2, t) ->
      (* tuple get e1 *)
      let a1 = compile_imm e1 env in
      let a2 = compile_imm e2 env in
      let tup_test = test_tuple "want_tuple_get" in
      let int_test = test_int "want_int_get" in
      let index_test =
        test_bounds (Const 0L)
          (RegOffset (0, scratch_reg2))
          "want_index_small" "want_index_large"
      in
      [ IInstrComment (IMov (Reg RAX, a1), "Move tuple to RAX (Get Item)") ]
      (* move tuple to RAX *)
      @ List.concat [ tup_test; test_nil ]
      @ [ IMov (Reg RAX, a2) ] (* move index to RAX *)
      @ int_test
      @ [ IMov (Reg scratch_reg2, a1); ISub (Reg scratch_reg2, Const 1L) ]
        (* move actual pointer to RAX *)
      @ index_test (* test offset *)
      @ [
          IMov (Reg scratch_reg, a2);
          IShr (Reg scratch_reg, Const 1L);
          IInstrComment
            ( IMov
                ( Reg RAX,
                  Sized
                    ( QWORD_PTR,
                      RegOffsetReg (scratch_reg2, scratch_reg, word_size, 8) )
                ),
              "Move answer to RAX (Get Item)" );
        ]
  | CSetItem (e1, e2, e3, t) ->
      (* tuple set (e1 at e2) = e3 *)
      let a1 = compile_imm e1 env in
      let a2 = compile_imm e2 env in
      let a3 = compile_imm e3 env in
      let tup_test = test_tuple "want_tuple_set" in
      let int_test = test_int "want_int_set" in
      let index_test =
        test_bounds (Const 0L)
          (RegOffset (0, scratch_reg2))
          "want_index_small" "want_index_large"
      in
      [ IInstrComment (IMov (Reg RAX, a1), "Move tuple to RAX (SetItem)") ]
      @ List.concat [ tup_test; test_nil ]
      @ [
          IInstrComment (IMov (Reg RAX, a2), "Move int to test bounds (SetItem)");
        ]
      @ int_test
      @ [ IMov (Reg scratch_reg2, a1); ISub (Reg scratch_reg2, Const 1L) ]
      @ index_test
      @ [ IMov (Reg RAX, a3) ]
      @ [
          IMov (Reg scratch_reg, a2);
          IShr (Reg scratch_reg, Const 1L);
          IInstrComment
            ( IMov
                ( Sized
                    ( QWORD_PTR,
                      RegOffsetReg (scratch_reg2, scratch_reg, word_size, 8) ),
                  Reg RAX ),
              "Move value to offset (SetItem)" );
        ]
  | CApp (name, args, call, _) -> (
      match call with
      | Native fn -> (
          match name with
          | ImmId _ ->
              native_call (Label fn)
                (List.map (fun imm_exp -> compile_imm imm_exp env) args)
          | _ -> failwith "fhck ")
      | Snake -> compile_fun name args env
      | Prim -> failwith "feels like this isn't supposed to be here"
      | _ -> failwith "fhck")
  | CLambda _ -> compile_clambda e envs ftag false
  | CMutex _ -> [ ICall (Label "_loch_mutex_create") ]
(* TODO: NOT THIS*)

and compile_imm (e : tag immexpr) env =
  match e with
  | ImmNum (n, _) -> Const (Int64.shift_left n 1)
  | ImmBool (true, _) -> const_true
  | ImmBool (false, _) -> const_false
  | ImmId (x, _) -> find env x
  | ImmNil _ -> Const nil_value

and native_call label args =
  let rec zip_args regs args =
    match (regs, args) with
    | [], _ -> []
    | _, [] -> []
    | r :: rem_regs, a :: rem_args ->
        (Reg r, a)
        :: zip_args rem_regs rem_args (*should also save these dummies*)
  in
  let rec drop n l =
    match l with [] -> [] | _ :: rst -> if n = 0 then l else drop (n - 1) rst
  in
  let full_call =
    let rst = drop 6 args in
    let offset =
      if List.length rst mod 2 = 0 then []
      else [ IMov (Reg scratch_reg, pad_const); IPush (Reg scratch_reg) ]
    in
    offset
    @ List.flatten
        (List.map
           (fun a -> [ IMov (Reg scratch_reg, a); IPush (Reg scratch_reg) ])
           rst)
    @ [ ICall label ]
    @ List.map (fun _ -> IPop (Reg scratch_reg)) rst
    @ if offset = [] then [] else [ IPop (Reg scratch_reg) ]
  in
  let set_args = zip_args first_six_args_registers args in
  let add_args = List.map (fun (r, a) -> IMov (r, a)) set_args in
  let offset =
    if List.length set_args mod 2 = 0 then [] else [ Reg scratch_reg ]
  in
  [ ILineComment "Start of native call" ]
  (* move padding constant to make obvious*)
  @ List.concat_map (fun x -> [ IMov (x, pad_const); IPush x ]) offset (* pad *)
  @ add_args @ full_call
  @ List.map (fun x -> IPop x) offset
  @ [ ILineComment "End of native call" ]

(* reserves AND puts it into heap register. no need to increment though!*)
and reserve size =
  [
    ILineComment (sprintf "Reserving %d words" size);
    IMov (Reg RDI, Const (Int64.of_int size));
    IMov (Reg RSI, Reg RBP);
    IMov (Reg RDX, Reg RSP);
    ICall (Label "reserve");
  ]

let compile_prog ((anfed : tag aprogram), (env : arg envt envt)) : string =
  match anfed with
  | AProgram (body, _) ->
      let prologue =
        "section .text\n\
         extern equal\n\
         extern error\n\
         extern print\n\
         extern input\n\
         extern reserve\n\
         extern naive_print_heap\n\
         extern _loch_thread_create\n\
         extern _loch_thread_get\n\
         extern _loch_thread_start\n\
         extern _loch_yield\n\
         extern _loch_set_stack\n\
         extern HEAP\n\
         extern HEAP_END\n\
         extern set_stack_bottom\n\
         global our_code_starts_here\n\
         global thread_code_starts_here\n"
      in
      let thread_code_start_here_code =
        [
          ILabel "thread_code_starts_here";
          IPush (Reg RBP);
          IMov (Reg RBP, Reg RSP);
          IPush (Reg R12);
          IPush (Reg R13);
          IPush (Reg R14);
          IPush (Reg R15);

          IInstrComment
            (IMov (Reg scratch_reg, Reg RDI), "Move closure to scratch_reg");
          IMov (Reg RDI, Reg RBP);
          ICall (Label "_loch_set_stack");
          IPush (Reg scratch_reg);
          ISub (Reg scratch_reg, Const closure_tag);
          IMov (Reg RAX, RegOffset (8, scratch_reg));
          ICall (Reg RAX);
          IPop (Reg R15);
          IPop (Reg R15);
          IPop (Reg R14);
          IPop (Reg R13);
          IPop (Reg R12);
          IMov (Reg RSP, Reg RBP);
          IPop (Reg RBP);
          IRet;
        ]
      in
      let global_env = find env glob_name in
      let tmp_sz = deepest_stack global_env in
      let aligned_sz = tmp_sz + (tmp_sz mod 16) in
      let stack_setup =
        [
          IPush (Reg RBP);
          IMov (Reg RBP, Reg RSP);
          ILineComment (sprintf "ISub RSP, %d" aligned_sz);
        ]
        @ List.map
            (fun _ -> IPush (Const 0L))
            (List.init (aligned_sz / 8) (fun x -> x))
        @ [ IMov (Reg scratch_reg2, Reg RDI) ]
        @ native_call (Label "set_stack_bottom") [ Reg RBP ]
        @ [ IMov (Reg RDI, Reg scratch_reg2) ]
      in

      let body_prologue = [] in
      let body_epilogue = [ IMov (Reg RSP, Reg RBP); IPop (Reg RBP); IRet ] in
      let comp_body =
        [ ILineComment "Body start!**" ]
        @ compile_aexpr body env glob_name 0 false
      in
      let error_handle =
        to_asm
          [
            ILabel "want_int_comp";
            IMov (Reg RDI, Const 1L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_int_arith";
            IMov (Reg RDI, Const 2L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_bool_logic";
            IMov (Reg RDI, Const 3L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_bool_if";
            IMov (Reg RDI, Const 4L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "overflow";
            IMov (Reg RDI, Const 5L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_tuple_get";
            IMov (Reg RDI, Const 6L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_int_get";
            IMov (Reg RDI, Const 7L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_tuple_set";
            IMov (Reg RDI, Const 8L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_int_set";
            IMov (Reg RDI, Const 9L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "out_of_memory";
            IMov (Reg RDI, Const 10L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_index_small";
            IMov (Reg RDI, Const 11L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_index_large";
            IMov (Reg RDI, Const 12L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_tuple_nil";
            IMov (Reg RDI, Const 13L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "not_closure";
            IMov (Reg RDI, Const 14L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "closure_arity";
            IMov (Reg RDI, Const 15L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_mutex_lock";
            IMov (Reg RDI, Const 16L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_mutex_unlock";
            IMov (Reg RDI, Const 17L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_mutex_scoped";
            IMov (Reg RDI, Const 18L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_lambda_scoped";
            IMov (Reg RDI, Const 19L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_closure_thread";
            IMov (Reg RDI, Const 20L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_thread_start";
            IMov (Reg RDI, Const 21L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
            ILabel "want_thread_get";
            IMov (Reg RDI, Const 22L);
            IMov (Reg RSI, Reg RAX);
            ICall (Label "error");
          ]
      in
      let main =
        to_asm (body_prologue @ stack_setup @ comp_body @ body_epilogue)
      in
      prologue ^ "\nour_code_starts_here:" ^ main ^ error_handle
      ^ to_asm thread_code_start_here_code

(* Feel free to add additional phases to your pipeline.
   The final pipeline phase needs to return a string,
   but everything else is up to you. *)

let run_if should_run f = if should_run then f else no_op_phase

let compile_to_string ?(no_builtins = false)
    (prog : sourcespan program pipeline) : string pipeline =
  prog
  |> add_phase desugared desugar1
  |> run_if (not no_builtins) (add_phase add_natives add_native_lambda)
  |> add_err_phase well_formed is_well_formed
  |> add_phase desugared desugar2
  |> add_phase tagged tag
  |> add_phase renamed rename_and_tag
  |> add_phase anfed (fun p -> atag (anf p))
  |> add_phase locate_bindings naive_stack_allocation
  |> add_phase result compile_prog
