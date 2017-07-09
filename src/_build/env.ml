open Types
open Symtab
module A = Ast

type valty = 
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VString of string
  | VNil of unit
  (*用Association list来表示记录*)
  | VRecord of (symbol * valty) list
  (*用array来表示数组*)
  | VArray of valty array
  (*用args+return+body表示函数*)
  | VFunction of symbol list  * A.exp


(*值环境 *)
type venv = ty t

(*类型环境 *)
type tenv = ty t
(*环境包含了求值一个表达式的全部要素*)
type env = 
  {mutable v:venv;
   mutable t:tenv;
   inloop:bool;
   }

type valenv = valty t ref

let get_by_sb (s:symbol) (e:valenv) : valty=
  match get s !e with
  | Some t -> t
  | None -> failwith "can't find related sb"

(*可变绑定--修改*)
let bind_sb (s:symbol) (e:valenv) (v:valty) =
  e:= set s v !e 
(*不可变绑定--新建*)
let create_sb (s:symbol) (e:valenv) (v:valty) : valenv =
  let env=set s v !e in
  ref env 


let create_env ?(v=create ()) ?(t=create ()) ?(inloop = false) : env = {
  v=v;
  t=t;
  inloop=inloop;
}

(*在旧环境的基础上扩展新环境*)
let add_venv (k:symbol) (v:ty) (env:env) =
  {env with v = set k v env.v}

(*修改旧环环境*)
let update_venv (k:symbol) (v:ty) (env:env) =
  env.v <- set k v env.v

let lookup_venv (k:symbol) (env:env) =
  match (get k env.v) with
  | Some v -> v
  | None -> failwith "can't find var in venv"

let add_tenv (k:symbol) (t:ty) (env:env) =
  {env with t = set k t env.t}

let update_tenv (k:symbol) (t:ty) (env:env) =
  env.t <- set k t env.t

let lookup_tenv (k:symbol) (env:env) =
  match (get k env.t) with
  | Some t -> t
  | None -> failwith "can't find ty in tenv"