open Llvm
open HardCaml.Signal.Types

type global_type = G_Port | G_Internal | G_Reg | G_Mem
type global = 
  {
    width : int;
    rnd_width : int;
    cur : llvalue;
    next : llvalue;
    typ : global_type;
  }

type global_simple = bool -> int -> uid -> global
type global_reg = int -> uid -> global
type global_mem = int -> int -> uid -> global
type global_fns = global_simple * global_reg * global_mem

val globals : llmodule -> global UidMap.t ref * global_fns

type load_simple = bool -> signal -> llvalue
type load_reg = signal -> llvalue
type load_mem = llvalue -> signal -> llvalue
type load_fns = load_simple * load_reg * load_mem

val load : llvalue -> global_fns -> load_fns

type store_simple = llvalue -> bool -> signal -> unit
type store_reg = llvalue -> signal -> unit
type store_mem = llvalue -> signal -> unit
type store_fns = store_simple * store_reg * store_mem

val store : llbuilder -> global_fns -> store_fns

type update_reg = signal -> unit
type update_mem = llvalue -> signal -> unit
type update_fns = update_reg * update_mem

val update : llbuilder -> global_fns -> update_fns

val load_signal : load_fns -> llvalue UidMap.t -> signal -> llvalue

