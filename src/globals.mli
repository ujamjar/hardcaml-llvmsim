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

type load_simple = bool -> signal -> llvalue
type load_reg = signal -> llvalue
type load_mem = llvalue -> signal -> llvalue

type store_simple = llvalue -> bool -> signal -> unit
type store_reg = llvalue -> signal -> unit
type store_mem = llvalue -> signal -> unit

type update_reg = signal -> unit
type update_mem = llvalue -> signal -> unit

type globals = 
  {
    gmap : global UidMap.t ref;
    gsimple : global_simple;
    greg : global_reg;
    gmem : global_mem;
    fscope : llvalue Utils.func -> func;
  }

and func = 
  {
    loads : loads;
    stores : stores;
    updates : updates;
  }

and loads = 
  {
    lsimple : load_simple;
    lreg : load_reg;
    lmem : load_mem;
  }

and stores = 
  {
    ssimple : store_simple;
    sreg : store_reg;
    smem : store_mem;
  }

and updates = 
  {
    ureg : update_reg;
    umem : update_mem;
  }

val global_fns : Llvm.llmodule -> globals

val load_signal : ?rd_mem:bool -> func -> llvalue UidMap.t -> signal -> llvalue UidMap.t * llvalue

val store_signal : func -> Llvm.llvalue -> signal -> unit

