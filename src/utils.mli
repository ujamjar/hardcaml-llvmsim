module St = HardCaml.Signal.Types

val (>>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val (<<) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val (|>) : 'a -> ('a -> 'b) -> 'b

val global_context : unit -> Llvm.llcontext
val builder : unit -> Llvm.llbuilder

val platform_bits : int
val pbits : int -> int

val int_type : int -> Llvm.lltype

val const_int : int -> int -> Llvm.llvalue
val const_of_signal : St.signal -> Llvm.llvalue
val const_string : string -> Llvm.llvalue

val int64 : Llvm.lltype
val int32 : Llvm.lltype
val int8 : Llvm.lltype
val void : Llvm.lltype
val ptr_type : int -> Llvm.lltype -> Llvm.lltype

val zero64 : Llvm.llvalue
val zero32 : Llvm.llvalue
val zero8 : Llvm.llvalue

val build_uresize : Llvm.llvalue -> int -> int -> string -> Llvm.llbuilder -> Llvm.llvalue
val make_switch : int -> Llvm.llvalue -> Llvm.llbasicblock list -> Llvm.llbuilder -> unit
val append_block : string -> Llvm.llvalue -> Llvm.llbasicblock 

type 'a func = 
  {
    func : Llvm.llvalue;
    builder : Llvm.llbuilder;
    in_entry : (Llvm.llbuilder -> 'a) -> 'a;
  }

val make_function : 
  Llvm.llmodule -> string -> 
  Llvm.lltype -> Llvm.lltype array -> 
  ('b func -> 'a) -> 'a

val name : string -> St.signal -> string
val memsize : St.signal -> int

val split : int -> 'a list -> 'a list * 'a list

