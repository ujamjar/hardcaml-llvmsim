/* These functions are used to take 64 or 32 bit 
 * pointers returned from the LLVM module and
 * wrap them as 32 or 64 bit Bigarray's
 *
 * An assumption is made here that 
 * sizeof(long) == sizeof(Nativeint)
 * and therefore the integer value of a pointer
 * is passed from ocaml via a nativeint.
 *
 */
#include <stdio.h>
#include <sys/types.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/memory.h>

CAMLprim value llvmsim_get_ptr(value ptr, value bits) {
    CAMLparam2(ptr, bits);
    void *pp;
    long p = Nativeint_val(ptr);
    long words = Int_val(bits);
    int nbits = sizeof(long) * 8;

    words = (words + nbits - 1) / nbits;
    p = Nativeint_val(ptr);
    pp = (void *)p;
    
    // Allocate a big array wrapping the memory
    CAMLreturn 
        (alloc_bigarray_dims(BIGARRAY_NATIVE_INT|BIGARRAY_C_LAYOUT, 
                        1, pp, words));
}

