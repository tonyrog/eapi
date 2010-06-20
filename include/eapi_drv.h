//
// Support function that be used by driver
//
#ifndef __EAPI_DRV_H__
#define __EAPI_DRV_H__

#include <stdarg.h>
#include <stdio.h>

#if defined(__x86_64__)
#define WORDSIZE 64
#else
#define WORDSIZE 32
#endif

#define CBUF_USE_PUT_CTI
#define CBUF_USE_PUT_TRM

#include "erl_driver.h"
#include "cbuf.h"
#include "lhash.h"

// Type map to external communication
// Assume SHORT == INT16
#define SHORT                    INT16
#define USHORT                   UINT16
#define cbuf_get_ushort(in, ptr) cbuf_get_uint16((in),(uint32_t*)(ptr))
#define cbuf_get_short(in, ptr)  cbuf_get_int16((in),(int32_t*)(ptr))

// Asume INT == INT32  FIXME
#define INT                       INT32
#define UINT                      UINT32
#define cbuf_get_uint(in, ptr)    cbuf_get_uint32((in),(uint32_t*)(ptr))
#define cbuf_get_int(in, ptr)     cbuf_get_int32((in),(int32_t*)(ptr))
#define cbuf_put_int(out, val)    cbuf_put_int32((out),(int32_t)(val))
#define cbuf_put_uint(out, val)   cbuf_put_uint32((out),(uint32_t)(val))

#if WORDSIZE == 32
#define LONG                       INT32
#define ULONG                      UINT32
#define cbuf_get_ulong(in, ptr)    cbuf_get_uint32((in),(uint32_t*)(ptr))
#define cbuf_get_long(in, ptr)     cbuf_get_int32((in),(int32_t*)(ptr))
#elif WORDSIZE == 64
#define LONG                       INT64
#define ULONG                      UINT64
#define cbuf_get_ulong(in, ptr)    cbuf_get_uint64((in),(uint64_t*)(ptr))
#define cbuf_get_long(in, ptr)     cbuf_get_int64((in),(int64_t*)(ptr))
#endif

#if WORDSIZE == 32
#define SIZE_T                     UINT32
#define SSIZE_T                    INT32
#define cbuf_get_size(in, ptr)     cbuf_get_uint32((in),(uint32_t*)(ptr))
#define cbuf_get_ssize(in, ptr)    cbuf_get_int32((in),(int32_t*)(ptr))
#define cbuf_get_size_fn           (get_fn_t) cbuf_get_uint32
#elif WORDSIZE == 64
#define SIZE_T                     UINT64
#define SSIZE_T                    INT64
#define cbuf_get_size(in, ptr)     cbuf_get_uint64((in),(uint64_t*)(ptr))
#define cbuf_get_ssize(in, ptr)    cbuf_get_int64((in),(int64_t*)(ptr))
#define cbuf_get_size_fn           (get_fn_t) cbuf_get_uint64
#endif

#if WORDSIZE == 32
#define POINTER_T                   UINT32
typedef uint32_t                    pointer_t;
#define cbuf_put_pointer(out, ptr)  cbuf_put_uint32((out),(uint32_t)(ptr))
#define cbuf_get_pointer(in, ptr)   cbuf_get_uint32((in),(uint32_t*)(ptr))
#define cbuf_get_pointer_fn         cbuf_get_uint32
#elif WORDSIZE == 64
#define POINTER_T                   UINT64
typedef uint64_t                    pointer_t;
#define cbuf_put_pointer(out, val)  cbuf_put_uint64((out),(uint64_t)(val))
#define cbuf_get_pointer(in, ptr)   cbuf_get_uint64((in),(uint64_t*)(ptr))
#define get_pointer_fn              cbuf_get_uint64
#endif

#define EPTR_HANDLE(ptr) ((pointer_t)(ptr))

#define STRING             STRING4

/* environment */
typedef struct {
    ErlDrvPort  port;      // Port reference
    lhash_t     ref;       // NativePointer => Handle -> NativPointer
    ErlDrvTid   tid;       // Event thread dispatcher
    ErlDrvEvent evt[2];    // Thread events evt[0]=main size, evt[1]=thread side
    uint32_t    eref;      // event reference for event replies
    void*       user_data; // Data used by implementation
} eapi_ctx_t;

/* key value pairs */
typedef struct {
    char*          key; // ErlDrvTermData key;     // Atom key
    uint32_t       value;
} eapi_kv_t;

typedef struct {
    unsigned int offset;
    unsigned int len;
    ErlDrvBinary* bin;
} eapi_binary_t;

typedef struct {
    unsigned int len;
    char* buf;
} eapi_string_t;

// "Magic" Driver object
extern void* driver_alloc_object(size_t size, void* user_data, int user_type,
				 void (*final)(void* ptr, void* user_data));
extern void* driver_alloc_object_ptr(void* ptr, void* user_data, int user_type,
				  void (*final)(void** ptr, void* user_data));
extern void* driver_realloc_object(void* ptr, size_t size);
extern void  driver_free_object(void* ptr);
// "Magic" to send_term things (use ERL_DRV_BINARY tag!)
extern ErlDrvTermData driver_object_term(void* ptr);
extern ErlDrvTermData driver_object_size(void* ptr);
extern ErlDrvTermData driver_object_offset(void* ptr);
extern ErlDrvBinary*  driver_object_binary(void* ptr, int user_type);
extern void* driver_binary_to_object_ptr(ErlDrvBinary* bin, int user_type);

#ifdef DEBUG
extern void eapi_drv_emit_error(char* file, int line, ...);
#endif


static inline int cbuf_get_binary(cbuf_t* c_in,uint32_t n,eapi_binary_t* bp)
{
    // detect contigous binary memory and use it
    if (n && (bp->bin=c_in->v[c_in->iv].bp) && (cbuf_seg_r_avail(c_in) >= n)) {
	driver_binary_inc_refc(bp->bin);
	bp->offset = c_in->ip;
	bp->len = n;
	cbuf_forward(c_in, n);
	return 0;
    }
    else {
	if (!(bp->bin = driver_alloc_binary(n)))
	    return -1;  // report ? how ?
	bp->offset = 0;
	bp->len = n;
	return (cbuf_read(c_in, bp->bin->orig_bytes, n) == (int)n);
    }
}

// load a binary, size is reset of cbuf
static inline int cbuf_get_binary_tail(cbuf_t* c_in, eapi_binary_t* bp)
{
    uint32_t n = cbuf_r_avail(c_in);
    return cbuf_get_binary(c_in, n, bp);
}


// load a binary, size is uint32 prefix
static inline int cbuf_get_nbinary(cbuf_t* c_in, eapi_binary_t* bp)
{
    uint32_t n;
    int r;
    if ((r = cbuf_get_uint32(c_in, &n)) > 0)
	return cbuf_get_binary(c_in,n,bp);
    return r;
}

// release a binary object
static void cbuf_free_binary(eapi_binary_t* bp)
{
    driver_free_binary(bp->bin);
}

// load a string, size is uint32 prefix
static inline int cbuf_get_nstring(cbuf_t* c_in, eapi_string_t* sp)
{
    uint32_t n;
    int r;

    if ((r = cbuf_get_uint32(c_in, &n)) > 0) {
	if (!(sp->buf = malloc(n)))
	    return -1;
	sp->len = n;
	return (cbuf_read(c_in, sp->buf, n) == (int) n);
    }
    return r;
}

static inline void cbuf_free_string(eapi_string_t* sp)
{
    if (sp->buf) {
	free(sp->buf);
	sp->buf = 0;
	sp->len = 0;
    }
}

static inline int cbuf_get_object(cbuf_t* c_in, void** obj)
{
    ErlDrvBinary* bin;
    size_t n;
	
    if ((bin=c_in->v[c_in->iv].bp) && 
	(n = c_in->v[c_in->iv].size) && (c_in->ip == 0)) {
	*obj = driver_binary_to_object_ptr(bin, 0);
	cbuf_forward(c_in, n);
	return 1;
    }
    return 0;
}

// debug only!
#ifdef DEBUG
#define CBUF_DBG(buf,msg) cbuf_print((buf),(msg))
// #define CBUF_DBG(buf,msg)
#define EAPI_DRV_DBG(...) eapi_drv_emit_error(__FILE__,__LINE__,__VA_ARGS__)
#else
#define CBUF_DBG(buf,msg)
#define EAPI_DRV_DBG(...)
#endif

extern void eapi_driver_init(ErlDrvEntry* ptr,
			     void (*impl_init)(ErlDrvData d),
			     void (*impl_finish)(ErlDrvData d));

extern void eapi_emit_error(char* file, int line, ...);

extern int eapi_get_pointer(eapi_ctx_t* ctx, cbuf_t* c_in, void** ptr);
extern int eapi_put_pointer(eapi_ctx_t* ctx, cbuf_t* c_out, void* ptr);
extern void eapi_put_error(cbuf_t* c_out, char* err);
extern pointer_t eapi_drv_ptr(eapi_ctx_t* ctx, void* ptr);


//
// This function is normally generated
// The dispatch interface is wrapped in
//   eapi_drv_ctl
//   eapi_drv_output
//   eapi_drv_outputv
//
extern int eapi_dispatch(eapi_ctx_t* ctx, unsigned int cmd,
			 cbuf_t* c_in, cbuf_t* c_out);

#endif
