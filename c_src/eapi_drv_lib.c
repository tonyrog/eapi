//
// Erlang driver wrapper library
//
// Implements a generic driver interface
//
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#include "eapi_drv.h"

// debug async events
#define A_DBG(...) EAPI_DRV_DBG(__VA_ARGS__)

static void       (*eapi_drv_impl_init)(ErlDrvData d) = 0;
static void       (*eapi_drv_impl_finish)(ErlDrvData d) = 0;

static int        eapi_drv_init(void);
static void       eapi_drv_finish(void);
static void       eapi_drv_stop(ErlDrvData);
static void       eapi_drv_output(ErlDrvData, char*, int);
static void       eapi_drv_outputv(ErlDrvData, ErlIOVec*);
static void       eapi_drv_ready_input(ErlDrvData, ErlDrvEvent);
static void       eapi_drv_ready_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData eapi_drv_start(ErlDrvPort, char* command);
static int        eapi_drv_ctl(ErlDrvData,unsigned int,char*, int,char**,int);
static void       eapi_drv_timeout(ErlDrvData);

typedef struct _eapi_drv_object_t {
    lhash_bucket_t   hbucket;
    eapi_ctx_t*      ctx;
    void*            opaque;
} eapi_drv_object_t;

#ifdef DEBUG
void eapi_drv_emit_error(char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    va_start(ap, line);
    fmt = va_arg(ap, char*);

    fprintf(stderr, "%s:%d: ", file, line); 
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\r\n");
    va_end(ap);
}
#endif

static lhash_value_t ref_hash(void* key)
{
    return (lhash_value_t) key;
}

static int ref_cmp(void* key, void* data)
{
    if (((pointer_t)key) == EPTR_HANDLE(((eapi_drv_object_t*)data)->opaque))
	return 0;
    return 1;
}

static void ref_release(void *data)
{
    driver_free(data);
}


// Translate internal pointer to extern object handle
pointer_t eapi_drv_ptr(eapi_ctx_t* ctx, void* ptr)
{
    void* key = (void*) EPTR_HANDLE(ptr);
    if (lhash_lookup(&ctx->ref, key))
	return (pointer_t) key;
    return 0;
}

static inline eapi_drv_object_t* eapi_drv_object(eapi_ctx_t* ctx, pointer_t handle)
{
    eapi_drv_object_t* obj = (eapi_drv_object_t*) lhash_lookup(&ctx->ref,(void*)handle);
    if (obj)
	return obj;
    return 0;
}

// Translate eapi_drv_object_t external key 
static inline pointer_t eapi_drv_handle(eapi_drv_object_t* obj)
{
    if (obj)
	return EPTR_HANDLE(obj->opaque);
    return 0;
}

// Load and Translate external pointer
int eapi_get_pointer(eapi_ctx_t* ctx, cbuf_t* c_in, void** ptr)
{
    pointer_t handle;
    eapi_drv_object_t* obj;

    if (!cbuf_get_pointer(c_in, &handle))
	return 0;
    obj = eapi_drv_object(ctx, handle);
    *ptr = (void*) eapi_drv_handle(obj);
    return 1;
}

// Lookup/Insert and encode pointer
int eapi_put_pointer(eapi_ctx_t* ctx, cbuf_t* c_out, void* ptr)
{
    pointer_t p = 0;
    if (ptr) {
	if (!(p = eapi_drv_ptr(ctx, ptr))) {
	    eapi_drv_object_t* obj = malloc(sizeof(eapi_drv_object_t));
	    p = EPTR_HANDLE(ptr);
	    obj->ctx = ctx;
	    obj->opaque = ptr;
	    lhash_insert_new(&ctx->ref, (void*)p, obj);
	}
    }
    return cbuf_put_pointer(c_out, p);
}

// match ERL_SMALL_TO_BIN_LIMIT & ERL_ONHEAP_BIN_LINIT
// 4*ERL_ONHEAP_BIN_LIMIT
#define ERL_DRV_OBJECT_MIN_SIZE     (256+sizeof(void*))
// match BIN_FLAG_MAGIC
#define ERL_DRV_OBJECT_MAGIC_FLAG   1

// Mimic the Binary structure
typedef struct _erl_drv_object_t
{
    unsigned long flags;
    unsigned long refc;
#if WORDSIZE == 32
    uint32_t align;     // used for pointer alignment
#endif
    unsigned long  orig_size;
    // orig_bytes
    void (*destructor)(struct _erl_drv_object_t* obj);
    void*  user_data;
    int    user_type;
    void (*final)(void* object, void* user_data);
    char data[0];
} ErlDrvObject;

#define ErlDrvObjectOverhead (3*sizeof(void*))

#define ErlDrvBinary2ErlDrvObject(D) \
    ((ErlDrvObject *)				  \
     (((char *) (D)) -					\
      ((char *) &(((ErlDrvObject *) 0)->orig_size))))

#define Ptr2ErlDrvObject(ptr) \
    ((ErlDrvObject*) ((char*)(ptr) - sizeof(ErlDrvObject)))

#define Ptr2ErlDrvBinary(ptr) \
    ((ErlDrvBinary*) &(Ptr2ErlDrvObject(ptr)->orig_size))


// The eapi_object_destruct will be call by GC (and driver_free_binary)
static void eapi_object_destruct(struct _erl_drv_object_t* obj)
{
    fprintf(stderr, "eapi_object_destruct: %p\r\n", obj);
    (*obj->final)(&obj->data[0], obj->user_data);
}

//
// Create a ErlDrvObject
// The pointer returned point to the ErlDrvObject->data
// This pointer must be casted when sent to driver_send_term
//
void* driver_alloc_object(size_t size, void* user_data, int user_type,
			  void (*final)(void*, void*))
{
    ErlDrvObject* obj;
    ErlDrvBinary* bin;
    size_t orig_size = ErlDrvObjectOverhead + size;

    if (orig_size < ERL_DRV_OBJECT_MIN_SIZE)
	orig_size = ERL_DRV_OBJECT_MIN_SIZE;
    bin = driver_alloc_binary(orig_size);
    if (!bin)
	return 0;
    obj = ErlDrvBinary2ErlDrvObject(bin);
    obj->flags |= ERL_DRV_OBJECT_MAGIC_FLAG;
    obj->user_data     = user_data;
    obj->user_type     = user_type;
    obj->destructor    = eapi_object_destruct;
    obj->final         = final;
    return &obj->data[0];
}

// As driver_alloc object, but just create a "handle"
void* driver_alloc_object_ptr(void* ptr, void* user_data, int user_type,
			      void (*final)(void**, void*))
{
    void* p;

    if (!(p = driver_alloc_object(sizeof(void*), user_data, user_type, 
				  (void (*)(void*,void*)) final)))
	return 0;
    *((void**)p) = ptr;
    return p;
}


void* driver_realloc_object(void* ptr, size_t size)
{
    ErlDrvBinary* bin;
    ErlDrvObject* obj;
    size_t new_size;

    if (!ptr)
	return 0;
    bin = Ptr2ErlDrvBinary(ptr);
    new_size = ErlDrvObjectOverhead + size;
    if (new_size < ERL_DRV_OBJECT_MIN_SIZE)
	new_size = ERL_DRV_OBJECT_MIN_SIZE;
    if (!(bin = driver_realloc_binary(bin, new_size)))
	return 0;
    obj = ErlDrvBinary2ErlDrvObject(bin);
    return &obj->data[0];
}

void driver_free_object(void* ptr)
{
    if (ptr)
	driver_free_binary(Ptr2ErlDrvBinary(ptr));
}

ErlDrvTermData driver_object_term(void* ptr)
{
    if (ptr)
	return (ErlDrvTermData) Ptr2ErlDrvBinary(ptr);
    return 0;
}

// This make sure that erlang side wilö no see overhead data,
// only object data (use in send_term data)
ErlDrvTermData driver_object_size(void* ptr)
{
    if (ptr)
	return Ptr2ErlDrvObject(ptr)->orig_size - ErlDrvObjectOverhead;
    return 0;
}

// offset in object to be used in send_term data
ErlDrvTermData driver_object_offset(void* ptr)
{
    if (ptr)
	return ErlDrvObjectOverhead;
    return 0;
}

// Convert a driver binary to an object and do some checks
void* driver_binary_to_object_ptr(ErlDrvBinary* bin, int user_type)
{
    if (bin && (bin->orig_size >= (int) ErlDrvObjectOverhead)) {
	ErlDrvObject* obj = ErlDrvBinary2ErlDrvObject(bin);
	if ((obj->user_type == user_type) &&
	    (obj->destructor == eapi_object_destruct))
	    return &obj->data[0];
    }
    return 0;
}

// Convert driver_object to ErlDrvBinary*
ErlDrvBinary*  driver_object_binary(void* ptr, int user_type)
{
    if (ptr) {
	ErlDrvObject* obj = Ptr2ErlDrvObject(ptr);
	if ((obj->orig_size >= (int) ErlDrvObjectOverhead) &&
	    (obj->user_type == user_type) &&
	    (obj->destructor == eapi_object_destruct))
	    return Ptr2ErlDrvBinary(ptr);
    }
    return 0;
}

// Write ERROR,ATOM,String
void eapi_put_error(cbuf_t* c_out, char* err)
{
    cbuf_put_tuple_begin(c_out, 2);
    cbuf_put_tag_error(c_out);
    cbuf_put_atom(c_out, err);
    cbuf_put_tuple_end(c_out, 2);
}

void eapi_driver_init(ErlDrvEntry* ptr,
		      void (*impl_init)(ErlDrvData d),
		      void (*impl_finish)(ErlDrvData d))

{
    eapi_drv_impl_init   = impl_init;
    eapi_drv_impl_finish = impl_finish;
    ptr->init  = eapi_drv_init;
    ptr->start = eapi_drv_start;
    ptr->stop  = eapi_drv_stop;
    ptr->output = eapi_drv_output;
    ptr->ready_input  = eapi_drv_ready_input;
    ptr->ready_output = eapi_drv_ready_output;
    ptr->finish = eapi_drv_finish;
    ptr->control = eapi_drv_ctl;
    ptr->timeout = eapi_drv_timeout;
    ptr->outputv = eapi_drv_outputv;
    ptr->ready_async = 0;
    ptr->flush = 0;
    ptr->call = 0;
    ptr->event = 0;
    ptr->extended_marker = ERL_DRV_EXTENDED_MARKER;
    ptr->major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    ptr->minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    ptr->driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
    ptr->process_exit = 0;
    ptr->stop_select = 0;  // add me
};

/* setup global object area */
static int eapi_drv_init(void)
{
    return 0;
}

static void eapi_drv_finish(void)
{
}

static ErlDrvData eapi_drv_start(ErlDrvPort port, char* command)
{
    eapi_ctx_t* ctx;
    (void) command;

    if ((ctx = (eapi_ctx_t*) driver_alloc(sizeof(eapi_ctx_t))) != NULL) {
	lhash_func_t func = { ref_hash, ref_cmp, ref_release, 0 };
	ctx->port = port;
	ctx->eref = 0xFEEDBABE;  // random start
	lhash_init(&ctx->ref, "ref", 2, &func);
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
	if (eapi_drv_impl_init)
	    (*eapi_drv_impl_init)((ErlDrvData)ctx);
	return (ErlDrvData) ctx;
    }
    return ERL_DRV_ERROR_ERRNO;    
}

static void eapi_drv_stop(ErlDrvData d)
{
    eapi_ctx_t* ctx = (eapi_ctx_t*) d;

    if (eapi_drv_impl_finish)
	(*eapi_drv_impl_finish)(d);
    lhash_delete(&ctx->ref);
    driver_free(ctx);
}

static int eapi_drv_ctl(ErlDrvData d, 
		    unsigned int cmd, char* buf, int len,
		    char** rbuf, int rsize)
{
    eapi_ctx_t* ctx = (eapi_ctx_t*) d;
    cbuf_t  arg;    // argument data stream
    cbuf_t  reply;  // reply data stream

    // input data
    cbuf_init(&arg, (unsigned char*) buf, len, 0, 0, 0);
    CBUF_DBG(&arg, "ctl_arg");
    // default data - will relloacte (as binary) on overflow
    cbuf_init(&reply, (unsigned char*) *rbuf, rsize, 0,
	      CBUF_FLAG_BINARY | CBUF_FLAG_PUT_CTI, 0);

    cbuf_put_begin(&reply);  // put respose header    

    eapi_dispatch(ctx, cmd, &arg, &reply); // FIXME: check return

    cbuf_put_end(&reply);  // put response footer
    
    CBUF_DBG(&reply, "ctl_reply");

    if (reply.vlen == 1) {
	if (reply.v[0].bp) {
	    cbuf_trim(&reply);
	    *rbuf = (char*) reply.v[0].bp;
	}
	else
	    *rbuf = (char*) reply.v[0].base;
	return cbuf_seg_used(&reply);
    }
    else {
	// FIXME return a event handle and do a send_term
	// example: get_program_info(Program, binaries) 
	fprintf(stderr, "FIXME:vector reply!\r\n");
	return 0;
    }    
}

static void eapi_drv_output(ErlDrvData d, char* buf, int len)
{
    eapi_ctx_t*   ctx = (eapi_ctx_t*) d;
    cbuf_t     arg;    // argument data stream
    cbuf_t     reply;  // reply data stream
    u_int32_t  cmd;
    u_int32_t  cmd_ref;

    // input data
    cbuf_init(&arg, (unsigned char*) buf, len, 0, 0, 0);
    CBUF_DBG(&arg, "output: async_arg");
    cbuf_init(&reply, 0, 0, 0, CBUF_FLAG_PUT_TRM, 0);

    cbuf_put_begin(&reply);  // put respose header    

    cbuf_put_tuple_begin(&reply, 3);
    cbuf_put_atom(&reply, "eapi_reply");

    // Fetch command & command reference
    if (cbuf_get_uint32(&arg, &cmd) && cbuf_get_uint32(&arg, &cmd_ref)) {
	EAPI_DRV_DBG("Async Cmd: %u ref=%u", cmd, cmd_ref);
	cbuf_put_uint32(&reply, cmd_ref);
	eapi_dispatch(ctx, cmd, &arg, &reply);  // CHECK return
    }
    else {
	cbuf_put_uint32(&reply, 0);
	eapi_put_error(&reply, "badarg");
    }
    cbuf_put_tuple_end(&reply, 3);
    cbuf_put_end(&reply);  // put response footer

    // send reply to caller {eapi_reply, CmdRef, ReplyData}
    cbuf_trim(&reply);
    
    A_DBG("eapi_drv_output: cmd=%u, cmd_ref=%u", cmd, cmd_ref);
    CBUF_DBG(&reply, "async_reply");

    driver_send_term(ctx->port, driver_caller(ctx->port), 
		     (ErlDrvTermData*) reply.v[0].base,
		     (reply.v[0].len / sizeof(ErlDrvTermData)));
    cbuf_final(&reply);
}



static void eapi_drv_outputv(ErlDrvData d, ErlIOVec* ev)
{
    eapi_ctx_t*  ctx = (eapi_ctx_t*) d;
    cbuf_t       arg;    // argument data stream
    cbuf_t       reply;  // reply data stream
    u_int32_t    cmd;
    u_int32_t    cmd_ref;

    cbuf_initv(&arg, ev, 0);
    CBUF_DBG(&arg, "outputv: async_arg");
    cbuf_init(&reply, 0, 0, 0, CBUF_FLAG_PUT_TRM, 0);

    cbuf_put_begin(&reply);  // put respose header

    cbuf_put_tuple_begin(&reply, 3);
    cbuf_put_atom(&reply, "eapi_reply");

    // Fetch command & command reference
    if (cbuf_get_uint32(&arg, &cmd) && cbuf_get_uint32(&arg, &cmd_ref)) {
	EAPI_DRV_DBG("Async Cmd: %02X ref=%u", cmd, cmd_ref);
	cbuf_put_uint32(&reply, cmd_ref);
	eapi_dispatch(ctx, cmd, &arg, &reply); // CHECK return
    }
    else {
	cbuf_put_uint32(&reply, 0);
	eapi_put_error(&reply, "badarg");
    }
    cbuf_put_tuple_end(&reply, 3);
    cbuf_put_end(&reply);  // put response footer

    // send reply to caller {eapi_reply, CmdRef, ReplyData}
    cbuf_trim(&reply);

    A_DBG("eapi_drv_outputv: cmd=%02X, cmd_ref=%u", cmd, cmd_ref);
    CBUF_DBG(&reply, "async_reply");

    driver_send_term(ctx->port, driver_caller(ctx->port), 
		     (ErlDrvTermData*) reply.v[0].base,
		     (reply.v[0].len / sizeof(ErlDrvTermData)));

    cbuf_final(&reply);
}

static void eapi_drv_ready_input(ErlDrvData d, ErlDrvEvent e)
{
    (void) d;
    (void) e;
}

static void eapi_drv_ready_output(ErlDrvData d, ErlDrvEvent e)
{
   (void) d;
   (void) e;
}


static void eapi_drv_timeout(ErlDrvData d)
{
    (void) d;
}

