/**** BEGIN COPYRIGHT ********************************************************
 *
 * Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
 *
 * This software is licensed as described in the file COPYRIGHT, which
 * you should have received as part of this distribution. The terms
 * are also available at http://www.rogvall.se/docs/copyright.txt.
 *
 * You may opt to use, copy, modify, merge, publish, distribute and/or sell
 * copies of the Software, and permit persons to whom the Software is
 * furnished to do so, under the terms of the COPYRIGHT file.
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied.
 *
 **** END COPYRIGHT **********************************************************/
//
// Implementation of object_api
//

#include "eapi_drv.h"
#include "cbuf_put_trm.h"
#include "object_api.h"

static ErlDrvEntry object_drv_entry;

typedef struct 
{
    int x;
    char* string;
} magic_object_t;

DRIVER_INIT(object_drv)
{
    eapi_driver_init(&object_drv_entry,  0, 0);
    object_drv_entry.driver_name = "object_drv";
    // patch other members here if needed 
    return (ErlDrvEntry*) &object_drv_entry;
}


void magic_object_final(magic_object_t* obj, void* user_data)
{
    (void) user_data;
    fprintf(stderr, "magic_object_final\r\n");
    fprintf(stderr, "magic_object_final: x=%d, [%s]\r\n", obj->x, obj->string);
    free(obj->string);
}

void object_drv_impl_create_object(eapi_ctx_t* ctx,cbuf_t* c_out,int x)
{
    (void) ctx;
    magic_object_t* obj = (magic_object_t*) 
	driver_alloc_object(sizeof(magic_object_t), (void*) 0,
			    (void(*)(void*,void*))  magic_object_final);
    fprintf(stderr, "object_drv_imple_create_object: %p\n", obj);
    cbuf_put_tuple_begin(c_out, 2);
    if (obj) {
	ErlDrvBinary* bin = driver_object_binary(obj);

	obj->x = x;
	obj->string = strdup("Hello world");
	cbuf_put_tag_ok(c_out);
	cbuf_put_bin(c_out, bin,
		     driver_object_size(obj),
		     driver_object_offset(obj));
	// Can not free, sine put_bin does not reference binary!
	driver_binary_dec_refc(bin);
    }
    else {
	cbuf_put_tag_error(c_out);
	cbuf_put_atom(c_out, "enomem");
    }
    cbuf_put_tuple_end(c_out, 2);
}


void object_drv_impl_get_x(eapi_ctx_t* ctx,cbuf_t* c_out,void* ptr)
{
    (void) ctx;
    magic_object_t* obj = (magic_object_t*) ptr;
    fprintf(stderr, "object_drv_impl_get_x: %p\n", obj);
    cbuf_put_tuple_begin(c_out, 2);
    cbuf_put_tag_ok(c_out);
    cbuf_put_int32(c_out, obj->x);
    cbuf_put_tuple_end(c_out, 2);
}

void object_drv_impl_set_x(eapi_ctx_t* ctx,cbuf_t* c_out,void* ptr,int x)
{
    (void) ctx;
    magic_object_t* obj = (magic_object_t*) ptr;
    fprintf(stderr, "object_drv_impl_get_x: %p\n", obj);
    obj->x = x;
    cbuf_put_tag_ok(c_out);
}
