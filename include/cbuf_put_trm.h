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
// cbuf_put_trm:  cbuf driver term interface
//

#ifndef __CBUF_PUT_TRM_H__
#define __CBUF_PUT_TRM_H__

#include "cbuf_core.h"

#ifdef DEBUG_PUT_TRM
#define PUT_DBG(fmt, ...) fprintf(stderr, "put_trm: " fmt "\r\n", __VA_ARGS__)
#else
#define PUT_DBG(fmt, ...) 
#endif

static inline int trm_put_1(cbuf_t* cp, ErlDrvTermData tag)
{
    ErlDrvTermData* p;

    if (!(p = (ErlDrvTermData*) cbuf_seg_alloc(cp, sizeof(ErlDrvTermData))))
	return 0;
    p[0] = tag;
    return 1;
}


static inline int trm_put_2(cbuf_t* cp, ErlDrvTermData tag, ErlDrvTermData value)
{
    ErlDrvTermData* p;

    if (!(p = (ErlDrvTermData*) cbuf_seg_alloc(cp, sizeof(ErlDrvTermData)*2)))
	return 0;    
    p[0] = tag;
    p[1] = value;
    return 1;
}

static inline int trm_put_3(cbuf_t* cp, ErlDrvTermData tag, 
			    ErlDrvTermData value1, ErlDrvTermData value2)
{
    ErlDrvTermData* p;

    if (!(p = (ErlDrvTermData*) cbuf_seg_alloc(cp, sizeof(ErlDrvTermData)*3)))
	return 0;    
    p[0] = tag;
    p[1] = value1;
    p[2] = value2;
    return 1;
}

static inline int trm_put_4(cbuf_t* cp, ErlDrvTermData tag, 
			    ErlDrvTermData value1, ErlDrvTermData value2,
			    ErlDrvTermData value3)
{
    ErlDrvTermData* p;

    if (!(p = (ErlDrvTermData*) cbuf_seg_alloc(cp, sizeof(ErlDrvTermData)*4)))
	return 0;    
    p[0] = tag;
    p[1] = value1;
    p[2] = value2;
    p[3] = value3;
    return 1;
}


static inline int trm_put_uint(cbuf_t* cp, ErlDrvUInt value)
{
    return trm_put_2(cp, ERL_DRV_UINT, (ErlDrvTermData) value);
}

static inline int trm_put_sint(cbuf_t* cp, ErlDrvSInt value)
{
    return trm_put_2(cp, ERL_DRV_INT, (ErlDrvTermData) value);
}

static inline int trm_put_atom(cbuf_t* cp, const char* value)
{
    return trm_put_2(cp, ERL_DRV_ATOM,
		     (ErlDrvTermData) driver_mk_atom((char*)value));
}


static inline int cbuf_trm_put_begin(cbuf_t* cp)
{
    (void) cp;
    PUT_DBG("BEGIN%s", ""); 
    return 1;
}

static inline int cbuf_trm_put_end(cbuf_t* cp)
{
    (void) cp;
    PUT_DBG("END%s", "");
    return 1;
}

static inline int cbuf_trm_put_int8(cbuf_t* cp, int8_t value)
{
    PUT_DBG("INT8 %d", value);
    return trm_put_sint(cp, (ErlDrvSInt) value);
}

static inline int cbuf_trm_put_int16(cbuf_t* cp, int16_t value)
{
    PUT_DBG("INT16 %d", value);
    return trm_put_sint(cp, (ErlDrvSInt) value);
}

static inline int cbuf_trm_put_int32(cbuf_t* cp, int32_t value)
{
    PUT_DBG("INT32 %d", value);
    return trm_put_sint(cp, (ErlDrvSInt) value);
}

static inline int cbuf_trm_put_int64(cbuf_t* cp, int64_t value)
{
    ErlDrvSInt64* ptr = (ErlDrvSInt64*)cbuf_heap_alloc(cp,sizeof(ErlDrvSInt64));
    PUT_DBG("INT64 %lld", value);
    *ptr = value;
    return trm_put_2(cp, ERL_DRV_INT64, (ErlDrvTermData) ptr);
}

static inline int cbuf_trm_put_float32(cbuf_t* cp, float value)
{
    double* ptr = (double*) cbuf_heap_alloc(cp, sizeof(double));
    PUT_DBG("FLOAT32 %f", value);
    *ptr = (double) value;
    return trm_put_2(cp, ERL_DRV_FLOAT, (ErlDrvTermData) ptr);
}

static inline int cbuf_trm_put_float64(cbuf_t* cp, double value)
{
    double* ptr = (double*) cbuf_heap_alloc(cp, sizeof(double));
    PUT_DBG("FLOAT64 %f", value);
    *ptr = value;
    return trm_put_2(cp, ERL_DRV_FLOAT, (ErlDrvTermData) ptr);
}

static inline int cbuf_trm_put_uint8(cbuf_t* cp, uint8_t value)
{
    PUT_DBG("UINT8 %u", value);
    return trm_put_uint(cp, (ErlDrvUInt) value);
}

static inline int cbuf_trm_put_uint16(cbuf_t* cp, uint16_t value)
{
    PUT_DBG("UINT16 %u", value);
    return trm_put_uint(cp, (ErlDrvUInt) value);
}

static inline int cbuf_trm_put_uint32(cbuf_t* cp, uint32_t value)
{
    PUT_DBG("UINT32 %u", value);
    return trm_put_uint(cp, (ErlDrvUInt) value);
}

static inline int cbuf_trm_put_uint64(cbuf_t* cp, uint64_t value)
{
    ErlDrvUInt64* ptr = (ErlDrvUInt64*)cbuf_heap_alloc(cp,sizeof(ErlDrvUInt64));
    PUT_DBG("UINT64 %llu", value);
    *ptr = value;
    return trm_put_2(cp, ERL_DRV_UINT64, (ErlDrvTermData) ptr);
}

static inline int cbuf_trm_put_atom(cbuf_t* cp, const char* value)
{
    PUT_DBG("ATOM [%s]", value);
    return trm_put_atom(cp, value);
}

static inline int cbuf_trm_put_boolean(cbuf_t* cp, uint8_t value)
{
    PUT_DBG("BOOLEAN %d", value);
    if (value)
	return trm_put_atom(cp, "true");
    else
	return trm_put_atom(cp, "false");
}

static inline int cbuf_trm_put_tuple_begin(cbuf_t* cp, size_t n)
{
    (void) cp;
    (void) n;
    PUT_DBG("TUPLE-BEGIN %lu", n);
    return 1;
}

static inline int cbuf_trm_put_tuple_end(cbuf_t* cp, size_t n) 
{
    PUT_DBG("TUPLE-END %lu", n);
    return trm_put_2(cp, ERL_DRV_TUPLE, (ErlDrvTermData) n);
}

static inline int cbuf_trm_put_list_begin(cbuf_t* cp, size_t n)
{
    (void) cp;
    (void) n;
    PUT_DBG("LIST-BEGIN %lu", n);
    return 1;
}

static inline int cbuf_trm_put_list_end(cbuf_t* cp, size_t n)
{
    PUT_DBG("LIST-END %lu", n);
    trm_put_1(cp, ERL_DRV_NIL);
    return trm_put_2(cp, ERL_DRV_LIST, (ErlDrvTermData) (n+1));
}

static inline int cbuf_trm_put_tag_ok(cbuf_t* cp)
{
    PUT_DBG("OK%s", "");
    return cbuf_trm_put_atom(cp, "ok");
}

static inline int cbuf_trm_put_tag_error(cbuf_t* cp)
{
    PUT_DBG("ERROR%s", "");
    return cbuf_trm_put_atom(cp, "error");
}

static inline int cbuf_trm_put_tag_event(cbuf_t* cp)
{
    PUT_DBG("EVENT%s", "");
    return cbuf_trm_put_atom(cp, "event");
}

static inline int cbuf_trm_put_string(cbuf_t* cp, const char* string, int n)
{
    PUT_DBG("STRING len=%d [%*s]", n, n, string);
    return trm_put_3(cp, ERL_DRV_STRING, (ErlDrvTermData) string,
		     (ErlDrvTermData) n);
}

static inline int cbuf_trm_put_bin(cbuf_t* cp, ErlDrvBinary* bin, size_t len, size_t offset)
{
    PUT_DBG("BIN len=%lu, offset=%lu", len, offset);
    return trm_put_4(cp, ERL_DRV_BINARY, (ErlDrvTermData) bin, 
		     (ErlDrvTermData) len, (ErlDrvTermData) offset);
}

static inline int cbuf_trm_put_binary(cbuf_t* cp, const uint8_t* buf, uint32_t len)
{
    PUT_DBG("BINARY len=%u, buf=%p", len, buf);
    return trm_put_3(cp, ERL_DRV_BUF2BINARY, (ErlDrvTermData) buf,
		     (ErlDrvTermData) len);
}

#undef PUT_DBG

#endif
