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
// CBUF - External term format
//
#ifndef __CBUF_PUT_ETF_H__
#define __CBUF_PUT_ETF_H__

#include "cbuf_core.h"

#define VERSION_MAGIC 131
#define SMALL_INTEGER_EXT 97  // 'a'
#define INTEGER_EXT       98  // 'b'
#define FLOAT_EXT         99  // 'c'
#define ATOM_EXT          100 // 'd'
#define SMALL_ATOM_EXT    115 // 's'
#define REFERENCE_EXT     101 // 'e'
#define NEW_REFERENCE_EXT 114 // 'r'
#define PORT_EXT          102 // 'f'
#define NEW_FLOAT_EXT     70  // 'F'
#define PID_EXT           103 // 'g'
#define SMALL_TUPLE_EXT   104 // 'h'
#define LARGE_TUPLE_EXT   105 // 'i'
#define NIL_EXT           106 // 'j'
#define STRING_EXT        107 // 'k'
#define LIST_EXT          108 // 'l'
#define BINARY_EXT        109 // 'm'
#define BIT_BINARY_EXT    77 // 'M'
#define SMALL_BIG_EXT     110 // 'n'
#define LARGE_BIG_EXT     111 // 'o'
#define NEW_FUN_EXT       112 // 'p'
#define EXPORT_EXT        113 // 'q'
#define FUN_EXT           117 // 'u'
#define DIST_HEADER       68  // 'D'
#define ATOM_CACHE_REF    82 // 'R'
#define COMPRESSED        80 // 'P'

#ifdef DEBUG_PUT_ETF
#define PUT_DBG(fmt, ...) fprintf(stderr, "put_etf: " fmt "\r\n", __VA_ARGS__)
#else
#define PUT_DBG(fmt, ...) 
#endif

static inline int etf_put_uint8(cbuf_t* cp, uint8_t value)
{
    uint8_t* p;
    if (!(p = cbuf_seg_alloc(cp, 2)))
	return 0;    
    p[0] = SMALL_INTEGER_EXT;    
    p[1] = value;
    return 1;
}

static inline int etf_put_int32(cbuf_t* cp, int32_t value)
{
    uint8_t* p;
    if (!(p = cbuf_seg_alloc(cp, 5)))
	return 0;
    p[0] = INTEGER_EXT;
    memcpy_n2b(&p[1], &value, 4);
    return 1;
}

static inline int etf_put_u64(cbuf_t* cp,uint8_t sign,uint64_t value)
{
    uint8_t* p;

    if (!(p = cbuf_seg_alloc(cp, 13)))
	return 0;
    p[0] = SMALL_BIG_EXT;
    p[1] = 8;
    p[2] = sign;
    memcpy_n2b(&p[3], &value, 8);
    return 1;
}

static inline int etf_put_int64(cbuf_t* cp, int64_t value)
{
    if (value < 0)
	return etf_put_u64(cp, 1, (uint64_t) -value);
    else
	return etf_put_u64(cp, 0, (uint64_t) value);
}

static inline int etf_put_float(cbuf_t* cp, double value)
{
    uint8_t* p;
    if (!(p = cbuf_seg_alloc(cp, 9)))
	return 0;
    p[0] = NEW_FLOAT_EXT;
    memcpy_n2b(&p[1], &value, 8);
    return 1;
}


static inline int etf_put_atom(cbuf_t* cp, const char* atom, size_t len)
{
    uint8_t* p;
    if (len > 255) len = 255;
    if (!(p = cbuf_seg_alloc(cp, len+2)))
	return 0;    
    p[0] = SMALL_ATOM_EXT;
    p[1] = len;
    memcpy(&p[2], atom, len);
    return 1;
}

static inline int cbuf_etf_put_atom(cbuf_t* cp, const char* atom)
{
    size_t n = strlen(atom);
    PUT_DBG("ATOM [%s]", atom);
    return etf_put_atom(cp, atom, n);
}

static inline int cbuf_etf_put_boolean(cbuf_t* cp, uint8_t value)
{
    PUT_DBG("BOOLEAN %d", value);
    if (value) 
	return etf_put_atom(cp, "true", 4);
    else
	return etf_put_atom(cp, "false", 5);
}

static inline int cbuf_etf_put_int8(cbuf_t* cp, int8_t value)
{
    PUT_DBG("INT8 %d", value);
    if (value >= 0)
	return etf_put_uint8(cp, (uint8_t) value);
    else
	return etf_put_int32(cp, (int32_t) value);
}

static inline int cbuf_etf_put_int16(cbuf_t* cp, int16_t value)
{
    PUT_DBG("INT16 %d", value);
    return etf_put_int32(cp, (int32_t) value);
}

static inline int cbuf_etf_put_int32(cbuf_t* cp, int32_t value)
{
    PUT_DBG("INT32 %d", value);
    return etf_put_int32(cp, value);
}

static inline int cbuf_etf_put_int64(cbuf_t* cp, int64_t value)
{
    PUT_DBG("INT64 %lld", value);
    return etf_put_int64(cp, value);
}

static inline int cbuf_etf_put_float32(cbuf_t* cp, float value)
{
    PUT_DBG("FLOAT32 %f", value);
    return etf_put_float(cp, (double) value);
}

static inline int cbuf_etf_put_float64(cbuf_t* cp, double value)
{
    PUT_DBG("FLOAT64 %f", value);
    return etf_put_float(cp, value);
}


static inline int cbuf_etf_put_uint8(cbuf_t* cp, uint8_t value)
{
    PUT_DBG("UINT8 %u", value);
    return etf_put_uint8(cp, value);
}

static inline int cbuf_etf_put_uint16(cbuf_t* cp, uint16_t value)
{
    PUT_DBG("UINT16 %u", value);
    return etf_put_int32(cp, (int32_t) value);
}

static inline int cbuf_etf_put_uint32(cbuf_t* cp, uint32_t value)
{
    PUT_DBG("UINT32 %u", value);
    if (value > 0x7fffffff)
	return etf_put_u64(cp, 0, (uint64_t) value);
    else
	return etf_put_int32(cp, (int32_t) value);
}

static inline int cbuf_etf_put_uint64(cbuf_t* cp, uint64_t value)
{
    PUT_DBG("UINT64 %llu", value);
    return etf_put_u64(cp, 0, value);
}

static inline int cbuf_etf_put_begin(cbuf_t* cp)
{
    uint8_t* p;
    PUT_DBG("BEGIN%s", ""); 
    if (!(p = cbuf_seg_alloc(cp, 1)))
	return 0;
    p[0] = VERSION_MAGIC;
    return 1;
}

static inline int cbuf_etf_put_end(cbuf_t* cp)
{
    (void) cp;
    PUT_DBG("END%s", "");
    return 1;
}


static inline int cbuf_etf_put_tuple_begin(cbuf_t* cp, size_t n)
{
    uint8_t* p;

    PUT_DBG("TUPLE-BEGIN %lu", n);

    if (n > 0xFF) {
	if (!(p = cbuf_seg_alloc(cp, 5)))
	    return 0;
	p[0] = LARGE_TUPLE_EXT;
	memcpy_n2b(&p[1], &n, 4);
    }
    else {
	if (!(p = cbuf_seg_alloc(cp, 2)))
	    return 0;
	p[0] = SMALL_TUPLE_EXT;
	p[1] = n;
    }
    return 1;
}

static inline int cbuf_etf_put_tuple_end(cbuf_t* cp, size_t n)
{
    (void) cp;
    (void) n;
    PUT_DBG("TUPLE-END %lu", n);
    return 1;
}

static inline int cbuf_etf_put_list_begin(cbuf_t* cp, size_t n)
{
    uint8_t* p;
    PUT_DBG("LIST-BEGIN %lu", n);
    if (!(p = cbuf_seg_alloc(cp, 5)))
	return 0;
    p[0] = LIST_EXT;
    memcpy_n2b(&p[1], &n, 4);
    return 1;
}

// proper list end!
static inline int cbuf_etf_put_list_end(cbuf_t* cp, size_t n)
{
    uint8_t* p;
    (void) n;
    PUT_DBG("LIST-END %lu", n);
    if (!(p = cbuf_seg_alloc(cp, 1)))
	return 0;
    p[0] = NIL_EXT;
    return 1;
}

static inline int cbuf_etf_put_tag_ok(cbuf_t* cp)
{
    PUT_DBG("OK%s", "");
    return etf_put_atom(cp, "ok", 2);
}

static inline int cbuf_etf_put_tag_error(cbuf_t* cp)
{
    PUT_DBG("ERROR%s", "");
    return etf_put_atom(cp, "error", 5);
}

static inline int cbuf_etf_put_tag_event(cbuf_t* cp)
{
    PUT_DBG("EVENT%s", "");
    return etf_put_atom(cp, "event", 5);
}

static inline int cbuf_etf_put_string(cbuf_t* cp, const char* string, int n)
{
    uint8_t* p;
    PUT_DBG("STRING len=%d [%*s]", n, n, string);
    if ((string == NULL) || (n == 0)) {
	if (!(p = cbuf_seg_alloc(cp, 1)))
	    return 0;
	p[0] = NIL_EXT;
    }
    else {
	if (n > 0xFFFF) n = 0xFFFF; // warn?
	if (!(p = cbuf_seg_alloc(cp, n+3)))
	    return 0;
	p[0] = STRING_EXT;
	p[1] = n>>8;
	p[2] = n;
	memcpy(&p[3], string, n);
    }
    return 1;
}

static inline int cbuf_etf_put_bin(cbuf_t* cp, ErlDrvBinary* bin, size_t len, size_t offset)
{
    (void)cp;
    (void)bin;
    (void)len;
    (void)offset;
    PUT_DBG("BIN (FIXME) len=%lu, offset=%lu", len, offset);
    return 0;  // FIXME
}

// FIXME - if vector interface add as binary part
static inline int cbuf_etf_put_binary(cbuf_t* cp, const uint8_t* buf, 
				      uint32_t len)
{
    uint8_t* p;

    PUT_DBG("BINARY len=%u, buf=%p", len, buf);
    if (!(p = cbuf_seg_alloc(cp, len+5)))
	return 0;
    p[0] = BINARY_EXT;
    memcpy_n2b(&p[1], &len, 4);
    memcpy(&p[5], buf, len);
    return 1;
}

#endif
