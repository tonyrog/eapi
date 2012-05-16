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
// cbuf_cti: control buffer using control tag interface
//
#ifndef __CBUF_PUT_CTI_H__
#define __CBUF_PUT_CTI_H__

#include "cbuf_core.h"

#define OK             1   // 'ok'
#define ERROR          2   // 'error'
#define EVENT          3   // 'event'
#define INT8           4   // int8_t
#define UINT8          5   // uint8_t 
#define INT16          6   // int16_t
#define UINT16         7   // uint16_t
#define INT32          8   // int32_t
#define UINT32         9   // uint32_t
#define INT64          10  // int64_t
#define UINT64         11  // uint64_t
#define BOOLEAN        12  // uint8_t
#define FLOAT32        13  // float
#define FLOAT64        14  // double
#define STRING1        15  // len byte followed by UTF-8 chars 
#define STRING4        16  // 4-byte len followed by UTF-8 string 
#define ATOM           17  // len bytes followed by ASCII chars
#define BINARY         18  // binary 4-byte len followed by Octets
#define LIST           19  // list begin
#define LIST_END       20  // list end 
#define TUPLE          21  // tuple begin
#define TUPLE_END      22  // tuple end 
#define ENUM           23  // Encoded as INT32
#define BITFIELD       24  // Encoded as UINT64
#define HANDLE         25  // Encoded pointer 32/64 bit

#ifdef DEBUG_PUT_CTI
#define PUT_DBG(fmt, ...) fprintf(stderr, "put_cti: " fmt "\r\n", __VA_ARGS__)
#else
#define PUT_DBG(fmt, ...) 
#endif



// copy tag and data in ptr,len to cbuf, fix fill vector version
static inline int cbuf_twrite(cbuf_t* cp, uint8_t tag, void* ptr, size_t len)
{
    uint8_t* p;

    if (!(p = cbuf_seg_alloc(cp, 1+len)))
	return 0;
    p[0] = tag;
    switch(len) {
    case 4: p[4] = ((uint8_t*)ptr)[3];
    case 3: p[3] = ((uint8_t*)ptr)[2];
    case 2: p[2] = ((uint8_t*)ptr)[1];
    case 1: p[1] = ((uint8_t*)ptr)[0];
    case 0: break;
    default: memcpy(p+1, ptr, len); break;
    }
    return 1;
}

static inline size_t cbuf_sizeof(uint8_t tag)
{
    switch (tag) {
    case BOOLEAN:   return sizeof(uint8_t);
    case UINT8:     return sizeof(uint8_t);
    case UINT16:    return sizeof(uint16_t);
    case UINT32:    return sizeof(uint32_t);
    case UINT64:    return sizeof(uint64_t);
    case STRING1:   return 0;  // variable
    case LIST:      return 0;  // variable
    case LIST_END:  return 0;  // variable
    case TUPLE:     return 0;  // variable
    case TUPLE_END: return 0;  // variable
    case ATOM:      return 0;  // variable
    case BINARY:    return 0;  // variable
    case INT8:      return sizeof(int8_t);
    case INT16:     return sizeof(int16_t);
    case INT32:     return sizeof(int32_t);
    case INT64:     return sizeof(int64_t);
    case FLOAT32:   return sizeof(float);
    case FLOAT64:   return sizeof(double);
    case STRING4:   return 0;
    case ENUM:      return sizeof(int32_t);
    case BITFIELD:  return sizeof(int64_t);
    case HANDLE:    return sizeof(intptr_t);
    default: return 0;
    }
}

static inline int cbuf_cti_put_boolean(cbuf_t* cp, uint8_t value)
{
    PUT_DBG("BOOLEAN %d", value);
    return cbuf_twrite(cp, BOOLEAN, &value, sizeof(value));
}

static inline int cbuf_cti_put_int8(cbuf_t* cp, int8_t value)
{
    PUT_DBG("INT8 %d", value);
    return cbuf_twrite(cp, INT8, &value, sizeof(value));
}

static inline int cbuf_cti_put_int16(cbuf_t* cp, int16_t value)
{
    PUT_DBG("INT16 %d", value);
    return cbuf_twrite(cp, INT16, &value, sizeof(value));
}

static inline int cbuf_cti_put_int32(cbuf_t* cp, int32_t value)
{
    PUT_DBG("INT32 %d", value);
    return cbuf_twrite(cp, INT32, &value, sizeof(value));
}

static inline int cbuf_cti_put_int64(cbuf_t* cp, int64_t value)
{
    PUT_DBG("INT64 %lld", value);
    return cbuf_twrite(cp, INT64, &value, sizeof(value));
}

static inline int cbuf_cti_put_float32(cbuf_t* cp, float value)
{
    PUT_DBG("FLOAT32 %f", value);
    return cbuf_twrite(cp, FLOAT32, &value, sizeof(value));
}

static inline int cbuf_cti_put_float64(cbuf_t* cp, double value)
{
    PUT_DBG("FLOAT64 %f", value);
    return cbuf_twrite(cp, FLOAT64, &value, sizeof(value));
}

static inline int cbuf_cti_put_uint8(cbuf_t* cp, uint8_t value)
{
    PUT_DBG("UINT8 %u", value);
    return cbuf_twrite(cp, UINT8, &value, sizeof(value));
}

static inline int cbuf_cti_put_uint16(cbuf_t* cp, uint16_t value)
{
    PUT_DBG("UINT16 %u", value);
    return cbuf_twrite(cp, UINT16, &value, sizeof(value));
}

static inline int cbuf_cti_put_uint32(cbuf_t* cp, uint32_t value)
{
    PUT_DBG("UINT32 %u", value);
    return cbuf_twrite(cp, UINT32, &value, sizeof(value));
}

static inline int cbuf_cti_put_uint64(cbuf_t* cp, uint64_t value)
{
    PUT_DBG("UINT64 %llu", value);
    return cbuf_twrite(cp, UINT64, &value, sizeof(value));
}

static inline int cbuf_cti_put_tuple_begin(cbuf_t* cp, size_t n)
{
    (void) n;
    PUT_DBG("TUPLE-BEGIN %lu", n);
    return cbuf_twrite(cp, TUPLE, 0, 0);    
}

static inline int cbuf_cti_put_tuple_end(cbuf_t* cp, size_t n)
{
    (void) n;
    PUT_DBG("TUPLE-END %lu", n);
    return cbuf_twrite(cp, TUPLE_END, 0, 0);    
}

static inline int cbuf_cti_put_list_begin(cbuf_t* cp, size_t n)
{
    (void) n;
    PUT_DBG("LIST-BEGIN %lu", n);
    return cbuf_twrite(cp, LIST, 0, 0);        
}

static inline int cbuf_cti_put_list_end(cbuf_t* cp, size_t n)
{
    (void) n;
    PUT_DBG("LIST-END %lu", n);
    return cbuf_twrite(cp, LIST_END, 0, 0);
}

static inline int cbuf_cti_put_begin(cbuf_t* cp)
{
    (void) cp;
    PUT_DBG("BEGIN%s", "");
    return 1;
}

static inline int cbuf_cti_put_end(cbuf_t* cp)
{
    (void) cp;
    PUT_DBG("END%s", "");
    return 1;
}

static inline int cbuf_cti_put_tag_ok(cbuf_t* cp)
{
    PUT_DBG("OK%s", "");
    return cbuf_twrite(cp, OK, 0, 0);
}

static inline int cbuf_cti_put_tag_error(cbuf_t* cp)
{
    PUT_DBG("ERROR%s", "");
    return cbuf_twrite(cp, ERROR, 0, 0);
}

static inline int cbuf_cti_put_tag_event(cbuf_t* cp)
{
    PUT_DBG("EVENT%s", "");
    return cbuf_twrite(cp, EVENT, 0, 0);
}

static inline int cbuf_cti_put_atom(cbuf_t* cp, const char* atom)
{
    uint8_t* ptr;
    uint32_t n = strlen(atom);

    PUT_DBG("ATOM [%s]", atom);
    if (n > 0xff) n = 0xff; // truncate error?
    if (!(ptr = cbuf_seg_alloc(cp, n+2)))
	return 0;
    ptr[0] = ATOM;
    ptr[1] = n;
    memcpy(&ptr[2], atom, n);
    return 1;
}

static inline int cbuf_cti_put_string(cbuf_t* cp, const char* string, int n)
{
    uint8_t* ptr;

    PUT_DBG("STRING len=%d [%*s]", n, n, string);
    if ((string == NULL) || (n == 0)) {
	if (!(ptr = cbuf_seg_alloc(cp, 2)))
	    return 0;
	ptr[0] = STRING1;
	ptr[1] = 0;
    }
    else {
	if (n <= 0xff) {
	    if (!(ptr = cbuf_seg_alloc(cp, n+2)))
		return 0;
	    ptr[0] = STRING1;
	    ptr[1] = n;
	    memcpy(&ptr[2], string, n);
	}
	else {
	    uint32_t len = n;
	    if (!(ptr = cbuf_seg_alloc(cp, n+5)))
		return 0;
	    ptr[0] = STRING4;
	    memcpy(&ptr[1], &len, sizeof(len));
	    memcpy(&ptr[5], string, n);
	}
    }
    return 1;
}

static inline int cbuf_cti_put_bin(cbuf_t* cp, ErlDrvBinary* bin, size_t len, size_t offset)
{
    uint8_t* ptr;

    PUT_DBG("BIN len=%lu, offset=%lu", len, offset);
    if (!(ptr = cbuf_seg_alloc(cp, len+5)))
	return 0;
    ptr[0] = BINARY;
    memcpy(ptr+1, &len, sizeof(len));
    memcpy(ptr+5, bin->orig_bytes+offset, len);
    return 1;
}


static inline int cbuf_cti_put_binary(cbuf_t* cp, const uint8_t* buf, uint32_t len)
{
    uint8_t* ptr;

    PUT_DBG("BINARY len=%u, buf=%p", len, buf);
    if (!(ptr = cbuf_seg_alloc(cp, len+5)))
	return 0;
    ptr[0] = BINARY;
    memcpy(ptr+1, &len, sizeof(len));
    memcpy(ptr+5, buf, len);
    return 1;
}

#undef PUT_DBG

#endif
