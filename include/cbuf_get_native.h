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
//  cbuf_data: untagged native endian data 
//

#ifndef __CBUF_GET_NATIVE_H__
#define __CBUF_GET_NATIVE_H__

#include "cbuf_core.h"

static inline int cbuf_get_boolean(cbuf_t* cp, uint8_t* val)
{
    return (cbuf_read(cp, val, sizeof(*val)) == sizeof(*val));
}

static inline int cbuf_get_uint8(cbuf_t* cp, uint8_t* val)
{
    return (cbuf_read(cp, val, sizeof(*val)) == sizeof(*val));
}

static inline int cbuf_get_uint16(cbuf_t* cp, uint16_t* val)
{
    return (cbuf_read(cp, val, sizeof(*val)) == sizeof(*val));
}

static inline int cbuf_get_uint32(cbuf_t* cp, uint32_t* val)
{
    return (cbuf_read(cp, val, sizeof(*val)) == sizeof(*val));
}

static inline int cbuf_get_uint64(cbuf_t* cp, uint64_t* val)
{
    return (cbuf_read(cp, val, sizeof(*val)) == sizeof(*val));
}

static inline int cbuf_get_int8(cbuf_t* cp, int8_t* val)
{
    return (cbuf_read(cp, val, sizeof(*val)) == sizeof(*val));
}

static inline int cbuf_get_int16(cbuf_t* cp, int16_t* val)
{
    return (cbuf_read(cp, val, sizeof(*val)) == sizeof(*val));
}

static inline int cbuf_get_int32(cbuf_t* cp, int32_t* val)
{
    return (cbuf_read(cp, val, sizeof(*val)) == sizeof(*val));
}

static inline int cbuf_get_int64(cbuf_t* cp, int64_t* val)
{
    return (cbuf_read(cp, val, sizeof(*val)) == sizeof(*val));
}

static inline int cbuf_get_float32(cbuf_t* cp, float* val)
{
    return cbuf_read(cp, val, sizeof(*val));
}

static inline int cbuf_get_float64(cbuf_t* cp, double* val)
{
    return cbuf_read(cp, val, sizeof(*val));
}

#endif
