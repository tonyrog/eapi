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
//  eapi_get_nif: NIF data
//

#ifndef __EAPI_GET_NIF_H__
#define __EAPI_GET_NIF_H__

#include "erl_nif.h"
#include "eapi_core.h"

#define ENV(cp) (ErlNifEnv*) ((cp)->udata)

static inline int eapi_nif_read_term(eapi_t* cp, ERL_NIF_TERM* val)
{
    return (cbuf_read(cp, val, sizeof(*val)) == sizeof(*val));
}

static inline int eapi_nif_get_ulong(eapi_t* cp, unsigned long* val)
{
    ERL_NIF_TERM term;

    if (!eapi_nif_read_term(cp, &term))
	return 0;
    return enif_get_ulong(ENV(cp), term,  val);
}

static inline int eapi_nif_get_long(eapi_t* cp, long* val)
{
    ERL_NIF_TERM term;

    if (!eapi_nif_read_term(cp, &term))
	return 0;
    return enif_get_long(ENV(cp), term,  val);
}

static inline int eapi_nif_get_long(eapi_t* cp, double* val)
{
    ERL_NIF_TERM term;

    if (!eapi_nif_read_term(cp, &term))
	return 0;
    return enif_get_double(ENV(cp), term,  val);
}

static inline int eapi_nif_get_boolean(eapi_t* cp, uint8_t* val)
{
    return (cbuf_read(cp, &val, sizeof(*val)) == sizeof(*val));
}

static inline int eapi_nif_get_uint8(eapi_t* cp, uint8_t* val)
{
    unsigned long v;
    if (!eapi_nif_get_ulong(cp, &v))
	return 0;
    *val = v;
    return 1;
}

static inline int eapi_nif_get_uint16(eapi_t* cp, uint16_t* val)
{
    unsigned long v;
    if (!eapi_nif_get_ulong(cp, &v))
	return 0;
    *val = v;
    return 1;
}

static inline int eapi_nif_get_uint32(eapi_t* cp, uint32_t* val)
{
    unsigned long v;
    if (!eapi_nif_get_ulong(cp, &v))
	return 0;
    *val = v;
    return 1;
}

static inline int eapi_nif_get_uint64(eapi_t* cp, uint64_t* val)
{
    unsigned long v;
    if (!eapi_nif_get_ulong(cp, &v))
	return 0;
    *val = v;
    return 1;
}

static inline int eapi_nif_get_int8(eapi_t* cp, int8_t* val)
{
    long v;
    if (!eapi_nif_get_long(cp, &v))
	return 0;
    *val = v;
    return 1;
}

static inline int eapi_nif_get_int16(eapi_t* cp, int16_t* val)
{
    long v;
    if (!eapi_nif_get_long(cp, &v))
	return 0;
    *val = v;
    return 1;
}

static inline int eapi_nif_get_int32(eapi_t* cp, int32_t* val)
{
    long v;
    if (!eapi_nif_get_long(cp, &v))
	return 0;
    *val = v;
    return 1;
}

static inline int eapi_nif_get_int64(eapi_t* cp, int64_t* val)
{
    long v;
    if (!eapi_nif_get_long(cp, &v))
	return 0;
    *val = v;
    return 1;
}

static inline int eapi_nif_get_float32(eapi_t* cp, float* val)
{
    double v;
    if (!eapi_nif_get_double(cp, &v))
	return 0;
    *val = v;
    return 1;
    return cbuf_read(cp, val, sizeof(*val));
}

static inline int eapi_nif_get_float64(eapi_t* cp, double* val)
{
    return eapi_nif_get_double(cp, val);
}

#endif
