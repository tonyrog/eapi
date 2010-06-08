//
// CBUF - control buffer managment
//
#ifndef __CBUF_H__
#define __CBUF_H__

#include "cbuf_core.h"

#include "cbuf_get_native.h"

#ifdef CBUF_USE_PUT_CTI
#include "cbuf_put_cti.h"
#endif

#ifdef CBUF_USE_PUT_ETF
#include "cbuf_put_etf.h"
#endif

#ifdef CBUF_USE_PUT_TRM
#include "cbuf_put_trm.h"
#endif

#ifdef CBUF_USE_PUT_NIF
#include "cbuf_put_nif.h"
#endif

#ifdef CBUF_USE_PUT_ETF
#define CASE_ETF(Do) case CBUF_FLAG_PUT_ETF: Do
#else
#define CASE_ETF(Do)
#endif

#ifdef CBUF_USE_PUT_CTI
#define CASE_CTI(Do) case CBUF_FLAG_PUT_CTI: Do
#else
#define CASE_CTI(Do)
#endif

#ifdef CBUF_USE_PUT_TRM
#define CASE_TRM(Do) case CBUF_FLAG_PUT_TRM: Do
#else
#define CASE_TRM(Do)
#endif

#ifdef CBUF_USE_PUT_NIF
#define CASE_NIF(Do) case CBUF_FLAG_PUT_NIF: Do
#else
#define CASE_NIF(Do)
#endif


#define CBUF_PUT(what,cp) \
    switch((cp)->flags & CBUF_FLAG_PUT_MASK) {		\
	CASE_ETF({return cbuf_etf_put_##what((cp));})	\
	    CASE_CTI({return cbuf_cti_put_##what((cp)); })	\
	    CASE_TRM({return cbuf_trm_put_##what((cp));})	\
	    CASE_NIF({return cbuf_nif_put_##what((cp));})	\
    default: return 0;						\
    }

#define CBUF_PUT_VALUE(what,cp,arg...)			\
    switch((cp)->flags & CBUF_FLAG_PUT_MASK) {		\
	CASE_ETF({return cbuf_etf_put_##what((cp),arg);})	\
	    CASE_CTI({return cbuf_cti_put_##what((cp),arg); })	\
	    CASE_TRM({return cbuf_trm_put_##what((cp),arg);})		\
	    CASE_NIF({return cbuf_nif_put_##what((cp),arg);})		\
    default: return 0;							\
    }


static inline int cbuf_put_boolean(cbuf_t* cp, uint8_t value)
{
    CBUF_PUT_VALUE(boolean, cp, value);
}

static inline int cbuf_put_int8(cbuf_t* cp, int8_t value)
{
    CBUF_PUT_VALUE(int8, cp, value);
}

static inline int cbuf_put_int16(cbuf_t* cp, int16_t value)
{
    CBUF_PUT_VALUE(int16, cp, value);
}

static inline int cbuf_put_int32(cbuf_t* cp, int32_t value)
{
    CBUF_PUT_VALUE(int32, cp, value);
}

static inline int cbuf_put_int64(cbuf_t* cp, int64_t value)
{
    CBUF_PUT_VALUE(int64, cp, value);
}
static inline int cbuf_put_float32(cbuf_t* cp, float value)
{
    CBUF_PUT_VALUE(float32, cp, value);
}
static inline int cbuf_put_float64(cbuf_t* cp, double value)
{
    CBUF_PUT_VALUE(float64, cp, value);
}
static inline int cbuf_put_uint8(cbuf_t* cp, uint8_t value)
{
    CBUF_PUT_VALUE(uint8, cp, value);
}
static inline int cbuf_put_uint16(cbuf_t* cp, uint16_t value)
{
    CBUF_PUT_VALUE(uint16, cp, value);
}

static inline int cbuf_put_uint32(cbuf_t* cp, uint32_t value)
{
    CBUF_PUT_VALUE(uint32, cp, value);
}

static inline int cbuf_put_uint64(cbuf_t* cp, uint64_t value)
{
    CBUF_PUT_VALUE(uint64, cp, value);
}

static inline int cbuf_put_atom(cbuf_t* cp, const char* value)
{
    CBUF_PUT_VALUE(atom, cp, value);
}

static inline int cbuf_put_tuple_begin(cbuf_t* cp, size_t n)
{
    CBUF_PUT_VALUE(tuple_begin, cp, n);
}

static inline int cbuf_put_tuple_end(cbuf_t* cp, size_t n) 
{
    CBUF_PUT_VALUE(tuple_end, cp, n);
}

static inline int cbuf_put_list_begin(cbuf_t* cp, size_t n)
{
    CBUF_PUT_VALUE(list_begin, cp, n);
}

static inline int cbuf_put_list_end(cbuf_t* cp, size_t n)
{
    CBUF_PUT_VALUE(list_end, cp, n);
}

static inline int cbuf_put_begin(cbuf_t* cp)
{
    CBUF_PUT(begin, cp);
}

static inline int cbuf_put_end(cbuf_t* cp)
{
    CBUF_PUT(end, cp);
}

static inline int cbuf_put_tag_ok(cbuf_t* cp)
{
    CBUF_PUT(tag_ok, cp);
}

static inline int cbuf_put_tag_error(cbuf_t* cp)
{
    CBUF_PUT(tag_error, cp);
}
static inline int cbuf_put_tag_event(cbuf_t* cp)
{
    CBUF_PUT(tag_event, cp);
}

static inline int cbuf_put_string(cbuf_t* cp, const char* value, int n)
{
    CBUF_PUT_VALUE(string, cp, value, n);
}

static inline int cbuf_put_bin(cbuf_t* cp, ErlDrvBinary* bin, size_t len, size_t offset)
{
    CBUF_PUT_VALUE(bin, cp, bin, len, offset);
}

static inline int cbuf_put_binary(cbuf_t* cp, const uint8_t* buf, uint32_t len)
{
    CBUF_PUT_VALUE(binary, cp, buf, len);
}

#endif
