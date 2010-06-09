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
