//
// cbuf_core: core function for control buffer managment
//
#ifndef __CBUF_CORE_H__
#define __CBUF_CORE_H__

#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <memory.h>
#include <unistd.h>
#include <sys/types.h>
#ifdef DARWIN
#include <machine/endian.h>
#endif

#include "erl_driver.h"

#define UNUSED __attribute__((unused))

#define CBUF_FLAG_HEAP     0x01  // allocated heap memory
#define CBUF_FLAG_BINARY   0x02  // ErlDrvBinary 
#define CBUF_FLAG_PUT_CTI  0x00  // Put CTI data (default)
#define CBUF_FLAG_PUT_ETF  0x10  // Put ETF data
#define CBUF_FLAG_PUT_TRM  0x20  // Put ErlDrvTerm data
#define CBUF_FLAG_PUT_NIF  0x30  // Put Nif data
#define CBUF_FLAG_PUT_MASK 0x30  // put type value selection

#define CBUF_VEC_SIZE      4     // static vector size
#define CBUF_SEG_EXTRA     256

// Cbuf vector segment
typedef struct {
    uint8_t  flags;   // allocation status (HEAP|BINARY)
    uint8_t* base;    // base pointer
    size_t    size;   // allocated length of segment
    size_t    len;    // used length of segment
    ErlDrvBinary* bp; // reference when segment is in a binary
} cbuf_segment_t;

#define CBUF_MIN_HEAP 64

// Memory for extra non relocatable data
typedef struct _cbuf_heap_t {
    struct _cbuf_heap_t* next;  // chained heap
    size_t    size;             // size of heap
    uint8_t* ptr;               // heap top 
    uint8_t* end;               // end of heap
    uint8_t  base[0];           // base area
} cbuf_heap_t;
    
typedef struct
{
    uint8_t  flags;          // allocation flags
    size_t    ip;            // poistion in current segment
    size_t    iv;            // current segment number
    size_t    vlen;          // used length of v
    size_t    vsize;         // actual length of v
    void*     udata;         // implementation data
    cbuf_heap_t* h;          // heap segments
    cbuf_segment_t* v;       // base segment (initially dv)
    cbuf_segment_t  dv[CBUF_VEC_SIZE];
} cbuf_t;


// Debug
static UNUSED void cbuf_print(cbuf_t* cp,char* name)
{
    size_t i;
    FILE* f = stderr;

    fprintf(f,"cbuf %s = {\r\n", name);
    fprintf(f,"  flags:");
    if (cp->flags & CBUF_FLAG_BINARY)	fprintf(f," binary");
    if (cp->flags & CBUF_FLAG_HEAP)	fprintf(f," heap");
    fprintf(f,"\r\n");
    fprintf(f,"     iv: %zu\r\n", cp->iv);
    fprintf(f,"     ip: %zu\r\n", cp->ip);
    fprintf(f,"  vsize: %zu\r\n", cp->vsize);
    fprintf(f,"   vlen: %zu\r\n", cp->vlen);
    fprintf(f,"     dv: %s\r\n", (cp->v == cp->dv) ? "true" : "false");
    for (i = 0; i < cp->vlen; i++) {
	fprintf(f,"    v[%zu].flags:", i);
	if (cp->v[i].flags & CBUF_FLAG_BINARY)	fprintf(f," binary");
	if (cp->v[i].flags & CBUF_FLAG_HEAP)	fprintf(f," heap");
	fprintf(f,"\r\n");
	fprintf(f,"    v[%zu].base = %p\r\n",  i, cp->v[i].base);
	fprintf(f,"    v[%zu].size = %zu\r\n", i, cp->v[i].size);
	fprintf(f,"    v[%zu].len  = %zu\r\n", i, cp->v[i].len);
	fprintf(f,"    v[%zu].bp   = %p\r\n", i,  cp->v[i].bp);
    }
    fprintf(f,"};\r\n");
}

// copy src to dst. native-endian to big-endian
// src is a buffer holding a number in native endian order 
// dst is a buffer holding a number in big endian order
//
static inline void* memcpy_n2b(void* dst, void* src, size_t len)
{
#if BYTE_ORDER == BIG_ENDIAN
    return memcpy(dst, src, len);
#else
    uint8_t* sp = ((uint8_t*) src) + len;
    uint8_t* dp = (uint8_t*) dst;
    while(len--)
	*dp++ = *--sp;
    return dst;
#endif
}

// Number of bytes written/read to current segment
static inline size_t cbuf_seg_used(cbuf_t* cp)
{
    return cp->ip;
}

// Return a pointer to current poistion
static inline uint8_t* cbuf_seg_ptr(cbuf_t* cp)
{
    return (uint8_t*) (cp->v[cp->iv].base + cp->ip);
}

// Number of byte available to read in current segment
static inline size_t cbuf_seg_r_avail(cbuf_t* cp)
{
    return (cp->iv >= cp->vlen) ? 0 : (cp->v[cp->iv].len - cp->ip);
}

// Total number of byte available for read
static size_t cbuf_r_avail(cbuf_t* cp) 
{
    size_t sz = cbuf_seg_r_avail(cp);
    size_t i = cp->iv + 1;
    while(i < cp->vlen) {
	sz += cp->v[i].len;
	i++;
    }
    return sz;
}

// Number of bytes available to write in current segment
static inline size_t cbuf_seg_w_avail(cbuf_t* cp)
{
    return (cp->iv >= cp->vlen) ? 0 : (cp->v[cp->iv].size - cp->ip);
}

// return 1 if at end of buf 0 otherwise
static inline int cbuf_eob(cbuf_t* cp)
{
    return (cp->iv >= cp->vlen) || 
	((cp->iv == cp->vlen-1) && (cbuf_seg_r_avail(cp) == 0));
}

// Adjust position if end of segment to next segment
static inline void cbuf_adjust_r_ip(cbuf_t* cp)
{
    if (cp->ip >= cp->v[cp->iv].len) {
	cp->iv++;
	cp->ip = 0;
    }
}

// Adjust position if end of segment to next segment
static inline void cbuf_adjust_w_ip(cbuf_t* cp)
{
    if (cp->ip >= cp->v[cp->iv].size) {
	cp->iv++;
	cp->ip = 0;
    }
}


// Rest the cbuf to start & set read flag
static inline void cbuf_reset(cbuf_t* cp, uint32_t skip)
{
    cp->iv = 0;
    cp->ip = 0;
    while(skip > cp->v[cp->iv].len) {
	skip -= cp->v[cp->iv].len;
	cp->iv++;
    }
    cp->ip = skip;
}

// resize (grow) current segment
static uint8_t* cbuf_seg_realloc(cbuf_t* cp, size_t need)
{
    cbuf_segment_t* sp = &cp->v[cp->iv];
    size_t new_size;

    if (sp->len + need <= sp->size) {
	sp->len += need;
	return sp->base + cp->ip;
    }
    new_size = sp->size + need + CBUF_SEG_EXTRA;
    if (sp->flags & CBUF_FLAG_BINARY) { 
	// Data is allocated in ErlDrvBinary
	if (sp->bp) {
	    ErlDrvBinary* bp;
	    // fprintf(stderr, "realloc_binary: %lu\r\n", new_size);
	    if (!(bp = driver_realloc_binary(sp->bp, new_size)))
		return 0;
	    sp->bp = bp;
	}
	else {
	    // fprintf(stderr, "alloc_binary: %lu\r\n", new_size);
	    if (!(sp->bp = driver_alloc_binary(new_size)))
		return 0;
	    memcpy(sp->bp->orig_bytes, sp->base, sp->len);
	}
	sp->base = (uint8_t*) sp->bp->orig_bytes;
    }
    else if (sp->flags & CBUF_FLAG_HEAP) {
	// Data is already dynamic binaries not used
	uint8_t* dp;
	// fprintf(stderr, "realloc: %lu\r\n", new_size);
	if (!(dp = driver_realloc(sp->base, new_size)))
	    return 0;
	sp->base = dp;
    }
    else {
	// Move data from static buffer to dynamic
	uint8_t* base = sp->base;
	uint8_t* dp;

	// fprintf(stderr, "alloc: %lu\r\n", new_size);
	if (!(dp = driver_alloc(new_size)))
	    return 0;
	sp->base = dp;
	memcpy(sp->base, base, sp->len);
	sp->flags |= CBUF_FLAG_HEAP;
    }
    sp->size = new_size;
    return sp->base + cp->ip;
}

// grow the segment vector
static int cbuf_vec_grow(cbuf_t* cp)
{
    size_t vsize = 2*cp->vsize;
    cbuf_segment_t* sp;

    if (cp->v == cp->dv) {
	if (!(sp = driver_alloc(sizeof(cbuf_segment_t)*vsize)))
	    return 0;
	memcpy(sp,cp->dv,CBUF_VEC_SIZE*sizeof(cbuf_segment_t));
    }
    else {
	if (!(sp = driver_realloc(cp->v,sizeof(cbuf_segment_t)*vsize)))
	    return 0;
    }
    cp->v = sp;	
    cp->vsize = vsize;
    return 1;
}

// Terminate current segment (patch iov_len)
// add new segment and increase iv
static UNUSED int cbuf_seg_add(cbuf_t* cp)
{
    cp->v[cp->iv].len = cp->ip;
    cp->iv++;
    cp->ip = 0;
    if (cp->iv >= cp->vlen) {
	cp->vlen++;
	if (cp->vlen >= cp->vsize) {
	    if (!cbuf_vec_grow(cp))
		return 0;
	}
	memset(&cp->v[cp->iv], 0, sizeof(cbuf_segment_t));
    }
    return 1;
}

// Allocate len contigous bytes in current segment
static inline uint8_t* cbuf_seg_alloc(cbuf_t* cp, size_t len)
{
    uint8_t* ptr;

    if (cbuf_seg_w_avail(cp) < len) {
	if (!cbuf_seg_realloc(cp, len))
	    return 0;
    }
    ptr = cbuf_seg_ptr(cp);
    cp->ip += len;
    cp->v[cp->iv].len = cp->ip;
    return ptr;
}

static UNUSED uint8_t* cbuf_heap_alloc(cbuf_t* cp, size_t len)
{
    if (cp->h && (cp->h->ptr + len < cp->h->end)) {
	uint8_t* ptr = cp->h->ptr;
	cp->h->ptr += len;
	return ptr;
    }
    else {
	cbuf_heap_t* hp;
	size_t sz = (len > CBUF_MIN_HEAP) ? len : CBUF_MIN_HEAP;
	hp = (cbuf_heap_t*) driver_alloc(sizeof(cbuf_heap_t) + sz);
	hp->size = sz;
	hp->ptr = hp->base + len;
	hp->end = hp->base + sz;
	hp->next = cp->h;
	cp->h = hp;
	return hp->base;
    }
}


// segmented read & handle end of segment pointer
// read at most len bytes
static int cbuf_seg_read(cbuf_t* cp, void* ptr, size_t len)
{
    size_t n;
    int nread = 0;

    while((cp->iv < cp->vlen) && len && ((n=cbuf_seg_r_avail(cp)) < len)) {
	memcpy(ptr, cp->v[cp->iv].base + cp->ip, n);
	ptr = ((uint8_t*) ptr) + n;
	len -= n;
	cp->iv++;
	cp->ip = 0;
	nread += n;
    }
    if ((n=cbuf_seg_r_avail(cp)) < len) {
	if (n == 0)
	    return nread;
	len = n;
    }
    memcpy(ptr, cp->v[cp->iv].base + cp->ip, len);
    cp->ip += len;
    cbuf_adjust_r_ip(cp);
    return nread+len;
}

// read data from cbuf into ptr,len
static inline int cbuf_read(cbuf_t* cp, void* ptr, size_t len)
{
    if (cbuf_seg_r_avail(cp) < len)
	return cbuf_seg_read(cp, ptr, len);
    else {
	memcpy(ptr, cp->v[cp->iv].base + cp->ip, len);
	cp->ip += len;
	cbuf_adjust_r_ip(cp);
	return len;
    }
}

//
// Write data into segments
// FIXME: add code to expand segments
//
static UNUSED int cbuf_seg_write(cbuf_t* cp, void* ptr, size_t len)
{
    size_t n;
    while((cp->iv < cp->vlen) && len && ((n=cbuf_seg_w_avail(cp)) < len)) {
	memcpy(cp->v[cp->iv].base+cp->ip, ptr, n);
	ptr = ((uint8_t*) ptr) + n;
	cp->v[cp->iv].len = cp->ip + n;
	len -= n;
	cp->iv++;
	cp->ip = 0;
    }
    if (cbuf_seg_w_avail(cp) < len)
	return 0;
    memcpy(cp->v[cp->iv].base + cp->ip, ptr, len);
    cp->ip += len;
    cp->v[cp->iv].len = cp->ip;
    cbuf_adjust_w_ip(cp);
    return 1;    
}

// write data to cbuf, fix fill vector version
static inline int cbuf_write(cbuf_t* cp, void* ptr, size_t len)
{
    uint8_t* p;

    if (!(p = cbuf_seg_alloc(cp, len)))
	return 0;
    switch(len) {
    case 4: p[3] = ((uint8_t*)ptr)[3];
    case 3: p[2] = ((uint8_t*)ptr)[2];
    case 2: p[1] = ((uint8_t*)ptr)[1];
    case 1: p[0] = ((uint8_t*)ptr)[0];
    case 0: break;
    default: memcpy(p, ptr, len); break;
    }
    return 1;
}

//
// Initialize read/write buffer
// for write buffer 
//   flags = 0      => data will be resized with malloc
//   flags = HEAP   => data will be resize with realloc
//   flags = BINARY => data will be resize with binary_alloc
//   flags = PUT_CTI => put CTI format 
//   flags = PUT_ETF => put ETF format
//
static void cbuf_init(cbuf_t* cp, void* buf, size_t len, 
		      size_t skip, uint8_t flags, void* udata)
{
    cp->flags = (flags & CBUF_FLAG_PUT_MASK);
    cp->v     = cp->dv;
    cp->vlen  = 1;
    cp->vsize = CBUF_VEC_SIZE;
    cp->udata = udata;

    cp->v[0].flags = flags;
    cp->v[0].base  = buf;
    cp->v[0].len   = len;
    cp->v[0].size  = len;
    cp->v[0].bp    = 0;
    cp->h     = 0;
    cp->iv    = 0;           // current vector index
    cp->ip    = skip;        // current position in current vector
}

// IOV read only (or copy on write?)
static UNUSED void cbuf_initv(cbuf_t* cp, ErlIOVec* vec, void* udata)
{
    int i;

    cp->flags = 0;
    if (vec->vsize > CBUF_VEC_SIZE)
	cp->v = driver_alloc(sizeof(cbuf_segment_t)*vec->vsize);
    else
	cp->v = cp->dv;
    cp->vsize = vec->vsize;
    cp->vlen  = vec->vsize;
    cp->udata = udata;
    for (i = 0; i < vec->vsize; i++) {
	cp->v[i].flags = 0;
	cp->v[i].base  = (uint8_t*) vec->iov[i].iov_base;
	cp->v[i].size  = vec->iov[i].iov_len;
	cp->v[i].len   = vec->iov[i].iov_len;
	cp->v[i].bp    = vec->binv[i];
    }
    cp->h  = 0;
    cp->iv = 0;
    while((cp->iv < cp->vlen) && !cp->v[cp->iv].len)
	cp->iv++;
    cp->ip = 0;
}


// Create cbuf as a binary 
static UNUSED cbuf_t* cbuf_new_bin(uint8_t* buf,size_t len,size_t skip,
				   void* udata)
{
    cbuf_t* cp;
    ErlDrvBinary* bp;

    if (!(cp = (cbuf_t*) driver_alloc(sizeof(cbuf_t))))
	return 0;
    if (!(bp = driver_alloc_binary(len))) {
	driver_free(cp);
	return 0;
    }
    cbuf_init(cp,bp->orig_bytes,len,skip,CBUF_FLAG_BINARY,udata);
    cp->flags = CBUF_FLAG_HEAP;  // cp is on global heap
    cp->v[0].bp = bp; // the binary ref (after init!)
    if (buf) memcpy(cp->v[0].base, buf, len);
    return cp;
}

/* allocate a combi cbuf_t and buffer (non growing) */
static UNUSED cbuf_t* cbuf_new(uint8_t* buf, uint32_t len, uint32_t skip,
			       void* udata)
{
    cbuf_t* cp;
    char*   bp;

    if (!(cp = (cbuf_t*) driver_alloc(sizeof(cbuf_t))))
	return 0;
    if (!(bp = driver_alloc(len))) {
	driver_free(cp);
	return 0;
    }
    cbuf_init(cp,bp,len,skip,CBUF_FLAG_HEAP,udata);
    cp->flags = CBUF_FLAG_HEAP;
    if (buf) memcpy(cp->v[0].base, buf, len);
    return cp;
}

// Cleanup heap data, when no longer needed
static void cbuf_reset_heap(cbuf_t* cp)
{
    if (cp->h) {
	cbuf_heap_t* hp = cp->h;
	while(hp) {
	    cbuf_heap_t* hpn = hp->next;
	    driver_free(hp);
	    hp = hpn;
	}
    }
}

//
// Cleanup dynamically created vectors etc
// 
static void cbuf_final(cbuf_t* cp)
{
    size_t i;
    for (i = 0; i < cp->vlen; i++) {
	cbuf_segment_t* sp = &cp->v[i];
	if (sp->flags & CBUF_FLAG_BINARY) {
	    if (sp->bp)
		driver_free_binary(sp->bp);
	}
	else if (sp->flags & CBUF_FLAG_HEAP)
	    driver_free(sp->base);
    }
    if (cp->v != cp->dv)
	driver_free(cp->v);
    cbuf_reset_heap(cp);
}


static inline void cbuf_free(cbuf_t* cp)
{
    cbuf_final(cp);
    if (cp->flags & CBUF_FLAG_HEAP)
	driver_free(cp);
}

// Trim buffer to used size (when binary)
// The control interface wont use the size return in the case
// of an allocated binary. THIS IS A BUG (I think)  
// FIXME: a bit dangerous since I do not know the orig_size
//        the real fix is to reallocate!
static inline void cbuf_trim(cbuf_t* cp)
{
    if (cp->v[cp->iv].bp)
	cp->v[cp->iv].bp->orig_size = cbuf_seg_used(cp);
}

/* add "raw" data to cbuf_t buffer */
static inline void cbuf_add(cbuf_t* cp, uint8_t* buf, uint32_t len)
{
    uint8_t* ptr = cbuf_seg_alloc(cp, len);
    memcpy(ptr, buf, len);
}

// skip "data" (reading) moving ptr forward 
static void cbuf_forward(cbuf_t* cp, size_t len)
{
    while(cp->iv < cp->vlen) {
	size_t n = cbuf_seg_r_avail(cp);
	if (n >= len) {
	    cp->ip += len;
	    cbuf_adjust_r_ip(cp);
	    return;
	}
	len -= n;
	cp->iv++;
	cp->ip = 0;
    }
}

// skip backward 
static UNUSED void cbuf_backward(cbuf_t* cp, size_t len)
{
    while(len) {
	size_t n = cbuf_seg_used(cp);
	if (n >= len) {
	    cp->ip -= len;
	    return;
	}
	len -= n;
	if (cp->iv == 0) {
	    cp->ip = 0;
	    return;
	}
	cp->iv--;
	cp->ip = cp->v[cp->iv].len;
    }
    cbuf_adjust_r_ip(cp);
}

#endif
