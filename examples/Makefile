EAPI_DIR = ..
include $(EAPI_DIR)/make/eapi_drv.mk

override ERLC_FLAGS = -Ddebug

CFLAGS += -I $(EAPI_DIR)/include 

OBJECT_DRV = object_drv.so

OBJECT_DRV_OBJS = \
	object_api.o \
	object_drv_impl.o

EBIN = .
ERLC = erlc

MODULES = \
	object
BEAMS = $(MODULES:%=$(EBIN)/%.beam)

EAPI_LIB = $(EAPI_DIR)/lib/debug/eapi_drv.a

all: $(OBJECT_DRV) $(BEAMS)

$(OBJECT_DRV): $(OBJECT_DRV_OBJS)
	$(LD_SHARED) -o $@ $(OBJECT_DRV_OBJS) $(EAPI_LIB) $(LDFLAGS)

object_api.o: $(EAPI_DIR)/include/eapi_drv.h
object_drv_impl.o: $(EAPI_DIR)/include/eapi_drv.h

%.o: %.c
	$(CC) -c -o $@ $(CFLAGS) -I$(ERL_C_INCLUDE_DIR) $<

$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN) $<

