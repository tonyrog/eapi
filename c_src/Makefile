
include ../make/eapi_drv.mk

EDRV_OBJS = \
	$(OBJDIR)/$(EAPI_PFX)eapi_drv_lib.o \
	$(OBJDIR)/$(EAPI_PFX)lhash.o

all:
	$(MAKE) lib TYPE=release
	$(MAKE) lib TYPE=debug

# Make eapi library
debug:
	$(MAKE) lib TYPE=debug

release:
	$(MAKE) lib TYPE=release

clean: 
	$(MAKE) clean_internal TYPE=release
	$(MAKE) clean_internal TYPE=debug

clean_debug:
	$(MAKE) clean_internal TYPE=debug

lib: $(LIBDIR)/$(EAPI_PFX)eapi_drv.a

$(LIBDIR)/$(EAPI_PFX)eapi_drv.a: $(EDRV_OBJS)
	rm $@
	$(AR) -r $@ $(EDRV_OBJS)
	$(RANLIB) $@

depend:

clean_internal:
	-rm -f $(OBJDIR)/$(EAPI_PFX)*.o

$(OBJDIR)/$(EAPI_PFX)eapi_drv_lib.o: \
	../include/cbuf.h \
	../include/cbuf_core.h \
	../include/cbuf_get_native.h \
	../include/cbuf_put_cti.h \
	../include/cbuf_put_etf.h \
	../include/cbuf_put_trm.h \
	../include/lhash.h

$(OBJDIR)/$(EAPI_PFX)%.o: %.c
	$(CC) -c -o $@ $(CFLAGS) -I$(ERL_C_INCLUDE_DIR) $<

 $(OBJDIR)/$(EAPI_PFX)%.s: %.c
	$(CC) -S -o $@ $(CFLAGS) -I$(ERL_C_INCLUDE_DIR) $<
