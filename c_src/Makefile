
include ../make/eapi_drv.mk

EDRV_OBJS = \
	$(OBJDIR)/eapi_drv_lib.o \
	$(OBJDIR)/lhash.o

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

lib: $(LIBDIR)/eapi_drv.a

$(LIBDIR)/eapi_drv.a: $(EDRV_OBJS)
	$(AR) -r $@ $(EDRV_OBJS)
	$(RANLIB) $@

depend:

clean_internal:
	-rm -f $(OBJDIR)/*.o

$(OBJDIR)/eapi_drv_lib.o: \
	../include/cbuf.h \
	../include/cbuf_core.h \
	../include/cbuf_get_native.h \
	../include/cbuf_put_cti.h \
	../include/cbuf_put_etf.h \
	../include/cbuf_put_trm.h \
	../include/lhash.h

$(OBJDIR)/%.o: %.c
	$(CC) -c -o $@ $(CFLAGS) -I$(ERL_C_INCLUDE_DIR) $<

 $(OBJDIR)/%.s: %.c
	$(CC) -S -o $@ $(CFLAGS) -I$(ERL_C_INCLUDE_DIR) $<
