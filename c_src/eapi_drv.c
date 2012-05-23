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

#include <stdio.h>
#include "../include/eapi_drv.h"

static ErlDrvEntry eapi_drv_entry;
// This is a dummy driver that do nothing

int eapi_dispatch(eapi_ctx_t* ctx, unsigned int cmd, cbuf_t* c_in, cbuf_t* c_out)
{
    return -1;
}

static void eapi_init(ErlDrvData d)
{
}

static void eapi_finish(ErlDrvData d)
{
}

DRIVER_INIT(eapi_drv)
{
    eapi_driver_init(&eapi_drv_entry,
		     eapi_init,
		     eapi_finish);
    eapi_drv_entry.driver_name = "eapi_drv";
    return (ErlDrvEntry*) &eapi_drv_entry;    
}
