%%%---- BEGIN COPYRIGHT --------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ----------------------------------------------------------
%%
%% CBUF (CTI data) 
%%
-ifndef(__CBUF_HRL__).
-define(__CBUF_HRL__, true).

%% FIXME: split encoder/decoder part that use this definition
-define(POINTER_SIZE, 32).
-define(SIZE_SIZE,    32).

-define(OK,             1).
-define(ERROR,          2).
-define(EVENT,          3).
-define(INT8,           4).
-define(UINT8,          5).
-define(INT16,          6).
-define(UINT16,         7).
-define(INT32,          8).
-define(UINT32,         9).
-define(INT64,          10).
-define(UINT64,         11).
-define(BOOLEAN,        12).
-define(FLOAT32,        13).
-define(FLOAT64,        14).
-define(STRING1,        15).
-define(STRING4,        16).
-define(ATOM,           17).
-define(BINARY,         18).
-define(LIST,           19).
-define(LIST_END,       20).
-define(TUPLE,          21).
-define(TUPLE_END,      22).
-define(ENUM,           23).
-define(BITFIELD,       24).
-define(HANDLE,         25).
-define(OBJECT,         26).

-define(INT,            ?INT32).
-define(UINT,           ?UINT32).
-define(FLOAT,          ?FLOAT64).

%% transport types
-define(uint8_t(X),     X:8/native-unsigned-integer).
-define(uint16_t(X),    X:16/native-unsigned-integer).
-define(uint32_t(X),    X:32/native-unsigned-integer).
-define(uint64_t(X),    X:64/native-unsigned-integer).
-define(int8_t(X),      X:8/native-signed-integer).
-define(int16_t(X),     X:16/native-signed-integer).
-define(int32_t(X),     X:32/native-signed-integer).
-define(int64_t(X),     X:64/native-signed-integer).
-define(float32_t(X),   X:32/native-float).
-define(float64_t(X),   X:64/native-float).
-define(pointer_t(X),   X:?POINTER_SIZE/native-unsigned-integer).
-define(size_t(X),      X:?SIZE_SIZE/native-unsigned-integer).
-define(ssize_t(X),     X:?SIZE_SIZE/native-nsigned-integer).
-define(object_t(X),    (X)).

%% "natural" sizes
-define(int_t(X),         ?int32_t(X)).
-define(uint_t(X),        ?uint32_t(X)).
-define(float_t(X),       ?float64_t(X)).

-endif.
