/*
 *  Copyright (C) 2012-2013, Jon Gettler
 *  http://www.mvpmc.org/
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/*
 * cmyth.i - SWIG interface file for libcmyth
 */

%module cmyth

#if defined(SWIGLUA)
%rename(start_t) start;
%rename(end_t) end;
#endif

%{
#if defined(SWIGCFFI)
#include <refmem/refmem.h>
#include <cmyth/cmyth.h>
#else
#include <cppmyth/cppmyth.h>
#endif
%}

#if defined(SWIGCFFI)
%typemap(cout) char* ":pointer";
#endif

#if !defined(SWIGPHP) && !defined(SWIGJAVA) && !defined(SWIGCFFI) && !defined(SWIGLUA)
%include "cstring.i"

%cstring_output_allocate_size(char **file_data, int *bytes_read, free(*$1));
#endif

#if defined(SWIGJAVA)
%pragma(java) jniclasscode=%{
static {
	try {
		try {
			System.loadLibrary("cmyth_java");
		} catch (UnsatisfiedLinkError e) {
			String path;
			path = System.getenv("CMYTH_SWIGLIB");
			if (path == null) {
				throw new UnsatisfiedLinkError();
			}
			System.load(path);
		}
	} catch (UnsatisfiedLinkError e) {
		System.out.println("Failed to load the SWIG cmyth library.");
		e.printStackTrace();
		System.exit(1);
	}
}
%}

%typemap(jni) int *bytes_read "jobject"
%typemap(jtype) int *bytes_read "int"
%typemap(jstype) int *bytes_read "int"
%typemap(javain) int *bytes_read ""

%typemap(jni) char **file_data "jobject"
%typemap(jtype) char **file_data "java.nio.ByteBuffer"
%typemap(jstype) char **file_data "java.nio.ByteBuffer"
%typemap(javain) char **file_data "file_data"

%typemap(in) (char **file_data, int *bytes_read) {
	// file::read() input
	char **fd = (char**)malloc(sizeof(char*));
	int *br = (int*)malloc(sizeof(int));
	$1 = fd;
	$2 = br;
}

%typemap(argout) (char **file_data, int *bytes_read) {
	// file::read() output
	int *br = (int*)$2;
	char *buf;
	if (jresult == 0) {
		jresult = *$2;
		buf = (char*)jenv->GetDirectBufferAddress((jbyteArray)$input);
		//$input = jenv->NewDirectByteBuffer(*$1, *$2);
		memcpy(buf, *$1, *$2);
		free(*$1);
	}
	free($1);
	free($2);
}
#endif /* SWIGJAVA */

#if defined(SWIGPHP)
%typemap(in) (char **file_data, int *bytes_read) {
	// file::read() input
	char **fd = (char**)malloc(sizeof(char*));
	int *br = (int*)malloc(sizeof(int));
	$1 = fd;
	$2 = br;
}

%typemap(argout) (char **file_data, int *bytes_read) {
	// file::read() output
	if (result == 0) {
		result = *$2;
		ZVAL_LONG(return_value,result);
		add_index_stringl(*args[1], 0, *$1, result, 1);
	}
	free($1);
	free($2);
}
#endif /* SWIGPHP */

#if defined(SWIGLUA)
%native(with_object) int with_object(lua_State*L);
%{

//
// with_object()
//
// Use with_object() in Lua to pass a SWIG wrapped object to a c function:
//
//     cmyth.with_object(<function>, <object>, <arg1>, <arg2>, ...)
//
// <function> will be called with <object> being a pointer to a libcppmyth
// object, while the optional arguments will be unmodified.  A pointer to
// <object> can be retrieved with lua_touserdata().
//
static int with_object(lua_State*L)
{
	void *object = NULL;
	lua_CFunction func;
	long long result;
	swig_lua_userdata *usr;
	int i;

	SWIG_check_num_args(__FUNCTION__, 2, 16);

	func = lua_tocfunction(L, 1);

	if (!func) {
		SWIG_fail_arg(__FUNCTION__, 1, "function");
	}

	if(!SWIG_isptrtype(L,2)) {
		SWIG_fail_arg(__FUNCTION__, 2, "object");
	}

	usr = (swig_lua_userdata*)lua_touserdata(L, 2);

	if (!usr) {
		SWIG_fail_arg(__FUNCTION__, 2, "object");
	}

	if (!SWIG_IsOK(SWIG_ConvertPtr(L, 2, (void**)&object, usr->type, 0))) {
		SWIG_fail_ptr(__FUNCTION__, 2, usr->type);
	}

	lua_remove(L, 1);
	lua_pushlightuserdata(L, object);
	lua_replace(L, 1);

	return func(L);

fail:
	lua_error(L);
	return 0;
}
%}

%typemap(in) (char **file_data, int *bytes_read) {
}

%typemap(argout) (char **file_data, int *bytes_read) {
	lua_pushnumber(L, 0);
}
#endif /* SWIGLUA */

typedef unsigned int time_t;

%typemap(newfree) const char * "ref_release($1);";
%newobject category;
%newobject channel_name;
%newobject channel_sign;
%newobject channel_string;
%newobject description;
%newobject end_str;
%newobject host;
%newobject pathname;
%newobject program_id;
%newobject recording_group;
%newobject series_id;
%newobject stars;
%newobject start_str;
%newobject subtitle;
%newobject title;
%newobject message;
%newobject name;

#if defined(SWIGPYTHON)
%typemap(newfree) proginfo * "ref_release($1);";
%newobject get_prog;

%typemap(newfree) proglist * "ref_release($1);";
%newobject get_proglist;

%typemap(newfree) file * "ref_release($1);";
%newobject open;

%typemap(newfree) connection * "ref_release($1);";
%newobject connection;

%typemap(newfree) event * "ref_release($1);";
%newobject get_event;
#endif /* SWIGPYTHON */

#if defined(SWIGCFFI)
%include <refmem/refmem.h>
%include <cmyth/cmyth.h>
#else
%include <cppmyth/cppmyth.h>
#endif
