/*
 *  Copyright (C) 2012, Jon Gettler
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

%{
#include <cppmyth/cppmyth.h>
%}

#if !defined(SWIGPHP) && !defined(SWIGJAVA)
%include "cstring.i"

%cstring_output_allocate_size(char **file_data, int *bytes_read, free(*$1));
#endif

#if defined(SWIGJAVA)
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
	int *br = (int*)$2;
	char *buf;
	if (result == 0) {
		result = *$2;
		ZVAL_LONG(return_value,result);
		add_index_stringl(*args[1], 0, *$1, result, 1);
	}
	free($1);
	free($2);
}
#endif /* SWIGPHP */

typedef unsigned int time_t;

%typemap(newfree) const char * "ref_release($1);";
%newobject start_str;
%newobject end_str;

%include <cppmyth/cppmyth.h>
