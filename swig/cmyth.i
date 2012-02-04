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

%include "exception.i"

%exception {
    try {
        $action
    } catch (cmyth::exception& e) {
        SWIG_exception(SWIG_RuntimeError,const_cast<char*>(e.what()));
    }
}

#if !defined(SWIGPHP)
%include "cstring.i"

%cstring_output_allocate_size(char **file_data, int *bytes_read, free(*$1));
#endif

%include <cppmyth/cppmyth.h>
