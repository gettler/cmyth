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

#include <cppmyth/cppmyth.h>

using namespace cmyth;

proginfo::proginfo(cmyth_proglist_t list, int which)
{
	prog = cmyth_proglist_get_item(list, which);
}

proginfo::~proginfo()
{
	release();
}

void
proginfo::release(void)
{
	if (prog) {
		ref_release(prog);
		prog = NULL;
	}
}

#define get_item_str2(internal,external)			\
char*								\
proginfo::external(void)					\
{								\
	char *ptr;						\
	ptr = cmyth_proginfo_##internal(prog);			\
	ref_release(ptr);					\
	return ptr;						\
}

#define get_item_str(name)		get_item_str2(name,name)

#define get_item_num2(internal, external, type)			\
type								\
proginfo::external(void)					\
{								\
	return cmyth_proginfo_##internal(prog);			\
}

#define get_item_num(name, type)	get_item_num2(name,name,type)

get_item_num(port, int);
get_item_num(length, long long);

get_item_num2(chan_id, channel_id, long);

get_item_str(category);
get_item_str(description);
get_item_str(host);
get_item_str(pathname);
get_item_str(stars);
get_item_str(subtitle);
get_item_str(title);

get_item_str2(channame, channel_name);
get_item_str2(chansign, channel_sign);
get_item_str2(chanstr, channel_string);
get_item_str2(programid, program_id);
get_item_str2(seriesid, series_id);
