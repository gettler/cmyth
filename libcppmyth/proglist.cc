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

#include <stdio.h>

#include <cppmyth/cppmyth.h>

using namespace cmyth;

proglist::proglist(cmyth_conn_t conn)
{
	list = cmyth_proglist_get_all_recorded(conn);
}

proglist::~proglist()
{
	release();
}

void
proglist::release(void)
{
	if (list) {
		ref_release(list);
		list = NULL;
	}
}

int
proglist::get_count(void)
{
        return cmyth_proglist_get_count(list);
}

proginfo*
proglist::get_prog(int which)
{
	return new proginfo(list, which);
}
