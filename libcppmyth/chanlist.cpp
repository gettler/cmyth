/*
 *  Copyright (C) 2013, Jon Gettler
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
#include <unistd.h>
#include <stdlib.h>

#include <cppmyth/cppmyth.h>

using namespace cmyth;

chanlist::chanlist(cmyth_recorder_t rec)
{
	list = cmyth_recorder_get_chanlist(rec);

	ref_release(rec);
}

chanlist::~chanlist()
{
	release();
}

void
chanlist::release(void)
{
	if (list) {
		ref_release(list);
		list = NULL;
	}
}

int
chanlist::get_count(void)
{
	return cmyth_chanlist_get_count(list);
}

channel*
chanlist::get_channel(int which)
{
	return new channel((cmyth_chanlist_t)ref_hold(list), which);
}
