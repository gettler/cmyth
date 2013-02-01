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

channel::channel(cmyth_chanlist_t list, int which)
{
	chan = cmyth_chanlist_get_item(list, which);

	ref_release(list);
}

channel::~channel()
{
	release();
}

void
channel::release(void)
{
	if (chan) {
		ref_release(chan);
		chan = NULL;
	}
}

long
channel::id(void)
{
	return cmyth_channel_id(chan);
}

const char*
channel::name(void)
{
	return cmyth_channel_name(chan);
}

const char*
channel::sign(void)
{
	return cmyth_channel_sign(chan);
}

const char*
channel::string(void)
{
	return cmyth_channel_string(chan);
}
