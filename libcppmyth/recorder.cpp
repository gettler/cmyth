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

recorder::recorder(cmyth_conn_t conn, int id, bool attach) throw(exception)
{
	if (attach) {
		if (id > 0) {
			rec = cmyth_conn_get_recorder_from_num(conn, id);
		} else {
			rec = cmyth_conn_get_free_recorder(conn);
		}
	} else {
		if (id > 0) {
			rec = cmyth_conn_get_recorder(conn, id);
		} else {
			throw exception("Recorder ID not specified!");
		}
	}
}

recorder::~recorder()
{
	release();
}

void
recorder::release(void)
{
	if (rec) {
		ref_release(rec);
		rec = NULL;
	}
}

cmyth_recorder_t
recorder::get_recorder(void)
{
	return (cmyth_recorder_t)ref_hold(rec);
}

chanlist*
recorder::get_chanlist(void)
{
	return new chanlist((cmyth_recorder_t)ref_hold(rec));
}
