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
#include <string.h>

#include <cppmyth/cppmyth.h>

using namespace cmyth;

livetv::livetv(cmyth_conn_t conn, int id) throw(exception) :
	recorder(conn, id, true)
{
	if (id > 0) {
		rec = cmyth_conn_get_recorder_from_num(conn, id);
	} else {
		rec = cmyth_conn_get_free_recorder(conn);
	}

	rec = get_recorder();

	if (rec == NULL) {
		throw exception("Recorder connection failed");
	}

	if (cmyth_recorder_is_recording(rec) != 0) {
		throw exception("Recorder connection failed");
	}
}

livetv::~livetv()
{
	release();
}

void
livetv::release(void)
{
	if (rec) {
		ref_release(rec);
		rec = NULL;
	}
}

bool
livetv::start(void)
{
	if (cmyth_livetv_start(rec) != 0) {
		return false;
	}

	return true;
}

bool
livetv::stop(void)
{
	if (cmyth_livetv_stop(rec) != 0) {
		return false;
	}

	return true;
}

bool
livetv::set_channel(const char *name)
{
	int i;
	cmyth_chanlist_t list;
	char *text = NULL;

	list = cmyth_recorder_get_chanlist(rec);

	for (i=0; i<cmyth_chanlist_get_count(list); i++) {
		cmyth_channel_t chan = cmyth_chanlist_get_item(list, i);

		if (chan) {
			char *cname = cmyth_channel_name(chan);
			char *csign = cmyth_channel_sign(chan);
			char *cstring = cmyth_channel_string(chan);

			if (strcmp(cname, name) == 0) {
				text = (char*)ref_hold(cname);
			} else if (strcmp(csign, name) == 0) {
				text = (char*)ref_hold(cname);
			} else if (strcmp(cstring, name) == 0) {
				text = (char*)ref_hold(cname);
			}

			ref_release(cname);
			ref_release(csign);
			ref_release(cstring);
		}

		ref_release(chan);

		if (text) {
			break;
		}
	}

	ref_release(list);

	if (text) {
		int rc = cmyth_livetv_set_channel(rec, text);
		ref_release(text);
		if (rc == 0) {
			return true;
		}
	}

	return false;

}

bool
livetv::check_channel(const char *name)
{
	return false;
}

bool
livetv::change_channel(cmyth_channeldir_t dir)
{
	return false;
}

proginfo*
livetv::get_prog(void)
{
	return new proginfo(rec);
}

proginfo*
livetv::get_next_prog(proginfo *prog, cmyth_browsedir_t dir)
{
	return NULL;
}

int
livetv::read(char **file_data, int *bytes_read)
{
	char *buf;
	int total, n;
	int max = DEFAULT_BUFLEN;
	struct timeval to;

	buf = (char*)malloc(max);
	total = cmyth_livetv_request_block(rec, max);

	to.tv_sec = 5;
	to.tv_usec = 0;

	if (cmyth_livetv_select(rec, &to) == 0) {
		return -1;
	}

	n = 0;
	while (n < total) {
		int bytes = cmyth_livetv_get_block(rec, buf+n, total - n);

		if (bytes < 0) {
			break;
		}

		n += bytes;
	}

	*bytes_read = n;
	*file_data = buf;

	if (n == total) {
		return 0;
	} else {
		return -1;
	}
}

int
livetv::recorder_id(void)
{
	return cmyth_recorder_get_recorder_id(rec);
}

const char*
livetv::pathname(void)
{
	return cmyth_recorder_get_filename(rec);
}
