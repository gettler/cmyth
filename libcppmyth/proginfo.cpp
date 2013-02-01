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

#include <string.h>

#include <cppmyth/cppmyth.h>

using namespace cmyth;

proginfo::proginfo(cmyth_conn_t conn, cmyth_proglist_t list, int which)
{
	prog = cmyth_proglist_get_item(list, which);

	cbl = cmyth_get_commbreaklist(conn, prog);
}

proginfo::proginfo(cmyth_recorder_t rec)
{
	prog = cmyth_recorder_get_cur_proginfo(rec);

	cbl = NULL;
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
	if (cbl) {
		ref_release(cbl);
		cbl = NULL;
	}
}

#define get_item_str2(internal,external)			\
const char*							\
proginfo::external(void)					\
{								\
	char *ptr;						\
	ptr = cmyth_proginfo_##internal(prog);			\
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
get_item_num(card_id, long);

get_item_num2(chan_id, channel_id, long);
get_item_num2(length_sec, seconds, int);

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
get_item_str2(recgroup, recording_group);

time_t
proginfo::start(void)
{
	cmyth_timestamp_t ts;
	time_t t;

	ts = cmyth_proginfo_start(prog);
	t = cmyth_timestamp_to_unixtime(ts);

	ref_release(ts);

	return t;
}

time_t
proginfo::end(void)
{
	cmyth_timestamp_t ts;
	time_t t;

	ts = cmyth_proginfo_end(prog);
	t = cmyth_timestamp_to_unixtime(ts);

	ref_release(ts);

	return t;
}

time_t
proginfo::original_airdate(void)
{
	cmyth_timestamp_t ts;
	time_t t;

	ts = cmyth_proginfo_originalairdate(prog);
	t = cmyth_timestamp_to_unixtime(ts);

	ref_release(ts);

	return t;
}

const char*
proginfo::start_str(void)
{
	cmyth_timestamp_t ts;
	time_t t;
	char *str;

	str = (char*)ref_alloc(64);

	ts = cmyth_proginfo_start(prog);
	t = cmyth_timestamp_to_unixtime(ts);
	ctime_r(&t, str);
	str[strlen(str)-1] = '\0';

	ref_release(ts);

	return str;
}

const char*
proginfo::end_str(void)
{
	cmyth_timestamp_t ts;
	time_t t;
	char *str;

	str = (char*)ref_alloc(64);

	ts = cmyth_proginfo_end(prog);
	t = cmyth_timestamp_to_unixtime(ts);
	ctime_r(&t, str);
	str[strlen(str)-1] = '\0';

	ref_release(ts);

	return str;
}

int
proginfo::commercial_count(void)
{
	if (cbl == NULL) {
		return -1;
	}

	return cbl->commbreak_count;
}

long long
proginfo::commercial_start(int which)
{
	if (cbl == NULL) {
		return -1;
	}

	if (which >= cbl->commbreak_count) {
		return 0;
	}

	return cbl->commbreak_list[which]->start_mark;
}

long long
proginfo::commercial_end(int which)
{
	if (cbl == NULL) {
		return -1;
	}

	if (which >= cbl->commbreak_count) {
		return 0;
	}

	return cbl->commbreak_list[which]->end_mark;
}

file*
proginfo::open(filetype_t type)
{
	return new file(prog, type);
}

bool
proginfo::equals(class proginfo *other)
{
	bool rc;
	cmyth_proginfo_t other_prog;

	other_prog = other->get_prog();

	if (cmyth_proginfo_compare(prog, other_prog) == 0) {
		rc = true;
	} else {
		rc = false;
	}

	ref_release(other_prog);

	return rc;
}

cmyth_proginfo_t
proginfo::get_prog(void)
{
	return (cmyth_proginfo_t)ref_hold(prog);
}
