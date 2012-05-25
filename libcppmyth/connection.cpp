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
#include <unistd.h>
#include <cppmyth/cppmyth.h>

using namespace cmyth;

static void*
wd(void *arg)
{
	class connection *c = (class connection*)arg;

	c->_watchdog();

	return NULL;
}

connection::connection(const char *server, unsigned short port,
		       unsigned int buflen, int tcp_rcvbuf) throw(exception)
{
	conn = cmyth_conn_connect_ctrl((char*)server, port, buflen, tcp_rcvbuf);

	if (conn == NULL) {
		throw exception("Connection failed");
	}

	conn = (cmyth_conn_t)ref_hold(conn);

	pthread_create(&wd_thread, NULL, wd, this);
}

connection::~connection()
{
	release();
}

void
connection::release(void)
{
	if (wd_thread) {
		pthread_cancel(wd_thread);
		wd_thread = 0;
		ref_release(conn);
	}

	if (conn) {
		ref_release(conn);
		conn = NULL;
	}
}

int
connection::protocol_version(void)
{
	if (conn) {
		return cmyth_conn_get_protocol_version(conn);
	} else {
		return -1;
	}
}

proglist*
connection::get_proglist(void)
{
	return new proglist(conn);
}

long long
connection::storage_space_used(void)
{
	long long total;
	long long used;

	if (cmyth_conn_get_freespace(conn, &total, &used) < 0) {
		return -1;
	}

	return used;
}

long long
connection::storage_space_total(void)
{
	long long total;
	long long used;

	if (cmyth_conn_get_freespace(conn, &total, &used) < 0) {
		return -1;
	}

	return total;
}

bool
connection::hung(void)
{
	if (cmyth_conn_hung(conn)) {
		return true;
	} else {
		return false;
	}
}

void cmyth::cmyth_debug_level(int level) {
	cmyth_dbg_level(level);
}

void
connection::_watchdog(void)
{
	bool hung = false;

	while (1) {
		if (cmyth_conn_hung(conn)) {
			if (!hung) {
				hung = true;
				printf("Connection hung!\n");
			}
		} else {
			if (hung) {
				hung = false;
				printf("Connection resumed!\n");
			}
		}

		pthread_testcancel();

		sleep(5);
	}
}
