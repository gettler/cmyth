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

exception::exception(const char *str)
{
	msg = str;
}

const char*
exception::what() const throw()
{
	return msg;
}

connection::connection(char *server, unsigned short port,
		       unsigned int buflen, int tcp_rcvbuf)
{
	conn = cmyth_conn_connect_ctrl(server, port, buflen, tcp_rcvbuf);

	if (conn == NULL) {
		throw exception("Connection failed");
	}
}

connection::~connection()
{
	release();
}

void
connection::release(void)
{
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
