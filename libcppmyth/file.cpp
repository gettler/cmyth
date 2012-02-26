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
#include <stdlib.h>

#include <cppmyth/cppmyth.h>

using namespace cmyth;

file::file(cmyth_proginfo_t prog, filetype_t t)
{
	cmyth_conn_t conn;
	char *host;
	int port;

	type = t;

	host = cmyth_proginfo_host(prog);
	port = cmyth_proginfo_port(prog);

	conn = cmyth_conn_connect_ctrl(host, port, 16 * 1024, 4096);

	if (type == FILETYPE_RECORDING) {
		f = cmyth_conn_connect_file(prog, conn,
					    DEFAULT_BUFLEN, DEFAULT_BUFLEN);
	} else {
		f = cmyth_conn_connect_thumbnail(prog, conn,
						 DEFAULT_BUFLEN, DEFAULT_BUFLEN);
	}

	ref_release(conn);
	ref_release(host);
}

file::~file()
{
	release();
}

void
file::release(void)
{
	if (f) {
		ref_release(f);
		f = NULL;
	}
}

long long
file::seek(long long offset)
{
	return cmyth_file_seek(f, offset, SEEK_SET);
}

long long
file::offset(void)
{
	return cmyth_file_seek(f, 0, SEEK_CUR);
}

int
file::read(char **file_data, int *bytes_read)
{
	char *buf;
	int total, n;
	int max = DEFAULT_BUFLEN;

	buf = (char*)malloc(max);
	total = cmyth_file_request_block(f, max);

	n = 0;
	while (n < total) {
		int bytes = cmyth_file_get_block(f, buf+n, total - n);

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
