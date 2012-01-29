//
//  Copyright (C) 2012, Jon Gettler
//  http://www.mvpmc.org/
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
//

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>

#include <cppmyth/cppmyth.h>

static int
is_alive(char *host)
{
	cmyth::connection *conn;

	conn = new cmyth::connection(host);

	if (conn->protocol_version() < 0) {
		return 0;
	} else {
		return 1;
	}
}

int
main(int argc, char **argv)
{
	char *server;

	if (optind == argc) {
		fprintf(stderr, "no server given!\n");
		return -1;
	}

	server = argv[optind];

	if (!is_alive(server)) {
		printf("%s is not responding.\n", server);
		return 1;
	}

	printf("%s is alive.\n", server);

	return 0;
}
