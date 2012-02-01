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

#include <cppmyth/cppmyth.h>

using namespace cmyth;

void test_host(char *host)
{
	connection *conn;
	proglist *list;
	proginfo *prog;

	conn = new connection(host);

	printf("Protocol version: %d\n", conn->protocol_version());

	list = conn->get_proglist();

	printf("Recording count: %d\n", list->get_count());

	for (int i=0; i<list->get_count(); i++) {
		prog = list->get_prog(i);
		printf("  %s - %s\n", prog->title(), prog->subtitle());
		printf("    %s %lld\n",
		       prog->pathname(),
		       prog->length());
		printf("    %s %s %d\n",
		       prog->channel_sign(),
		       prog->channel_name(),
		       prog->channel_id());
		printf("    %s\n", prog->description());
		delete(prog);
	}

	delete(list);
	delete(conn);
}

int main(int argc, char **argv)
{
	refmem ref;

	try {
		test_host("nosuchhost");
	} catch (exception& e) {
		printf("Exception: %s\n", e.what());
	}

	try {
		test_host("localhost");
	} catch (exception& e) {
		printf("Exception: %s\n", e.what());
	}

	printf("Refs:  %d\n", ref.refs());
	printf("Bytes: %d\n", ref.bytes());

	return 0;
}
