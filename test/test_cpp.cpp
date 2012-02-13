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

#if defined(WITH_SSL)
#include <openssl/md5.h>
#endif

#include <cppmyth/cppmyth.h>

using namespace cmyth;

void test_host(const char *host)
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
		printf("    %s %s %ld\n",
		       prog->channel_sign(),
		       prog->channel_name(),
		       prog->channel_id());
		printf("    %s\n", prog->description());
		delete(prog);
	}

	delete(list);
	delete(conn);
}

void test_file(const char *host)
{
	connection *conn;
	proglist *list;
	proginfo *prog;
	file *file;
	char *buf;
	int i, rc, len;
#if defined(WITH_SSL)
	MD5_CTX ctx;
	unsigned char digest[MD5_DIGEST_LENGTH];
#endif

	conn = new connection(host);
	list = conn->get_proglist();
	prog = list->get_prog(0);
	file = prog->open();
	file->seek(0);	
#if defined(WITH_SSL)
	MD5_Init(&ctx);
#endif
	for (i=0; i<5; i++) {
		rc = file->read(&buf, &len);
		if (rc < 0) {
			break;
		}
#if defined(WITH_SSL)
		MD5_Update(&ctx, buf, len);
#endif
		free(buf);
	}
#if defined(WITH_SSL)
	MD5_Final(digest, &ctx);
	printf("MD5: ");
	for (i=0; i<MD5_DIGEST_LENGTH; i++) {
		printf("%.2x", digest[i]);
	}
	printf("\n");
#endif
	delete(file);
	delete(prog);
	delete(list);
	delete(conn);
}

int main(int argc, char **argv)
{
	refmem ref;
	const char *host;

	if (argc > 1) {
		host = argv[1];
	} else {
		host = "localhost";
	}

	try {
		test_host("nosuchhost");
	} catch (exception& e) {
		printf("Exception: %s\n", e.what());
	}

	try {
		test_host(host);
	} catch (exception& e) {
		printf("Exception: %s\n", e.what());
	}

	try {
		test_file(host);
	} catch (exception& e) {
		printf("Exception: %s\n", e.what());
	}

	printf("Refs:  %d\n", ref.refs());
	printf("Bytes: %d\n", ref.bytes());

	return 0;
}
