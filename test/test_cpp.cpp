//
//  Copyright (C) 2012-2013, Jon Gettler
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
	event *ev;
	const char *start, *end;

	conn = new connection(host);

	printf("Protocol version: %d\n", conn->protocol_version());

	ev = conn->get_event(0.1);

	if (ev) {
		const char *msg = ev->message();
		const char *name = ev->name();

		printf("Event: \"%s\" (%d) \"%s\"\n", name, ev->type(), msg);

		ref_release((void*)name);
		ref_release((void*)msg);

		delete(ev);
	}

	list = conn->get_proglist();

	printf("Recording count: %d\n", list->get_count());
	printf("Storage space total: %lld  used: %lld\n",
	       conn->storage_space_total(), conn->storage_space_used());

	for (int i=0; i<list->get_count(); i++) {
		const char *title, *subtitle, *description;
		const char *pathname, *chansign, *channame;

		prog = list->get_prog(i);

		title = prog->title();
		subtitle = prog->subtitle();
		description = prog->description();
		pathname = prog->pathname();
		chansign = prog->channel_sign();
		channame = prog->channel_name();

		printf("  %s - %s\n", title, subtitle);
		printf("    %s %lld\n", pathname, prog->length());
		printf("    %ld - %ld\n", prog->start(), prog->end());
		start = prog->start_str();
		end = prog->end_str();
		printf("    %s - %s\n", start, end);
		printf("    %s %s %ld\n",
		       chansign, channame, prog->channel_id());
		printf("    %s\n", description);

		ref_release((void*)start);
		ref_release((void*)end);
		ref_release((void*)title);
		ref_release((void*)subtitle);
		ref_release((void*)description);
		ref_release((void*)pathname);
		ref_release((void*)chansign);
		ref_release((void*)channame);

		delete(prog);
	}

	if (conn->hung()) {
		printf("Connection is hung!\n");
	}

	delete(list);

	list = conn->get_proglist(PROGTYPE_PENDING);
	printf("Pending count: %d\n", list->get_count());

	for (int i=0; i<list->get_count(); i++) {
		const char *title, *subtitle;
		const char *chansign, *channame;

		prog = list->get_prog(i);

		title = prog->title();
		subtitle = prog->subtitle();
		chansign = prog->channel_sign();
		channame = prog->channel_name();

		printf("  %s - %s\n", title, subtitle);
		start = prog->start_str();
		end = prog->end_str();
		printf("    %s - %s\n", start, end);
		printf("    %s %s %ld\n",
		       chansign, channame, prog->channel_id());

		ref_release((void*)start);
		ref_release((void*)end);
		ref_release((void*)title);
		ref_release((void*)subtitle);
		ref_release((void*)chansign);
		ref_release((void*)channame);

		delete(prog);
	}

	delete(list);

	list = conn->get_proglist(PROGTYPE_SCHEDULED);
	printf("Scheduled count: %d\n", list->get_count());

	for (int i=0; i<list->get_count(); i++) {
		const char *title;

		prog = list->get_prog(i);

		title = prog->title();

		printf("  %s\n", title);

		ref_release((void*)title);

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

void test_thumbnail(const char *host)
{
	connection *conn;
	proglist *list;
	proginfo *prog;
	file *file;
	char *buf;
	int rc, len, size;
#if defined(WITH_SSL)
	int i;
	MD5_CTX ctx;
	unsigned char digest[MD5_DIGEST_LENGTH];
#endif

	conn = new connection(host);
	list = conn->get_proglist();
	prog = list->get_prog(0);
	file = prog->open(FILETYPE_THUMBNAIL);
	file->seek(0);	
#if defined(WITH_SSL)
	MD5_Init(&ctx);
#endif
	size = 0;
	while (1) {
		rc = file->read(&buf, &len);
		if ((rc < 0) || (len == 0)) {
			break;
		}
#if defined(WITH_SSL)
		MD5_Update(&ctx, buf, len);
#endif
		size += len;
		free(buf);
	}
	printf("Thumbnail image size: %d\n", size);
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

void test_perf(const char *host)
{
	connection *conn;
	proglist *list;
	proginfo *prog;
	file *file;
	char *buf;
	int rc, len;
	int offset = 0;
	struct timeval start, end, delta;
	float duration;

	conn = new connection(host);
	list = conn->get_proglist();
	prog = list->get_prog(0);
	file = prog->open();
	file->seek(0);

	gettimeofday(&start, NULL);

	while (offset < 67108864) {
		rc = file->read(&buf, &len);
		if (rc < 0) {
			break;
		}
		offset += len;
		free(buf);
	}

	gettimeofday(&end, NULL);

	timersub(&end, &start, &delta);

	duration = (float)delta.tv_sec + (delta.tv_usec / 1000000.0),

	printf("Perf: read %d bytes in %5.2f seconds (%5.2f mb/s)\n",
	       offset, duration, (offset * 8) / duration / 1000000.0);

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

	try {
		test_thumbnail(host);
	} catch (exception& e) {
		printf("Exception: %s\n", e.what());
	}

	try {
		test_perf(host);
	} catch (exception& e) {
		printf("Exception: %s\n", e.what());
	}

	printf("Refs:  %d\n", ref.refs());
	printf("Bytes: %d\n", ref.bytes());

	return 0;
}
