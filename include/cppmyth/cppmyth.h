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

#ifndef __CPPMYTH_H
#define __CPPMYTH_H

extern "C" {
#include <cmyth/cmyth.h>
#include <refmem/refmem.h>
}

#include <exception>

namespace cmyth {

class connection;
class proginfo;
class proglist;
class recording;

class exception : public std::exception {
public:
	exception(const char *str = "cmyth exception");
	virtual const char* what() const throw();

	void release(void) { }

private:
	const char *msg;
};

class refmem {
public:
	refmem();
	~refmem();

	unsigned int refs(void);
	unsigned int bytes(void);

	void show(void);

	void release(void) { }
};

class connection {
public:
	connection(char *server, unsigned short port = 6543,
		   unsigned int buflen = 128 * 1024, int tcp_rcvbuf = 4096);
	~connection();

	int protocol_version(void);
	proglist* get_proglist(void);

	void release(void);

private:
	cmyth_conn_t conn;
};

class proglist {
public:
	proglist(cmyth_conn_t conn);
	~proglist();

	int get_count(void);
	proginfo* get_prog(int which);

	void release(void);

private:
	cmyth_proglist_t list;
};

class proginfo {
public:
	proginfo(cmyth_proglist_t list, int which);
	~proginfo();

	int port(void);

	long long length(void);
	long channel_id(void);

	char* category(void);
	char* channel_name(void);
	char* channel_sign(void);
	char* channel_string(void);
	char* description(void);
	char* host(void);
	char* pathname(void);
	char* program_id(void);
	char* series_id(void);
	char* stars(void);
	char* subtitle(void);
	char* title(void);

	void release(void);

private:
	cmyth_proginfo_t prog;
};

}

#endif /* __CPPMYTH_H */
