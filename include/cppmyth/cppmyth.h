/*
 *  Copyright (C) 2012-2013, Jon Gettler
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

/** \file cppmyth.h
 * A C++ library for communicating with a MythTV server.
 */

#ifndef __CPPMYTH_H
#define __CPPMYTH_H

extern "C" {
#include <cmyth/cmyth.h>
#include <refmem/refmem.h>
#include <pthread.h>
}

#include <exception>

namespace cmyth {

#define DEFAULT_BUFLEN	(128 * 1024)
#define DEFAULT_PORT	6543

typedef enum {
	FILETYPE_RECORDING = 0,
	FILETYPE_THUMBNAIL,
} filetype_t;

class event;
class connection;
class proginfo;
class proglist;
class recording;
class file;
class recorder;
class livetv;
class chanlist;
class channel;

class exception : public std::exception {
public:
	exception(const char *str = "cmyth exception");
	~exception() throw();
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

class event {
public:
	event(cmyth_event_t, char *msg = NULL);
	~event();

	void release(void);

	const char *message(void);
	const char *name(void);
	int type(void) { return (int)event_type; }

private:
	const char *event_msg;
	const char *event_name;
	cmyth_event_t event_type;
};

class connection {
public:
	connection(const char *server, unsigned short port = DEFAULT_PORT,
		   unsigned int buflen = DEFAULT_BUFLEN, int tcp_rcvbuf = 4096)
		throw(exception);
	~connection();

	int protocol_version(void);
	proglist* get_proglist(void);

	long long storage_space_total(void);
	long long storage_space_used(void);

	event* get_event(float timeout);
	event* get_event(void) { return get_event(-1); }

	livetv* start_livetv(void);
	recorder* get_recorder(int id);

	bool hung(void);

	void release(void);

	void _watchdog(void);

private:
	cmyth_conn_t conn;
	cmyth_conn_t econn;
	pthread_t wd_thread;
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
	cmyth_conn_t conn;
};

class proginfo {
public:
	proginfo(cmyth_conn_t conn, cmyth_proglist_t list, int which);
	proginfo(cmyth_recorder_t rec);
	~proginfo();

	bool equals(class proginfo *other);

	int port(void);
	int seconds(void);

	long long length(void);

	long channel_id(void);
	long card_id(void);

	time_t start(void);
	time_t end(void);
	time_t original_airdate(void);

	const char* category(void);
	const char* channel_name(void);
	const char* channel_sign(void);
	const char* channel_string(void);
	const char* description(void);
	const char* end_str(void);
	const char* host(void);
	const char* pathname(void);
	const char* program_id(void);
	const char* recording_group(void);
	const char* series_id(void);
	const char* stars(void);
	const char* start_str(void);
	const char* subtitle(void);
	const char* title(void);

	void release(void);

	int commercial_count(void);
	long long commercial_start(int which);
	long long commercial_end(int which);

	file* open(filetype_t type = FILETYPE_RECORDING);

	cmyth_proginfo_t get_prog(void);

private:
	cmyth_proginfo_t prog;
	cmyth_conn_t conn;
	cmyth_commbreaklist_t cbl;

	bool get_cbl(void);
};

class file {
public:
	file(cmyth_proginfo_t prog, filetype_t t = FILETYPE_RECORDING);
	~file();

	long long length(void);

	long long seek(long long offset);
	long long offset(void);
	int read(char **file_data, int *bytes_read);

	void release(void);

private:
	cmyth_file_t f;
	filetype_t type;
};

class channel {
public:
	channel(cmyth_chanlist_t list, int which);
	~channel();

	long id(void);
	const char *name(void);
	const char *sign(void);
	const char *string(void);

	void release(void);

private:
	cmyth_channel_t chan;
};

class chanlist {
public:
	chanlist(cmyth_recorder_t rec);
	~chanlist();

	int get_count(void);
	channel* get_channel(int which);

	void release(void);

private:
	cmyth_chanlist_t list;
};

class recorder {
public:
	recorder(cmyth_conn_t conn, int id, bool attach=false)
		throw(exception);
	~recorder();

	cmyth_recorder_t get_recorder(void);

	chanlist* get_chanlist(void);

	void release(void);

private:
	cmyth_recorder_t rec;
};

class livetv : public recorder {
public:
	livetv(cmyth_conn_t conn, int id=-1)
		throw(exception);
	~livetv();

	bool start(void);
	bool stop(void);

	bool set_channel(const char *name);
	bool check_channel(const char *name);
	bool change_channel(cmyth_channeldir_t dir);

	proginfo* get_prog(void);
	proginfo* get_next_prog(proginfo *prog, cmyth_browsedir_t dir);

	int read(char **file_data, int *bytes_read);

	int recorder_id(void);
	const char *pathname(void);

	void release(void);

private:
	cmyth_recorder_t rec;
};

extern void cmyth_debug_level(int level);
extern void refmem_debug_level(int level);

}

#endif /* __CPPMYTH_H */
