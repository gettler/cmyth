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

namespace cmyth {

extern "C" {
#include <cmyth/cmyth.h>
#include <refmem/refmem.h>
}

class connection {
public:
	connection(char *server, unsigned short port = 6543,
		   unsigned int buflen = 128 * 1024, int tcp_rcvbuf = 4096);
	~connection();

	int protocol_version(void);

private:
	cmyth_conn_t conn;
};

}

#endif /* __CPPMYTH_H */
