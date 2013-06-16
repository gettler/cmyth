//
//  Copyright (C) 2013, Jon Gettler
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
#include <string.h>

#if defined(WITH_SSL)
#include <openssl/md5.h>
#endif

#include "lua.hpp"
#include "lualib.h"
#include "lauxlib.h"

#include <cppmyth/cppmyth.h>

extern "C"
{
	int luaopen_cmyth(lua_State *L);
}

static char* test_file(cmyth::file *file)
{
	char *buf, *str;
	int i, rc, len;
#if defined(WITH_SSL)
	MD5_CTX ctx;
	unsigned char digest[MD5_DIGEST_LENGTH];
#endif

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

	str = (char*)malloc(64);
	str[0] = '\0';

#if defined(WITH_SSL)
	MD5_Final(digest, &ctx);
	for (i=0; i<MD5_DIGEST_LENGTH; i++) {
		char val[3];
		snprintf(val, sizeof(val), "%.2x", digest[i]);
		strcat(str, val);
	}
#endif

	return str;
}

static int file_md5(lua_State *L)
{
	cmyth::file *file;
	char *str;

	file = (cmyth::file*)lua_touserdata(L, 1);

	str = test_file(file);
	lua_pushstring(L, str);

	return 1;
}

static char* test_thumbnail(cmyth::file *file, int *bytes)
{
	char *buf, *str;
	int rc, len, size;
#if defined(WITH_SSL)
	int i;
	MD5_CTX ctx;
	unsigned char digest[MD5_DIGEST_LENGTH];
#endif

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

	*bytes = size;

	str = (char*)malloc(64);
	str[0] = '\0';

#if defined(WITH_SSL)
	MD5_Final(digest, &ctx);
	for (i=0; i<MD5_DIGEST_LENGTH; i++) {
		char val[3];
		snprintf(val, sizeof(val), "%.2x", digest[i]);
		strcat(str, val);
	}
#endif

	return str;
}

static int thumbnail_md5(lua_State *L)
{
	cmyth::file *file;
	char *str;
	int bytes;

	file = (cmyth::file*)lua_touserdata(L, 1);

	str = test_thumbnail(file, &bytes);

	lua_pushinteger(L, bytes);
	lua_pushstring(L, str);

	return 2;
}

int main(int argc, const char * argv[])
{
	lua_State *L;
	int rc = 0;
	const char *host = "localhost";

	if (argc < 2) {
		printf("%s: <lua script>\n", argv[0]);
		return 0;
	}

	if (argc >= 3) {
		host = argv[2];
	}

	L = luaL_newstate();

	luaopen_base(L);
	luaL_openlibs(L);
	luaopen_cmyth(L);

	lua_register(L, "file_md5", file_md5);
	lua_register(L, "thumbnail_md5", thumbnail_md5);

	if (luaL_loadfile(L,argv[1]) == 0) {
		lua_pcall(L,0,0,0);
		lua_getglobal(L, "main");
		lua_pushstring(L, host);
		if (lua_pcall(L,1,1,0) != 0) {
			fprintf(stderr, "error: %s\n", lua_tostring(L, -1));
			rc = -1;
		}
	} else {
		fprintf(stderr, "Failed to load %s!\n", argv[1]);
		rc = -1;
	}

	lua_close(L);

	return rc;
}
