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
#include <cppmyth/cppmyth.h>

using namespace cmyth;

event::event(cmyth_event_t type, char *msg)
{
	const char *n = NULL;

	event_type = type;
	event_msg = ref_strdup(msg);

	switch (type) {
	default:
	case CMYTH_EVENT_UNKNOWN:
		n = "unknown";
		break;
	case CMYTH_EVENT_CLOSE:
		n = "connection closed";
		break;
	case CMYTH_EVENT_RECORDING_LIST_CHANGE:
		n = "recording list change";
		break;
	case CMYTH_EVENT_RECORDING_LIST_CHANGE_ADD:
		n = "recording list change add";
		break;
	case CMYTH_EVENT_RECORDING_LIST_CHANGE_UPDATE:
		n = "recording list change update";
		break;
	case CMYTH_EVENT_RECORDING_LIST_CHANGE_DELETE:
		n = "recording list change delete";
		break;
	case CMYTH_EVENT_SCHEDULE_CHANGE:
		n = "schedule change";
		break;
	case CMYTH_EVENT_DONE_RECORDING:
		n = "done recording";
		break;
	case CMYTH_EVENT_QUIT_LIVETV:
		n = "quit livetv";
		break;
	case CMYTH_EVENT_WATCH_LIVETV:
		n = "watch livetv";
		break;
	case CMYTH_EVENT_LIVETV_CHAIN_UPDATE:
		n = "livetv chain update";
		break;
	case CMYTH_EVENT_SIGNAL:
		n = "signal";
		break;
	case CMYTH_EVENT_ASK_RECORDING:
		n = "ask recording";
		break;
	case CMYTH_EVENT_SYSTEM_EVENT:
		n = "system event";
		break;
	case CMYTH_EVENT_UPDATE_FILE_SIZE:
		n = "update file size";
		break;
	case CMYTH_EVENT_GENERATED_PIXMAP:
		n = "generated pixmap";
		break;
	case CMYTH_EVENT_CLEAR_SETTINGS_CACHE:
		n = "clear settings cache";
		break;
	}

	event_name = (const char*)ref_strdup((char*)n);
}

event::~event()
{
	release();
}

void
event::release(void)
{
	if (event_msg) {
		ref_release((void*)event_msg);
		event_msg = NULL;
	}

	if (event_name) {
		ref_release((void*)event_name);
		event_name = NULL;
	}

	event_type = CMYTH_EVENT_UNKNOWN;
}

const char*
event::message(void)
{
	return (const char*)ref_hold((void*)event_msg);
}

const char*
event::name(void)
{
	return (const char*)ref_hold((void*)event_name);
}
