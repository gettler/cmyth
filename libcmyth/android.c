/*
 *  Copyright (C) 2013, Jon Gettler
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

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <pthread.h>
#include <cmyth_local.h>

/*
 * Android lacks pthread_cancel() and friends, so apparently everyone is left
 * to implement their own version...
 */

struct pt {
	pthread_t id;
	int state;
	int exit;
};

#define MAX_THREADS	16

static struct pt threads[MAX_THREADS];

static pthread_mutex_t ptc_mutex = PTHREAD_MUTEX_INITIALIZER;

static int
find_thread(pthread_t thread)
{
	int i;
	int rc = -1;

	pthread_mutex_lock(&ptc_mutex);

	for (i=0; i<MAX_THREADS; i++) {
		if (threads[i].id == thread) {
			rc = i;
			break;
		}
	}

	pthread_mutex_unlock(&ptc_mutex);

	return rc;
}

static int
find_empty(pthread_t id)
{
	int i;
	int rc = -1;

	pthread_mutex_lock(&ptc_mutex);

	for (i=0; i<MAX_THREADS; i++) {
		if ((threads[i].id == 0) ||
		    (pthread_kill(threads[i].id, 0) < 0)) {
			rc = i;
			threads[i].id = id;
			break;
		}
	}

	pthread_mutex_unlock(&ptc_mutex);

	return rc;
}

static void
sigusr1_handler(int sig)
{
	int i = find_thread(pthread_self());

	if (i >= 0) {
		threads[i].exit = 1;
		if (threads[i].state == PTHREAD_CANCEL_DISABLE) {
			return;
		}
		threads[i].id = 0;
	}

	pthread_exit(0);
}

static int
register_thread(void)
{
	struct sigaction actions;
	int i;

	if ((i=find_empty(pthread_self())) >= 0) {
		threads[i].state = PTHREAD_CANCEL_ENABLE;
		threads[i].exit = 0;
	}

	memset(&actions, 0, sizeof(actions));

	sigemptyset(&actions.sa_mask);
	actions.sa_flags = 0;
	actions.sa_handler = sigusr1_handler;
	sigaction(SIGUSR1, &actions, NULL);

	return i;
}

static void
deregister_thread(void)
{
	int i;

	if ((i=find_thread(pthread_self())) >= 0) {
		threads[i].id = 0;
	}
}

int
pthread_cancel(pthread_t thread)
{
	int i = find_thread(thread);

	if (i >= 0) {
		threads[i].exit = 1;
	}

	return pthread_kill(thread, SIGUSR1);
}

int
pthread_setcancelstate(int state, int *oldstate)
{
	int i = find_thread(pthread_self());

	if (i < 0) {
		i = register_thread();
	}

	if (i >= 0) {
		*oldstate = threads[i].state;
		threads[i].state = state;

		if (threads[i].exit && (state == PTHREAD_CANCEL_ENABLE)) {
			threads[i].id = 0;
			pthread_exit(0);
		}
	}
}

void
pthread_testcancel(void)
{
	int i = find_thread(pthread_self());

	if (i < 0) {
		i = register_thread();
	}

	if ((i >= 0) && (threads[i].exit)) {
		threads[i].id = 0;
		pthread_exit(0);
	}
}
