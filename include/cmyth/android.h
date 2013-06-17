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

#ifndef __CMYTH_ANDROID_H
#define __CMYTH_ANDROID_H

#if defined(ANDROID)

#include <pthread.h>

#define PTHREAD_CANCEL_ENABLE	0
#define PTHREAD_CANCEL_DISABLE	1

extern int pthread_cancel(pthread_t thread);
extern void pthread_testcancel(void);

#endif /* ANDROID */

#endif /* __CMYTH_ANDROID_H */
