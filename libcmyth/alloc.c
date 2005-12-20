/*
 *  Copyright (C) 2005, Eric Lund
 *  http://mvpmc.sourceforge.net/
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
#ident "$Id$"

/*
 * alloc.c -   Memory management functions.  The structures returned from
 *             libcmyth APIs are actually pointers to reference counted
 *             blocks of memory.  The functions provided here handle allocating
 *             these blocks (strictly internally to the library), placing
 *             holds on these blocks (publicly) and releasing holds (publicly).
 *
 *             All pointer type return values, including strings are reference
 *             counted.
 *
 *       NOTE: Since reference counted pointers are used to move
 *             these structures around, it is strictly forbidden to
 *             modify the contents of a structure once its pointer has
 *             been returned to a callerthrough an API function.  This
 *             means that all API functions that modify the contents
 *             of a structure must copy the structure, modify the
 *             copy, and return a pointer to the copy.  It is safe to
 *             copy pointers (as long as you hold them) everyone
 *             follows this simple rule.  There is no need for deep
 *             copying of any structure.
 */
#include <sys/types.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <cmyth.h>
#include <cmyth_local.h>

#include <string.h>
#include <stdio.h>

#ifdef DEBUG
#include <assert.h>
#define ALLOC_MAGIC 0xef37a45d
#define GUARD_MAGIC 0xe3
#endif /* DEBUG */

/*
 * struct refcounter
 *
 * Scope: PRIVATE (local to this module)
 *
 * Description:
 *
 * The structure used to manage references.  One of these is prepended to every
 * allocation.  It contains two key pieces of information:
 *
 * - The reference count on the structure or string attached to it
 *
 * - A pointer to a 'destroy' function (a destructor) to be called when
 *   the last reference is released.  This function facilitates tearing down
 *   of any complex structures contained in the reference counted block.  If
 *   it is NULL, no function is called.
 *
 * NOTE: Make sure this has a word aligned length, as it will be placed
 *       before each allocation and will affect the alignment of pointers.
 */
typedef struct refcounter {
#ifdef DEBUG
	unsigned int magic;
#endif /* DEBUG */
	cmyth_atomic_t refcount;
	size_t length;
	destroy_t destroy;
} refcounter_t;

#ifdef DEBUG
typedef struct {
	unsigned char magic;
} guard_t;
#endif /* DEBUG */

#define CMYTH_REFCNT(p) ((refcounter_t *)(((unsigned char *)(p)) - sizeof(refcounter_t)))
#define CMYTH_DATA(r) (((unsigned char *)(r)) + sizeof(refcounter_t))

/*
 * cmyth_allocate(size_t len)
 * 
 * Scope: PRIVATE (mapped to __cmyth_allocate)
 *
 * Description
 *
 * Allocate a reference counted block of data for use as a libcmyth structure
 * or string.
 *
 * Return Value:
 *
 * Success: A non-NULL pointer to  a block of memory at least 'len' bytes long
 *          and safely aligned.  The block is reference counted and can be
 *          released using cmyth_release().
 *
 * Failure: A NULL pointer.
 */
void *
cmyth_allocate(size_t len)
{
#ifdef DEBUG
	void *block = malloc(sizeof(refcounter_t) + len + sizeof(guard_t));
	guard_t *guard;
#else
	void *block = malloc(sizeof(refcounter_t) + len);
#endif /* DEBUG */
	void *ret = CMYTH_DATA(block);
	refcounter_t *ref = (refcounter_t *)block;

	cmyth_dbg(CMYTH_DBG_DEBUG, "%s(%d, ret = %p, ref = %p) {\n",
		  __FUNCTION__, len, ret, ref);
	if (block) {
		memset(block, 0, sizeof(refcounter_t) + len);
		cmyth_atomic_set(&ref->refcount, 1);
#ifdef DEBUG
		ref->magic = ALLOC_MAGIC;
		guard = (guard_t*)((unsigned long)block +
				   sizeof(refcounter_t) + len);
		guard->magic = GUARD_MAGIC;
#endif /* DEBUG */
		ref->destroy = NULL;
		ref->length = len;
		cmyth_dbg(CMYTH_DBG_DEBUG, "%s(%d, ret = %p, ref = %p) }\n",
			  __FUNCTION__, len, ret, ref);
		return ret;
	}
	cmyth_dbg(CMYTH_DBG_DEBUG, "%s(%d, ret = %p, ref = %p) !}\n",
		  __FUNCTION__, len, ret, ref);
	return NULL;
}

/*
 * cmyth_reallocate(void *p, size_t len)
 * 
 * Scope: PRIVATE (mapped to __cmyth_reallocate)
 *
 * Description
 *
 * Change the allocation size of a reference counted allocation.
 *
 * Return Value:
 *
 * Success: A non-NULL pointer to  a block of memory at least 'len' bytes long
 *          and safely aligned.  The block is reference counted and can be
 *          released using cmyth_release().
 *
 * Failure: A NULL pointer.
 */
void *
cmyth_reallocate(void *p, size_t len)
{
	refcounter_t *ref = CMYTH_REFCNT(p);
	void *ret = cmyth_allocate(len);

	cmyth_dbg(CMYTH_DBG_DEBUG, "%s(%d, ret = %p, ref = %p) {\n",
		  __FUNCTION__, len, ret, ref);
#ifdef DEBUG
	assert(ref->magic == ALLOC_MAGIC);
#endif /* DEBUG */
	if (p && ret) {
		memcpy(ret, p, ref->length);
		cmyth_set_destroy(ret, ref->destroy);
	}
	if (p) {
		cmyth_release(p);
	}
	cmyth_dbg(CMYTH_DBG_DEBUG, "%s(%d, ret = %p, ref = %p) }\n",
		  __FUNCTION__, len, ret, ref);
	return ret;
}

/*
 * cmyth_set_destroy(void *block, destroy_t func)
 * 
 * Scope: PRIVATE (mapped to __cmyth_set_destroy)
 *
 * Description
 *
 * Set the destroy function for a block of data.  The first argument
 * is a pointer to the data block (as returned by cmyth_allocate()).  The
 * second argument is a pointer to the destroy function which, when
 * called, will be passed one argument, the pointer to the block (as
 * returned by cmyth_allocate()).  The destroy function is
 * respsonsible for any cleanup needed prior to finally releasing the
 * memory holding the memory block.
 *
 * Return Value: NONE
 */
void
cmyth_set_destroy(void *data, destroy_t func)
{
	void *block = CMYTH_REFCNT(data);
	refcounter_t *ref = block;

	cmyth_dbg(CMYTH_DBG_DEBUG, "%s(%p, func = %p, ref = %p) {\n",
		  __FUNCTION__, data, func, ref);
#ifdef DEBUG
	assert(ref->magic == ALLOC_MAGIC);
#endif /* DEBUG */
	if (data) {
		ref->destroy = func;
	}
	cmyth_dbg(CMYTH_DBG_DEBUG, "%s(%p, func = %p, ref = %p) }\n",
		  __FUNCTION__, data, func, ref);
}

/*
 * cmyth_strdup(char *str)
 * 
 * Scope: PUBLIC
 *
 * Description
 *
 * Similar to the libc version of strdup() except that it returns a pointer
 * to a reference counted string.
 *
 * Return Value: 
 *
 * Success: A non-NULL pointer to  a reference counted string which can be
 *          released using cmyth_release().
 *
 * Failure: A NULL pointer.
 */
char *
cmyth_strdup(char *str)
{
	size_t len;
	char *ret = NULL;

	cmyth_dbg(CMYTH_DBG_DEBUG, "%s(%p) {\n",
		  __FUNCTION__, str);
	if (str) {
		len = strlen(str) + 1;
		ret = cmyth_allocate(len);
		if (ret) {
			strncpy(ret, str, len);
			ret[len - 1] = '\0';
		}
		cmyth_dbg(CMYTH_DBG_DEBUG,
			  "%s str = %p[%s], len = %d, ret =%p\n",
			  __FUNCTION__, str, str, len, ret);
	}
	cmyth_dbg(CMYTH_DBG_DEBUG, "%s() }\n", __FUNCTION__);
	return ret;
}

/*
 * cmyth_hold(void *p)
 * 
 * Scope: PUBLIC
 *
 * Description
 *
 * This is how holders of references to reference counted blocks take
 * additional references.  The argument is a pointer to a structure or
 * string returned from a libcmyth API function (or from
 * cmyth_allocate).  The structure's reference count will be
 * incremented  and a  pointer to that space returned.
 *
 * There is really  no error condition possible, but if a NULL pointer
 * is passed in, a NULL is returned.
 *
 * NOTE: since this function operates outside of the space that is directly
 *       accessed by  the pointer, if a pointer that was NOT allocated by
 *       cmyth_allocate() is provided, negative consequences are likely.
 *
 * Return Value: A  pointer to the held space
 */
void *
cmyth_hold(void *p)
{
	void *block = CMYTH_REFCNT(p);
	refcounter_t *ref = block;
#ifdef DEBUG
	guard_t *guard;
#endif /* DEBUG */

	cmyth_dbg(CMYTH_DBG_DEBUG, "%s(%p) {\n", __FUNCTION__, p);
	if (p) {
#ifdef DEBUG
		assert(ref->magic == ALLOC_MAGIC);
		guard = (guard_t*)((unsigned long)block +
				   sizeof(refcounter_t) + ref->length);
		assert(guard->magic == GUARD_MAGIC);
#endif /* DEBUG */
		cmyth_atomic_inc(&ref->refcount);
	}
	cmyth_dbg(CMYTH_DBG_DEBUG, "%s(%p) }\n", __FUNCTION__, p);
        return p;
}

/*
 * cmyth_release(void *p)
 * 
 * Scope: PUBLIC
 *
 * Description
 *
 * This is how holders of references to reference counted blocks release
 * those references.  The argument is a pointer to a structure or string
 * returned from a libcmyth API function (or from cmyth_allocate).  The
 * structure's reference count will be decremented and, when it reaches zero
 * the structure's destroy function (if any) will be called and then the
 * memory block will be released.
 *
 * Return Value: NONE
 */
void
cmyth_release(void *p)
{
	void *block = CMYTH_REFCNT(p);
	refcounter_t *ref = block;
#ifdef DEBUG
	guard_t *guard;
#endif /* DEBUG */

	cmyth_dbg(CMYTH_DBG_DEBUG, "%s(%p) {\n", __FUNCTION__, p);
	if (p) {
		cmyth_dbg(CMYTH_DBG_DEBUG,
			  "%s:%d %s(%p,ref = %p,refcount = %p,length = %d)\n",
			  __FILE__, __LINE__, __FUNCTION__,
			  p, ref, ref->refcount, ref->length);
#ifdef DEBUG
		assert(ref->magic == ALLOC_MAGIC);
		guard = (guard_t*)((unsigned long)block +
				   sizeof(refcounter_t) + ref->length);
		assert(guard->magic == GUARD_MAGIC);
#endif /* DEBUG */
		if (cmyth_atomic_dec_and_test(&ref->refcount)) {
			/*
			 * Last reference, destroy the structure (if
			 * there is a destroy function) and free the
			 * block.
			 */
			if (ref->destroy) {
				ref->destroy(p);
			}
			cmyth_dbg(CMYTH_DBG_DEBUG,
				  "%s:%d %s() -- free it\n",
				  __FILE__, __LINE__, __FUNCTION__);
#ifdef DEBUG
			ref->magic = 0;
			guard->magic = 0;
#endif /* DEBUG */
			free(block);
		}
		if (ref->refcount < 0)
			fprintf(stderr, "*** %s(): %p refcount %d ***\n",
				__FUNCTION__, p, ref->refcount);
	}
	cmyth_dbg(CMYTH_DBG_DEBUG, "%s(%p) }\n", __FUNCTION__, p);
}

/*
 * cmyth_str_create(int len)
 * 
 * Scope: PUBLIC
 *
 * Description
 *
 * Allocate space for a string of 'len' bytes.
 *
 * Return Value: A pointer to the space
 */
char *
cmyth_str_create(int len)
{
	return cmyth_allocate(len);
}