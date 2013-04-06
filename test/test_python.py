#!/usr/bin/python
#
#  Copyright (C) 2012-2013, Jon Gettler
#  http://www.mvpmc.org/
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#

import sys
import hashlib
import cmyth

def test_host(host):
    conn = cmyth.connection(host)

    print 'Protocol version: %d' % conn.protocol_version()

    recorded = conn.get_proglist()
    pending = conn.get_proglist(cmyth.PROGTYPE_PENDING)
    scheduled = conn.get_proglist(cmyth.PROGTYPE_SCHEDULED)

    e = conn.get_event(0.1)
    if e:
        print 'Event: "%s" (%d) "%s"' % (e.name(), e.type(), e.message())

    print 'Recording count: %d' % recorded.get_count()

    total = conn.storage_space_total()
    used = conn.storage_space_used()
    print 'Storage space total: %d  used: %d' % (total, used)

    for i in range(recorded.get_count()):
        prog = recorded.get_prog(i)
        print '  %s - %s' % (prog.title(), prog.subtitle())
        print '    %s %d' % (prog.pathname(), prog.length())
        print '    %d - %d' % (prog.start(), prog.end())
        print '    %s - %s' % (prog.start_str(), prog.end_str())
        print '    %s - %s - %d' % (prog.channel_sign(), prog.channel_name(),
                                    prog.channel_id())
        print '    %s' % prog.description()

    print 'Pending count: %d' % pending.get_count()

    for i in range(pending.get_count()):
        prog = pending.get_prog(i)
        print '  %s - %s' % (prog.title(), prog.subtitle())
        print '    %s - %s' % (prog.start_str(), prog.end_str())
        print '    %s - %s - %d' % (prog.channel_sign(), prog.channel_name(),
                                    prog.channel_id())

    print 'Scheduled count: %d' % scheduled.get_count()

    for i in range(scheduled.get_count()):
        prog = scheduled.get_prog(i)
        print '  %s' % prog.title()

    if conn.hung():
        print 'Connection is hung!'

def test_file(host):
    conn = cmyth.connection(host)
    list = conn.get_proglist()
    prog = list.get_prog(0)
    file = prog.open()
    m = hashlib.md5()
    file.seek(0)
    for i in range(5):
        rc,buf = file.read()
        if rc < 0 or len(buf) == 0:
            print 'Error: file read failed!'
            break
        m.update(buf)
    print 'MD5: %s' % m.hexdigest()

def test_thumbnail(host):
    conn = cmyth.connection(host)
    list = conn.get_proglist()
    prog = list.get_prog(0)
    file = prog.open(cmyth.FILETYPE_THUMBNAIL)
    m = hashlib.md5()
    file.seek(0)
    size = 0
    while True:
        rc,buf = file.read()
        if rc < 0 or len(buf) == 0:
            break
        m.update(buf)
        size += len(buf)
    print 'Thumbnail image size: %d' % size
    print 'MD5: %s' % m.hexdigest()

if len(sys.argv) > 1:
    host = sys.argv[1]
else:
    host = 'localhost'

try:
    test_host('nosuchhost')
except cmyth.exception as e:
    print 'Exception: %s' % e.what()

try:
    test_host(host)
except cmyth.exception as e:
    print 'Exception: %s' % e.what()

try:
    test_file(host)
except cmyth.exception as e:
    print 'Exception: %s' % e.what()

try:
    test_thumbnail(host)
except cmyth.exception as e:
    print 'Exception: %s' % e.what()

ref = cmyth.refmem()

print 'Refs:  %d' % ref.refs()
print 'Bytes: %d' % ref.bytes()
