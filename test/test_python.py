#!/usr/bin/python
#
#  Copyright (C) 2012, Jon Gettler
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
import cmyth

def test_host(host):
    conn = cmyth.connection(host)

    print 'Protocol version: %d' % conn.protocol_version()

    list = conn.get_proglist()

    print 'Recording count: %d' % list.get_count()

    for i in range(list.get_count()):
        prog = list.get_prog(i)
        print '  %s - %s' % (prog.title(), prog.subtitle())
        print '    %s %d' % (prog.pathname(), prog.length())
        print '    %s %s %d' % (prog.channel_sign(), prog.channel_name(),
                              prog.channel_id())
        print '    %s' % prog.description()
        prog.release()

    list.release()
    conn.release()

try:
    test_host('nosuchhost')
except RuntimeError as e:
    print 'Exception: %s' % e

try:
    test_host('localhost')
except RuntimeError as e:
    print 'Exception: %s' % e

ref = cmyth.refmem()

print 'Refs:  %d' % ref.refs()
print 'Bytes: %d' % ref.bytes()
