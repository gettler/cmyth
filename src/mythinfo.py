#!/usr/bin/python
#
# Copyright (C) 2012, Jon Gettler
# http://www.mvpmc.org/
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

#
# MythTV information script
#
# This is an example of how to use the libcmyth python language bindings.
#

import sys
import getopt

import cmyth

verbose = False

def do_info(conn):
    print 'Protocol: %d' % conn.protocol_version()
    list = conn.get_proglist()
    print 'Recording count: %d' % list.get_count()

def do_proglist(conn, o_proglist):
    list = conn.get_proglist()
    count = list.get_count()
    print 'Recordings:'
    for i in range(count):
        prog = list.get_prog(i)
        title = prog.title()
        subtitle = prog.subtitle()
        desc = prog.description()
        path = prog.pathname()
        chan = prog.channel_name()
        length = prog.length()
        print '    %s - %s' % (title, subtitle)
        print '        %s - %d bytes' % (path[1:], length)
        print '        ' + chan
        for i in range(prog.commercial_count()):
            print '        %d - %d' % (prog.commercial_start(i),
                                       prog.commercial_end(i))
        start = 0
        end = 60
        for i in range(len(desc)/60):
            print '        ' + desc[start:end]
            start += 60
            end += 60

def do_cat(conn, i, output):
    list = conn.get_proglist()
    count = list.get_count()
    if i > count:
        throw
    f = open(output, 'wb')
    prog = list.get_prog(i)
    file = prog.open()
    length = prog.length()
    name = prog.pathname()[1:]
    print 'Writing %d bytes of %s to %s' % (length, name, output)
    file.seek(0)
    while True:
        rc,buf = file.read()
        if len(buf) == 0:
            break
        f.write(buf)
    file.release()
    f.close()

def do_event(conn):
    e = conn.get_event(0.1)
    if not e:
        print 'Waiting for events...'
        e = conn.get_event()
    while True:
        print 'Event: "%s" (%d) (%s)' % (e.name(), e.type(), e.message())
        if e.type() == 0 or e.type() == 1:
            return
        try:
            e = conn.get_event()
        except:
            return

def usage(code):
    print 'Usage: mythinfo.py [options]'
    print '       --cat number        dump a recording to stdout'
    print '       --event             display server events'
    print '       --help              print this help'
    print '       --info              print backend info'
    print '       --output filename   filename to write recording'
    print '       --proglist          list all recorded programs'
    print '       --server hostname   MythTV server hostname/IP'
    print '       --verbose           verbose output'
    sys.exit(code)

def main():
    global verbose

    try:
        opts, args = getopt.getopt(sys.argv[1:], 'c:ehio:ps:v',
                                   [ 'cat=', 'event', 'help', 'info',
                                     'proglist', 'output=', 'server=',
                                     'verbose' ])
    except getopt.GetoptError:
        usage(1)

    conn = None
    server = None
    which = None
    output = None

    o_proglist = False
    o_info = False
    o_event = False

    for o, a in opts:
        if o in ('-c', '--cat'):
            which = a
        if o in ('-e', '--event'):
            o_event = True
        if o in ('-h', '--help'):
            usage(0)
        if o in ('-i', '--info'):
            o_info = True
        if o in ('-o', '--output'):
            output = a
        if o in ('-p', '--proglist'):
            o_proglist = True;
        if o in ('-s', '--server'):
            server = a
        if o in ('-v', '--verbose'):
            verbose = True

    if server == None:
        print 'Error: server not specified!'
        usage(1)

    try:
        conn = cmyth.connection(server)
    except:
        print 'Could not connect to MythTV server at %s' % server
        sys.exit(-1)

    if which:
        try:
            do_cat(conn, int(which), output)
        except:
            print 'Failure detected!'
            sys.exit(-1)
        sys.exit(0)

    if o_info:
        do_info(conn)

    if o_proglist:
        do_proglist(conn, o_proglist)

    if o_event:
        do_event(conn)

if __name__ == "__main__":
    main()
