#!/usr/bin/python
#
# MythTV information script
#
# This is an example of how to use the libcmyth python language bindings.
#

import sys
import getopt

import cmyth

server = None
verbose = False
which = None
output = None

o_proglist = False
o_info = False

def do_info():
    print 'Protocol: %d' % conn.protocol_version()
    list = conn.get_proglist()
    print 'Recording count: %d' % list.get_count()

def do_proglist():
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

def do_cat(i):
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

def usage(code):
    print 'Usage: mythinfo.py [options]'
    print '       --cat number        dump a recording to stdout'
    print '       --help              print this help'
    print '       --info              print backend info'
    print '       --output filename   filename to write recording'
    print '       --proglist          list all recorded programs'
    print '       --server hostname   MythTV server hostname/IP'
    print '       --verbose           verbose output'
    sys.exit(code)

try:
    opts, args = getopt.getopt(sys.argv[1:], 'c:hio:ps:v',
                               [ 'cat=', 'help', 'info', 'proglist',
                                 'output=', 'server=', 'verbose' ])
except getopt.GetoptError:
    usage(1)

for o, a in opts:
    if o in ('-c', '--cat'):
        which = a
    if o in ('-h', '--help'):
        usage(0)
    if o in ('-i', '--info'):
        o_info = True
    if o in ('-o', '--output'):
        output = a;
    if o in ('-p', '--proglist'):
        o_proglist = True;
    if o in ('-s', '--server'):
        server = a
    if o in ('-v', '--verbose'):
        verbose = True;

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
        do_cat(int(which))
    except:
        print 'Failure detected!'
        sys.exit(-1)
    sys.exit(0)

if o_info:
    do_info();

if o_proglist:
    do_proglist();
