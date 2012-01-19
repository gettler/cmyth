#!/usr/bin/python
#
# python module for libcmyth
#
'''
cmyth is a Python module for connecting to a MythTV backend.
'''

import os
import ctypes

try:
    import cmyth_ffi
except ImportError, e:
    raise ImportError (str(e) + 'cmyth_ffi module import failed.')

cmyth = cmyth_ffi
refmem = cmyth_ffi

default_port = 6543
default_buflen = 128 * 1024
default_tcp_rcvbuf = 4096

class ExceptionCmyth(Exception):
    '''cmyth exception base class'''

    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)

    def get_trace(self):
        tblist = traceback.extract_tb(sys.exc_info()[2])
        tblist = [item for item in tblist if self.__filter_not_cmyth(item)]
        tblist = traceback.format_list(tblist)
        return ''.join(tblist)

    def __filter_not_cmyth(self, item):
        if item[0].find('cmyth.py') == -1:
            return True
        else:
            return False

#
# MythTV Connection Class
#
class connection:
    '''A MythTV connection'''

    def __init__(self, server, port=default_port, buflen=default_buflen,
                 tcp_rcvbuf=default_tcp_rcvbuf):
        self.conn = None
        self.conn = cmyth.cmyth_conn_connect_ctrl(server, port, buflen,
                                                  tcp_rcvbuf)
        if self.protocol_version() < 0:
            self.conn = None
            raise ExceptionCmyth('connection failed')

    def __del__(self):
        if self.conn != None:
            refmem.ref_release(self.conn)

    def protocol_version(self):
        return cmyth.cmyth_conn_get_protocol_version(self.conn)

    def get_proglist(self):
        return proglist(self.conn)

#
# MythTV File Class
#
class recording:
    '''A MythTV recording'''

    def __init__(self, prog, conn, buflen, tcp_rcvbuf):
        self.file = cmyth.cmyth_conn_connect_file(prog, conn, buflen, tcp_rcvbuf)
        self.buf = ctypes.create_string_buffer(buflen)

    def __del__(self):
        refmem.ref_release(self.file)

    def seek(self, offset):
        return cmyth.cmyth_file_seek(self.file, offset, os.SEEK_SET)

    def request_block(self, size):
        self.max = size;
        return cmyth.cmyth_file_request_block(self.file, size)

    def get_block(self):
        b = ctypes.cast(self.buf, ctypes.c_char_p)
        bytes = cmyth.cmyth_file_get_block(self.file, b, self.max)
        self.max -= bytes
        return bytes,self.buf;

#
# MythTV Proglist Class
#
class proglist:
    '''list of recordings'''

    def __init__(self, conn):
        self.list = cmyth.cmyth_proglist_get_all_recorded(conn)

    def __del__(self):
        refmem.ref_release(self.list)

    def get_count(self):
        return cmyth.cmyth_proglist_get_count(self.list)

    def get_prog(self, which):
        return proginfo(self.list, which)

class proginfo:
    '''program information'''

    def __init__(self, proglist, which):
        self.prog = cmyth.cmyth_proglist_get_item(proglist, which)

    def __del__(self):
        refmem.ref_release(self.prog)

    def get_file(self, server, port=default_port,
                 buflen=default_buflen, tcp_rcvbuf=default_tcp_rcvbuf):
        conn = cmyth.cmyth_conn_connect_ctrl(server, port, buflen, tcp_rcvbuf)
        f = recording(self.prog, conn, buflen, tcp_rcvbuf)
        refmem.ref_release(conn)
        return f

    def host(self):
        return cmyth.cmyth_proginfo_host(self.prog)

    def port(self):
        return cmyth.cmyth_proginfo_port(self.prog)

    def length(self):
        return cmyth.cmyth_proginfo_length(self.prog)

    def title(self):
        return cmyth.cmyth_proginfo_title(self.prog)

    def subtitle(self):
        return cmyth.cmyth_proginfo_subtitle(self.prog)

    def description(self):
        return cmyth.cmyth_proginfo_description(self.prog)

    def category(self):
        return cmyth.cmyth_proginfo_category(self.prog)

    def chanstr(self):
        return cmyth.cmyth_proginfo_chanstr(self.prog)

    def chansign(self):
        return cmyth.cmyth_proginfo_chansign(self.prog)

    def channame(self):
        return cmyth.cmyth_proginfo_channame(self.prog)

    def chan_id(self):
        return cmyth.cmyth_proginfo_chan_id(self.prog)

    def pathname(self):
        return cmyth.cmyth_proginfo_pathname(self.prog)

    def seriesid(self):
        return cmyth.cmyth_proginfo_seriesid(self.prog)

    def programid(self):
        return cmyth.cmyth_proginfo_programid(self.prog)

    def stars(self):
        return cmyth.cmyth_proginfo_stars(self.prog)

#
# Simple test app for when this is called directly.
#
if __name__ == "__main__":
    import sys
    import getopt

    server = None
    verbose = False

    def usage(code):
        print 'Usage: cmyth [options]'
        print '       --help              print this help'
        print '       --server hostname   MythTV server hostname/IP'
        print '       --verbose           verbose output'
        sys.exit(code)

    try:
        opts, args = getopt.getopt(sys.argv[1:], 'hs:v',
                                   [ 'help', 'server=', 'verbose' ])
    except getopt.GetoptError:
        usage(1)

    for o, a in opts:
        if o in ('-h', '--help'):
            usage(0)
        if o in ('-s', '--server'):
            server = a
        if o in ('-v', '--verbose'):
            verbose = True;

    if server == None:
        print 'Error: server not specified!'
        usage(1)

    try:
        conn = connection(server, 6543, 1024 * 128, 4096)
    except:
        print 'Could not connect to MythTV server at %s' % server
        sys.exit(-1)

    print 'Protocol: %d' % conn.protocol_version()

    list = conn.get_proglist()

    print 'Recording count: %d' % list.get_count()

    list = None;
    conn = None;
