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
# Transcode MythTV recordings into a format that seems to work well on all my
# various devices, and takes up far less space than the originals.  A
# description is added to the output file, along with chapter markers that
# match the original commercial skip points, although many media players do
# not support these mpeg4 extentions.
#
# The user provides the filename to transcode, and all the MythTV information
# is gathered from the MythTV backend via libcmyth.
#
# Add as a MythTV User Job with a command line like:
#
#     mythtranscode.py -p %DIR%/%FILE% -r
#
# This script requires the following tool:
#
#     mencoder
#
# This following tools are optional, but highly recommended:
#
#     MP4Box
#     mp4tags
#

import os
import sys
import cmyth
import getopt
import tempfile
import subprocess
import re

cmd_encode = 'mencoder %s -o %s -vf dsize=720:480:2,scale=-8:-8,harddup -oac faac -faacopts mpeg=4:object=2:raw:br=128 -of lavf -lavfopts format=mp4 -ovc x264 -sws 9 -x264encopts nocabac:level_idc=30:bframes=0:bitrate=1600:threads=auto:global_header:threads=auto:subq=5:frameref=6:partitions=all:trellis=1:chroma_me:me=umh -really-quiet'
cmd_chapters = 'MP4Box -chap %s -tmp %s %s'
cmd_tags = 'mp4tags -c %s %s'

verbose = False

def usage(code):
    """
    Print user help information.
    """
    print 'mythtranscode: <options>'
    print '        -d <password>  --database   MySQL password'
    print '        -h             --help       print this help'
    print '        -o <dir>       --output     output directory'
    print '        -p <file>      --program    program to transcode'
    print '        -r             --replace    replace original file'
    print '        -s <host>      --server     MythTV host'
    print '        -v             --verbose    verbose output'
    sys.exit(code)

def log(msg):
    global verbose

    if verbose:
        print msg

def binary(program):
    for path in os.environ["PATH"].split(os.pathsep):
        path = path.strip('"')
        fpath = os.path.join(path, program)
        if os.path.isfile(fpath) and os.access(fpath, os.X_OK):
            return True
    return None

def timecode(marker):
    divisor = 60
    frames = int((marker % divisor) / 2)
    marker = int(marker / divisor)
    seconds = int(marker % 60)
    marker = int(marker / 60)
    minutes = int(marker % 60)
    marker = int(marker / 60)
    hours = marker
    return hours,minutes,seconds,int(frames*33.3333)

def transcode(prog, dirname, filename, output):
    p = '%s/%s' % (dirname,filename)
    log('Transcode %s to %s.' % (p, output))
    convert = subprocess.Popen(cmd_encode % (p, output),
                               shell=True,
                               stdin=subprocess.PIPE,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
    stdout_value, stderr_value = convert.communicate()
    if convert.wait() != 0:
        print 'Error: Transcode failed: %s' % stderr_value
        sys.exit(-1)

def add_chapters(prog, dirname, output):
    if not binary('MP4Box'):
        log('MP4Box not found, not adding commercial breaks.')
        return
    log('Add %d commercial breaks.' % prog.commercial_count())
    f = tempfile.NamedTemporaryFile()
    n = 1
    for i in range(prog.commercial_count()):
        hours,minutes,seconds,ms = timecode(prog.commercial_start(i))
        f.write('CHAPTER%02d=%.2d:%.2d:%.2d.%.3d\n' %
                (n, hours, minutes, seconds, ms))
        f.write('CHAPTER%02dNAME=Start\n' % (n))
        hours,minutes,seconds,ms = timecode(prog.commercial_end(i))
        n = n + 1
        f.write('CHAPTER%02d=%.2d:%.2d:%.2d.%.3d\n' %
                (n, hours, minutes, seconds, ms))
        f.write('CHAPTER%02dNAME=End\n' % (n))
        n = n + 1
    f.flush()
    chapters = subprocess.Popen(cmd_chapters % (f.name, dirname, output),
                                shell=True,
                                stdin=subprocess.PIPE,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
    stdout_value, stderr_value = chapters.communicate()
    if chapters.wait() != 0:
        log('Error: failed to add chapter markers: %s' % stderr_value)
    f.close()

def add_tags(prog, output):
    if not binary('mp4tags'):
        log('mp4tags not found, not adding mpeg4 tags.')
        return
    log('Add mpeg4 tags.')
    title = prog.title()
    subtitle = prog.subtitle()
    chan0 = prog.channel_string()
    chan1 = prog.channel_sign()
    desc = prog.description()
    comment = '%s: %s - %s %s - %s' % (title, subtitle, chan0, chan1, desc)
    tags = subprocess.Popen(cmd_tags % (re.escape(comment), output),
                            shell=True,
                            stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    stdout_value, stderr_value = tags.communicate()
    if tags.wait() != 0:
        log('Error: failed to add tags: %s' % stderr_value)

def replace_original(dirname, filename, output, server, db_passwd):
    log('Replace original file and update the database.')

    # If the password is not provided, assume we can get it from /etc.
    if db_passwd == None:
        f = open('/etc/mythtv/mysql.txt', 'r')
        lines = f.readlines()
        for line in lines:
            if line.startswith('DBPassword'):
                db_passwd = (line.split('=')[1])[:-1]
        f.close()

    if db_passwd == None:
        print 'Error: MySQL password is unknown!'
        sys.exit(-1)

    # replace file
    oldsize = os.path.getsize('%s/%s' % (dirname, filename))
    cp = subprocess.Popen('mv %s/%s %s/%s.orig' % (dirname, filename, dirname, filename),
                          shell=True,
                          stdin=subprocess.PIPE,
                          stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE)
    stdout_value, stderr_value = cp.communicate()
    if cp.wait() != 0:
        print 'Error: failed to replace file: %s' % stderr_value
        sys.exit(-1)
    cp = subprocess.Popen('cp %s %s/%s' % (output, dirname, filename),
                          shell=True,
                          stdin=subprocess.PIPE,
                          stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE)
    stdout_value, stderr_value = cp.communicate()
    if cp.wait() != 0:
        print 'Error: failed to replace file: %s' % stderr_value
        sys.exit(-1)

    # run mythcommflag --rebuild to rebuild the seektable
    if server == 'localhost':
        mcf = subprocess.Popen('mythcommflag -f %s/%s --rebuild' % (dirname, filename),
                               shell=True,
                               stdin=subprocess.PIPE,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
        stdout_value, stderr_value = mcf.communicate()
        if mcf.wait() != 0:
            print 'Error: mythcommflag failed: %s' % stderr_value
            sys.exit(-1)
    else:
        log('Run "mythcommflag -f %s --rebuild" on the server.' % filename)

    # run mysql to change the filesize
    size = os.path.getsize('%s/%s' % (dirname, filename))
    log('file size changed from %d to %d' % (oldsize, size))
    sql = "UPDATE\nrecorded\nSET\nfilesize = %d\nWHERE\nbasename = '%s';\n" % (size,filename)
    mysql = subprocess.Popen('mysql -h %s -u mythtv -p%s mythconverg' % (server, db_passwd),
                             shell=True,
                             stdin=subprocess.PIPE,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
    mysql.stdin.write(sql)
    stdout_value, stderr_value = mysql.communicate()
    mysql.stdin.close()
    if mysql.wait() != 0:
        print 'Error: mysql update failed: %s' % stderr_value

def main():
    global verbose

    server = None
    filename = None
    dirname = None
    outdir = None
    output = None
    replace = False
    password = None

    try:
        opts, args = getopt.getopt(sys.argv[1:], "d:ho:p:rs:v",
                                   [ "database=", "help", "output=", "program=",
                                     "replace", "server=", 'verbose' ])
    except getopt.GetoptError:
        usage(1)

    for o, a in opts:
        if o in ("-d", "--database"):
            password = a
        if o in ("-h", "--help"):
            usage(0)
        if o in ("-o", "--output"):
            outdir = a
        if o in ("-p", "--program"):
            filename = os.path.basename(a)
            dirname = os.path.dirname(a)
        if o in ("-r", "--replace"):
            replace = True
        if o in ("-s", "--server"):
            server = a
        if o in ("-v", "--verbose"):
            verbose = True

    if server == None:
        server = 'localhost'

    if filename == None:
        print 'Error: filename not provided!'
        sys.exit(-1)

    if dirname == '':
        dirname = os.getcwd()

    if outdir == None:
        if replace == True:
            outdir = dirname
        else:
            outdir = os.getcwd()

    output = '%s/%s.mp4' % (outdir, filename)

    conn = cmyth.connection(server)

    list = conn.get_proglist()
    count = list.get_count()
    for i in range(count):
        prog = list.get_prog(i)
        pathname = prog.pathname()
        if pathname[1:] == filename:
            transcode(prog, dirname, filename, output)
            add_chapters(prog, dirname, output)
            add_tags(prog, output)
            if replace == True:
                replace_original(dirname, filename, output, server, password)

if __name__ == "__main__":
    main()
