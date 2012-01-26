#!/usr/bin/python
#
# Transcode MythTV recordings into a format that seems to work well on all my
# various devices, and takes up far less space than the originals.  A description
# is added to the output file, along with chapter markers that match the original
# commercial skip points, although many media players do not support these mpeg4
# extentions.
#
# The user provides the filename to transcode, and all the MythTV information
# is gathered from the MythTV backend via libcmyth.
#
# Add as a MythTV User Job with a command line like:
#
#     mythtranscode.py -p %DIR%/%FILE% -r
#
# This script requires the following tools:
#
#     mencoder
#     MP4Box
#     mp4tags
#

import os
import sys
import cmyth
import getopt
import tempfile
import subprocess

cmd_encode = 'mencoder %s -o %s -vf dsize=720:480:2,scale=-8:-8,harddup -oac faac -faacopts mpeg=4:object=2:raw:br=128 -of lavf -lavfopts format=mp4 -ovc x264 -sws 9 -x264encopts nocabac:level_idc=30:bframes=0:bitrate=1600:threads=auto:global_header:threads=auto:subq=5:frameref=6:partitions=all:trellis=1:chroma_me:me=umh -really-quiet'
cmd_chapters = 'MP4Box -chap %s -tmp %s %s'
cmd_tags = 'mp4tags -c "%s" %s'

server = None
filename = None
dirname = None
outdir = None
output = None
replace = False
verbose = False

def usage(code):
    """
    Print user help information.
    """
    print 'mythtranscode: <options>'
    print '        -h           --help       print this help'
    print '        -o <dir>     --output     output directory'
    print '        -p <file>    --program    program to transcode'
    print '        -r           --replace    replace original file'
    print '        -s <host>    --server     MythTV host'
    print '        -v           --verbose    verbose output'
    sys.exit(code)

def parse_opts():
    global outdir
    global filename
    global dirname
    global replace
    global server
    global verbose

    try:
        opts, args = getopt.getopt(sys.argv[1:], "ho:p:rs:v",
                                   [ "help", "output=", "program=",
                                 "replace", "server=", 'verbose' ])
    except getopt.GetoptError:
        usage(1)

    for o, a in opts:
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

def log(msg):
    global verbose

    if verbose:
        print msg

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

def transcode(prog):
    global dirname
    global filename
    global output
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

def add_chapters(prog):
    global dirname
    global output

    cbl = prog.commbreaklist()
    log('Add %d commercial breaks.' % len(cbl))
    f = tempfile.NamedTemporaryFile()
    n = 1
    for item in cbl:
        hours,minutes,seconds,ms = timecode(int(item[0]))
        f.write('CHAPTER%02d=%.2d:%.2d:%.2d.%.3d\n' %
                (n, hours, minutes, seconds, ms))
        f.write('CHAPTER%02dNAME=Start\n' % (n))
        hours,minutes,seconds,ms = timecode(int(item[1]))
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
        print 'Error: failed to add chapter markers: %s' % stderr_value
        sys.exit(-1)
    f.close()

def add_tags(prog):
    global output

    log('Add mpeg4 tags.')
    title = prog.title()
    subtitle = prog.subtitle()
    chan0 = prog.chanstr()
    chan1 = prog.chansign()
    desc = prog.description()
    comment = '%s: %s - %s %s - %s' % (title, subtitle, chan0, chan1, desc)
    tags = subprocess.Popen(cmd_tags % (comment, output),
                            shell=True,
                            stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    stdout_value, stderr_value = tags.communicate()
    if tags.wait() != 0:
        print 'Error: failed to add tags: %s' % stderr_value
        sys.exit(-1)

def replace_original():
    global dirname
    global filename
    global output

    log('Replace original file and update the database.')

    f = open('/etc/mythtv/mysql.txt', 'r')
    lines = f.readlines()
    for line in lines:
        if line.startswith('DBPassword'):
            db_passwd = (line.split('=')[1])[:-1]
    f.close()

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
    # run mythcommflag --rebuild
    mcf = subprocess.Popen('mythcommflag -f %s/%s --rebuild' % (dirname, filename),
                           shell=True,
                           stdin=subprocess.PIPE,
                           stdout=subprocess.PIPE,
                           stderr=subprocess.PIPE)
    stdout_value, stderr_value = mcf.communicate()
    if mcf.wait() != 0:
        print 'Error: mythcommflag failed: %s' % stderr_value
        sys.exit(-1)
    # run mysql to change the filesize
    size = os.path.getsize('%s/%s' % (dirname, filename))
    log('file size changed from %d to %d' % (oldsize, size))
    sql = "UPDATE\nrecorded\nSET\nfilesize = %d\nWHERE\nbasename = '%s';\n" % (size,filename)
    mysql = subprocess.Popen('mysql -u mythtv -p%s mythconverg' % db_passwd,
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
    global outdir
    global filename
    global dirname
    global server
    global output

    parse_opts()

    output = '%s/%s.mp4' % (outdir, filename)

    conn = cmyth.connection(server)

    list = conn.get_proglist()
    count = list.get_count()
    for i in range(count):
        prog = list.get_prog(i)
        pathname = prog.pathname()
        if pathname[1:] == filename:
            transcode(prog)
            add_chapters(prog)
            add_tags(prog)
            if replace == True:
                replace_original()

if __name__ == "__main__":
    main()
