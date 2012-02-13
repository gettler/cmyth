#
# SCons build script for libcmyth
# http://www.mvpmc.org/
#

import os
import sys
import string
import subprocess

from os import pathsep

def find_binary(self, filename):
    """Find a file in the system search path"""
    path = os.environ['PATH']
    paths = string.split(path, pathsep)
    for i in paths:
        name = os.path.join(i, filename)
        if os.path.isfile(name):
            return name
    return ''

def binary_exists(self, filename):
    if self.find_binary(filename) == '':
        return False
    else:
        return True

def run_command(self, cmd):
    command = subprocess.Popen(cmd,
                               shell=True,
                               stdin=subprocess.PIPE,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
    stdout_value, stderr_value = command.communicate()
    rc = command.wait()
    if rc != 0:
        print 'Error: ' + stderr_value
    return rc,stdout_value,stderr_value

def cmd_not_found(self, arg):
    """Abort the build"""
    print 'Error: %s not found!' % arg
    env.Exit(1)

def swig_use_java(self):
    if not self.binary_exists('swig'):
        return False
    if self.find_binary('javac') and 'JAVA_HOME' in self:
        javapath = self['JAVA_HOME']
        if os.path.isfile(javapath + '/include/jni.h') or \
                os.path.isfile(javapath + '/Headers/jni.h'):
            return True
    return False

def swig_use_php(self):
    if not self.binary_exists('swig'):
        return False
    if not self.binary_exists('php') or not self.binary_exists('php-config'):
        return False
    rc,phpinc,err = env.run_command('php-config --include-dir')
    if os.path.isdir(phpinc[:-1]):
        return True
    return False

def swig_use_python(self):
    if not self.binary_exists('swig'):
        return False
    return True

def swig_use_ruby(self):
    if not self.binary_exists('swig'):
        return False
    if not self.binary_exists('ruby'):
        return False
    rc,rubyarch,err = env.run_command('ruby -rrbconfig -e '
                                      '\'puts Config::CONFIG["archdir"]\'')
    if rc == 0 and os.path.isfile(rubyarch[:-1] + '/ruby.h'):
        return True
    return False

#
# Initialize the build environment
#
env = Environment()

env.AddMethod(cmd_not_found, 'cmd_not_found')
env.AddMethod(find_binary, 'find_binary')
env.AddMethod(binary_exists, 'binary_exists')
env.AddMethod(run_command, 'run_command')
env.AddMethod(swig_use_java, 'swig_use_java')
env.AddMethod(swig_use_php, 'swig_use_php')
env.AddMethod(swig_use_python, 'swig_use_python')
env.AddMethod(swig_use_ruby, 'swig_use_ruby')

vars = Variables('cmyth.conf')
vars.Add('CC', '', 'gcc')
vars.Add('CXX', '', 'g++')
vars.Add('LD', '', 'ld')
vars.Add('JAVA_HOME', '', '')

vars.Update(env)

if os.environ.has_key('CROSS'):
    cross = os.environ['CROSS']
    env.Append(CROSS = cross)
    env.Replace(CC = cross + 'gcc')
    env.Replace(CXX = cross + 'g++')
    env.Replace(LD = cross + 'ld')

env.Append(CCFLAGS = '-Werror')

if 'JAVA_HOME' in os.environ:
    env.Append(JAVA_HOME = os.environ['JAVA_HOME'])

#
# Check the command line targets
#
build_cscope = False
build_doxygen = False
if 'cscope' in COMMAND_LINE_TARGETS:
	build_cscope = True
if 'doxygen' in COMMAND_LINE_TARGETS:
	build_doxygen = True
if 'all' in COMMAND_LINE_TARGETS:
        if env.binary_exists('doxygen'):
            build_doxygen = True
        if env.binary_exists('cscope'):
            build_cscope = True

#
# Check for binaries that might be required
#
cs = env.find_binary('cscope')
dox = env.find_binary('doxygen')

#
# Find the install prefix
#
if os.environ.has_key('PREFIX'):
    prefix = os.environ['PREFIX']
else:
    prefix = '/usr/local'

env.Replace(PREFIX = prefix)

Export('env')

#
# source targets
#
cmyth = SConscript('libcmyth/SConscript')
cppmyth = SConscript('libcppmyth/SConscript')
refmem = SConscript('librefmem/SConscript')
src = SConscript('src/SConscript')
swig = SConscript('swig/SConscript')
test = SConscript('test/SConscript')

targets = [ cppmyth, cmyth, refmem, src, swig, test ]

env.Depends(swig, [ refmem, cmyth, cppmyth ])
env.Depends(src, swig)
env.Depends(test, swig)

#
# install targets
#
env.Install(prefix + '/include/cmyth',
            ['include/cmyth/cmyth.h'])
env.Install(prefix + '/include/refmem',
            ['include/refmem/refmem.h', 'include/refmem/atomic.h'])

all = targets

#
# cscope target
#
if build_cscope:
    if cs != '':
        cscope_files = [ Glob('src/*.[ch]'),
                         Glob('src/*.cc'),
                         Glob('lib*/*.[ch]'),
                         Glob('lib*/*.cc'),
                         Glob('include/*.h'),
                         Glob('include/*/*.h') ]
        f = open('cscope.files', 'w')
        for glob in cscope_files:
            for file in glob:
                f.write(str(file))
                f.write("\n")
        f.close()
        cscope = env.Command([ 'cscope.out',
                               'cscope.in.out', 'cscope.po.out' ],
                             cscope_files,
                             [ '%s -b -q -k' % cs ])
        env.Alias('cscope', [cscope])
        all += [cscope]
        if env.GetOption('clean'):
            os.unlink('cscope.files')
    else:
        env.cmd_not_found('cscope')

#
# doxygen target
#
if build_doxygen:
    if dox != '':
        doxygen = env.Command([ 'doc' ],
                              [ 'Doxyfile',
                                Glob('src/*.[ch]'),
                                Glob('src/*.cc'),
                                Glob('lib*/*.[ch]'),
                                Glob('lib*/*.cc'),
                                Glob('include/*.h'),
                                Glob('include/*/*.h') ],
                              [ '%s Doxyfile' % dox ])
        env.Alias('doxygen', [doxygen])
        all += [doxygen]
    else:
        env.cmd_not_found('doxygen')

#
# misc build targets
#
env.Alias('install', [prefix])
env.Alias('all', all)
env.Default(targets)

#
# cleanup
#
if 'all' in COMMAND_LINE_TARGETS:
    env.Clean(all, ['doc', 'cmyth.conf'])
if 'doxygen' in COMMAND_LINE_TARGETS:
    env.Clean(all, ['doc'])

if not env.GetOption('clean'):
    vars.Save('cmyth.conf', env)

env.Clean('distclean', [ '.sconsign.dblite', '.sconf_temp', 'config.log',
                         'cmyth.conf' ])
