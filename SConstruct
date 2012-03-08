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
    if self.find_binary('javac'):
        dirs = [ '/usr/lib/jvm/default-java',
                 '/System/Library/Frameworks/JavaVM.framework' ]
        for dir in dirs:
            if os.path.isdir(dir):
                env['JAVA_HOME'] = dir
                return True
    return False

def swig_use_php(self):
    if env['PLATFORM'] == 'android':
        return False
    if not self.binary_exists('swig'):
        return False
    if not self.binary_exists('php') or not self.binary_exists('php-config'):
        return False
    rc,phpinc,err = env.run_command('php-config --include-dir')
    if os.path.isdir(phpinc[:-1]):
        return True
    return False

def swig_use_python(self):
    if env['PLATFORM'] == 'android':
        return False
    if not self.binary_exists('swig'):
        return False
    return True

def swig_use_ruby(self):
    if env['PLATFORM'] == 'android':
        return False
    if not self.binary_exists('swig'):
        return False
    if not self.binary_exists('ruby'):
        return False
    rc,rubyarch,err = env.run_command('ruby -rrbconfig -e '
                                      '\'puts Config::CONFIG["archdir"]\'')
    if rc == 0 and os.path.isfile(rubyarch[:-1] + '/ruby.h'):
        return True
    return False

def shlibsuffix(self, major=-1, minor=-1, branch=-1):
    """Create the proper suffix for the shared library on the current OS."""
    if major == -1:
        if env['PLATFORM'] == 'darwin':
            return '.dylib'
        else:
            return '.so'
    elif minor == -1:
        if env['PLATFORM'] == 'darwin':
            return '-%d.dylib' % (major)
        else:
            return '.so.%d' % (major)
    elif branch == -1:
        if env['PLATFORM'] == 'darwin':
            return '-%d.%d.dylib' % (major, minor)
        else:
            return '.so.%d.%d' % (major, minor)
    else:
        if env['PLATFORM'] == 'darwin':
            return '-%d.%d.%d.dylib' % (major, minor, branch)
        else:
            return '.so.%d.%d.%d' % (major, minor, branch)

def soname(self, name, major=0, minor=0, branch=0):
    """Create the linker shared object argument for gcc for this OS."""
    if env['PLATFORM'] == 'darwin':
        return '-Wl,-headerpad_max_install_names,'\
               '-undefined,dynamic_lookup,-compatibility_version,%d.%d.%d,'\
               '-current_version,%d.%d.%d,-install_name,lib%s%s' % \
               (major, minor, branch,
                major, minor, branch,
                name, self.shlibsuffix(major, minor, branch))
    else:
        return '-Wl,-soname,lib%s.so.%d.%d.%d' % (name, major, minor, branch)

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
env.AddMethod(shlibsuffix, 'shlibsuffix')
env.AddMethod(soname, 'soname')

#
# Save the build configuration.
#
vars = Variables('cmyth.conf')
vars.Add('CC', '', 'gcc')
vars.Add('CXX', '', 'g++')
vars.Add('LD', '', 'ld')
vars.Add('CROSS', '', '')
vars.Add('CCFLAGS', '', '-Werror')
vars.Add('LDFLAGS', '', '')
vars.Add('PLATFORM', '', sys.platform)
vars.Update(env)

#
# Override the build settings with environment variables.
#
if 'BUILD_ANDROID' in os.environ:
    env.Replace(PLATFORM = 'android')

if 'CROSS' in os.environ:
    cross = os.environ['CROSS']
    env.Append(CROSS = cross)
    env.Replace(CC = cross + 'gcc')
    env.Replace(CXX = cross + 'g++')
    env.Replace(LD = cross + 'ld')

#
# SCons builders
#
builder = Builder(action = "ln -s ${SOURCE.file} ${TARGET.file}", chdir = True)
env.Append(BUILDERS = {"Symlink" : builder})

if env['PLATFORM'] == 'android':
    ndk_tool = Tool('android_ndk', toolpath = [ 'scons' ])
    ndk_tool(env)

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
if 'PREFIX' in os.environ:
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
swig = SConscript('swig/SConscript')

targets = [ cppmyth, cmyth, refmem, swig ]

if not env['PLATFORM'] == 'android':
    src = SConscript('src/SConscript')
    test = SConscript('test/SConscript')
    targets += [ src, test ]
    env.Depends(src, swig)
    env.Depends(test, swig)

env.Depends(swig, [ refmem, cmyth, cppmyth ])
env.Depends(cmyth, [ refmem ])
env.Depends(cppmyth, [ refmem, cmyth ])

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
    env.Clean(all, [ 'config.log','.sconf_temp','.sconsign.dblite',
                     'xcode/build' ])
if 'doxygen' in COMMAND_LINE_TARGETS:
    env.Clean(all, ['doc'])

if not env.GetOption('clean'):
    vars.Save('cmyth.conf', env)

env.Clean('distclean', [ '.sconsign.dblite', '.sconf_temp', 'config.log',
                         'cmyth.conf' ])
