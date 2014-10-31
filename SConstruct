#
# SCons build script for libcmyth
# http://cmyth.github.com/
#

import os
import sys
import string
import subprocess

from os import pathsep

import SCons.Builder

def CheckBinary(context, name):
    context.Message('Checking for %s ...' % name)
    ret = SCons.Util.WhereIs(name)
    if ret == None:
        context.Result('no')
    else:
        context.Result(ret)
    return ret

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
    return rc,stdout_value,stderr_value

def cmd_not_found(self, arg):
    """Abort the build"""
    print 'Error: %s not found!' % arg
    env.Exit(1)

def shlibsuffix(self, major=-1, minor=-1, branch=-1, fork=-1):
    """Create the proper suffix for the shared library on the current OS."""
    if major == -1:
        if env['PLATFORM'] in [ 'darwin', 'ios' ]:
            return '.dylib'
        else:
            return '.so'
    elif minor == -1:
        if env['PLATFORM'] in [ 'darwin', 'ios' ]:
            return '-%d.dylib' % (major)
        else:
            return '.so.%d' % (major)
    elif branch == -1:
        if env['PLATFORM'] in [ 'darwin', 'ios' ]:
            return '-%d.%d.dylib' % (major, minor)
        else:
            return '.so.%d.%d' % (major, minor)
    elif fork == -1 or fork == 0:
        if env['PLATFORM'] in [ 'darwin', 'ios' ]:
            return '-%d.%d.%d.dylib' % (major, minor, branch)
        else:
            return '.so.%d.%d.%d' % (major, minor, branch)
    else:
        if sys.platform == 'darwin':
            return '-%d.%d.%d.%d.dylib' % (major, minor, branch, fork)
        else:
            return '.so.%d.%d.%d.%d' % (major, minor, branch, fork)

def soname(self, name, major=0, minor=0, branch=0, fork=0):
    """Create the linker shared object argument for gcc for this OS."""
    if fork > 0 and sys.platform != 'darwin':
        version = '%d.%d.%d.%d' % (major, minor, branch, fork)
    else:
        version = '%d.%d.%d' % (major, minor, branch)
    if env['PLATFORM'] in [ 'darwin', 'ios' ]:
        return '-Wl,-headerpad_max_install_names,'\
               '-undefined,dynamic_lookup,-compatibility_version,%s,'\
               '-current_version,%s,-install_name,lib%s%s' % \
               (version, version, name,
                self.shlibsuffix(major, minor, branch, fork))
    else:
        return '-Wl,-soname,lib%s.so.%s' % (name, version)

def build_shared(self):
    """Determine if shared objects should be built for this OS."""
    if env['PLATFORM'] in [ 'cygwin', 'ios' ]:
        return False
    else:
        return True

def shared_library(env, target, source, **kw):
    """Create a shared library and the symlinks pointing to it."""
    version = kw['VERSION']
    major = version[0]
    minor = version[1]
    branch = version[2]
    fork = version[3]
    kw['SHLIBSUFFIX'] = env.shlibsuffix(major, minor, branch, fork)
    shared = env.SharedLibrary(target, source, **kw)
    if fork == 0:
        link0 = shared
    else:
        link0 = env.Symlink('lib%s%s' % (target, env.shlibsuffix(major, minor, branch)), shared)
    link1 = env.Symlink('lib%s%s' % (target, env.shlibsuffix(major, minor)), link0)
    link2 = env.Symlink('lib%s%s' % (target, env.shlibsuffix(major)), link1)
    link3 = env.Symlink('lib%s%s' % (target, env.shlibsuffix()), link2)
    return [ shared, link0, link1, link2, link3 ]

def install_shared(env, target, source, **kw):
    """Install a shared library and the symlinks pointing to it."""
    name = kw['NAME']
    version = kw['VERSION']
    major = version[0]
    minor = version[1]
    branch = version[2]
    fork = version[3]
    lib = env.Install(prefix + '/lib', source[0])
    if fork == 0:
        lib0 = lib
    else:
        lib0 = env.Symlink('%s/lib/lib%s%s' % (prefix, name,
                                               env.shlibsuffix(major, minor, branch)), lib)
    lib1 = env.Symlink('%s/lib/lib%s%s' % (prefix, name,
                                           env.shlibsuffix(major, minor)), lib0)
    lib2 = env.Symlink('%s/lib/lib%s%s' % (prefix, name,
                                           env.shlibsuffix(major)), lib1)
    lib3 = env.Symlink('%s/lib/lib%s%s' % (prefix, name,
                                           env.shlibsuffix()), lib2)
    return [ lib, lib0, lib1, lib2, lib3 ]

def gen_version(env, target, source=None, **kw):
    version = kw['VERSION']
    major = version[0]
    minor = version[1]
    branch = version[2]
    fork = version[3]
    path = os.path.dirname(Dir(target[0]).abspath) + '/' + target
    f = open(path, 'w')
    f.write('#define VERSION_MAJOR %d\n' % major)
    f.write('#define VERSION_MINOR %d\n' % minor)
    f.write('#define VERSION_BRANCH %d\n' % branch)
    f.write('#define VERSION_FORK %d\n' % fork)
    f.close()
    return [ path ]

#
# Initialize the build environment
#
env = Environment()

env.AddMethod(cmd_not_found, 'cmd_not_found')
env.AddMethod(find_binary, 'find_binary')
env.AddMethod(binary_exists, 'binary_exists')
env.AddMethod(run_command, 'run_command')
env.AddMethod(shlibsuffix, 'shlibsuffix')
env.AddMethod(soname, 'soname')
env.AddMethod(build_shared, 'build_shared')
env.AddMethod(shared_library, 'CMSharedLibrary')
env.AddMethod(install_shared, 'InstallShared')
env.AddMethod(gen_version, 'GenVersion')

#
# Save the build configuration.
#
cflags = '-Wall -Wextra -Werror -Wno-unused-parameter'
ldflags = ''

vars = Variables('cmyth.conf')
vars.Add('CC', '', 'cc')
vars.Add('CXX', '', 'c++')
vars.Add('LD', '', 'ld')
vars.Add('CROSS', '', '')
vars.Add('CFLAGS', '', cflags)
vars.Add('DEBUGFLAGS', '', '')
vars.Add('CXXFLAGS', '', cflags)
vars.Add('LDFLAGS', '', ldflags)
vars.Add('PLATFORM', '', sys.platform)
vars.Add('HAS_MYSQL', '', '')
vars.Add('CMD_PYSIDEUIC', '', '')
vars.Update(env)

#
# Override the build settings with environment variables.
#
if 'BUILD_ANDROID' in os.environ:
    env.Replace(PLATFORM = 'android')

if 'BUILD_IOS' in os.environ:
    env.Replace(PLATFORM = 'ios')

if 'CC' in os.environ:
    env.Replace(CC = os.environ['CC'])

if 'CXX' in os.environ:
    env.Replace(CC = os.environ['CXX'])

if 'LD' in os.environ:
    env.Replace(LD = os.environ['LD'])

if 'CFLAGS' in os.environ:
    cflags = os.environ['CFLAGS']
    env.Replace(CFLAGS = cflags)

if 'LDFLAGS' in os.environ:
    env.Replace(LDFLAGS = os.environ['LDFLAGS'])

if 'CROSS' in os.environ:
    cross = os.environ['CROSS']
    env.Append(CROSS = cross)
    env.Replace(CC = cross + 'gcc')
    env.Replace(CXX = cross + 'g++')
    env.Replace(LD = cross + 'ld')

if env['PLATFORM'] == 'ios':
    xcrun = 'xcrun -sdk iphoneos'
    cc = xcrun + ' gcc'
    cxx = xcrun + ' g++'
    ld = cxx
    common = '-isysroot "/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS4.0.sdk" -arch armv6 -D__IPHONE_OS_VERSION_MIN_REQUIRED=__IPHONE_3_0 -miphoneos-version-min=3.0'
    ldflags = '-lobjc -framework Foundation -framework CoreFoundation -ObjC++ -fobjc-exceptions -fobjc-call-cxx-cdtors %s -multiply_defined suppress' % common
    cflags = '-DTARGET_IPHONE=1 -O2 -Wall -Werror ' + common
    env.Replace(CC = cc)
    env.Replace(CXX = cxx)
    env.Replace(LD = ld)
    env.Replace(CFLAGS = cflags)
    env.Replace(CXXFLAGS = cflags)
    env.Replace(LDFLAGS = ldflags)

def commonpath(l1, l2, common=[]):
    if len(l1) < 1:
        return (common, l1, l2)
    if len(l2) < 1:
        return (common, l1, l2)
    if l1[0] != l2[0]:
        return (common, l1, l2)
    return commonpath(l1[1:], l2[1:], common+[l1[0]])

def relpath(p1, p2):
    (common,l1,l2) = commonpath(p1.split(os.path.sep), p2.split(os.path.sep))
    p = []
    if len(l1) > 1:
        p = [ '../' * (len(l1)-1) ]
    p = p + l2
    return os.path.join( *p )

if 'DEBUG' in os.environ:
    env.Replace(CFLAGS = cflags + ' -g -DDEBUG')
    env.Replace(DEBUGFLAGS = '-g')

if 'NO_MYSQL' in os.environ:
    env.Replace(HAS_MYSQL = 'no')
elif env['HAS_MYSQL'] == '':
    conf = Configure(env)
    if conf.CheckCHeader('mysql/mysql.h') and conf.CheckLib('mysqlclient'):
        conf.env.Replace(HAS_MYSQL = 'yes')
    env = conf.Finish()

if env['HAS_MYSQL'] == 'yes':
    env.Append(CPPFLAGS = '-DHAS_MYSQL')

conf = Configure(env, custom_tests = { 'CheckBinary' : CheckBinary })
env['CMD_PYSIDEUIC'] = conf.CheckBinary('pyside-uic')
if conf.CheckLib('c', 'arc4random_uniform', autoadd=0):
    env.Append(CPPFLAGS = '-DHAS_ARC4RANDOM')
env = conf.Finish()

#
# SCons builders
#
def create_link(target, source, env):
    src = os.path.abspath(str(source[0]))
    link = os.path.abspath(str(target[0]))
    os.symlink(relpath(link,src), link)

builder = SCons.Builder.Builder(action = create_link)
env.Append(BUILDERS = {"Symlink" : builder})

def cat_files(target, source, env):
    with open(str(target[0]), "w") as f:
        if 'HEADER' in env:
            f.write(env['HEADER'])
            f.write('\n')
        for s in source:
            src = open(str(s), "r")
            buf = src.read()
            f.write(buf)

builder = SCons.Builder.Builder(action = cat_files)
env.Append(BUILDERS = {"CatFiles" : builder})

builder = SCons.Builder.Builder(action = 'pyside-uic $SOURCE > $TARGET')
env.Append(BUILDERS = {"PySideUI" : builder})

if env['PLATFORM'] == 'android':
    ndk_tool = Tool('android_ndk', toolpath = [ 'scons' ])
    ndk_tool(env)

gem_tool = Tool('gen_gemspec', toolpath = [ 'scons' ])
gem_tool(env)

setup_tool = Tool('gen_setup', toolpath = [ 'scons' ])
setup_tool(env)

setup_tool = Tool('gen_asdf', toolpath = [ 'scons' ])
setup_tool(env)

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
    if not prefix[0] == '/':
        prefix = os.getcwd() + '/' + prefix
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

if not env['PLATFORM'] in [ 'android', 'ios']:
    src = SConscript('src/SConscript')
    test = SConscript('test/SConscript')
    targets += [ src, test ]
    env.Depends(src, swig)
    env.Depends(test, swig)

env.Depends(swig, [ refmem, cmyth, cppmyth ])
env.Depends(cmyth, [ refmem ])
env.Depends(cppmyth, [ refmem, cmyth ])

env.Depends(cmyth, refmem)

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
        internal = env.Command([ 'doc/all/html/index.html' ],
                               [ 'doc/Doxyfile.all',
                                 Glob('src/*.[ch]'),
                                 Glob('src/*.cpp'),
                                 Glob('lib*/*.[ch]'),
                                 Glob('lib*/*.cpp'),
                                 Glob('include/*.h'),
                                 Glob('include/*/*.h') ],
                               [ '%s doc/Doxyfile.all' % dox ])
        external = env.Command([ 'doc/api/html/index.html' ],
                               [ 'doc/Doxyfile.api',
                                 Glob('src/*.[ch]'),
                                 Glob('src/*.cpp'),
                                 Glob('include/*/*.h') ],
                               [ '%s doc/Doxyfile.api' % dox ])
        manpages = env.Command([ 'doc/man/man3/cmyth.h.3' ],
                               [ 'doc/Doxyfile.man',
                                 Glob('include/*/*.h') ],
                               [ '%s doc/Doxyfile.man' % dox ])
        doxygen = [ internal, external, manpages ]
        env.Alias('doxygen', doxygen)
        all += doxygen
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
    env.Clean(all, ['doc/all', 'doc/api', 'doc/man', 'cmyth.conf'])
    env.Clean(all, [ 'config.log','.sconf_temp','.sconsign.dblite',
                     'xcode/build' ])
if 'doxygen' in COMMAND_LINE_TARGETS:
    env.Clean(all, ['doc/all', 'doc/api', 'doc/man'])

if not env.GetOption('clean'):
    vars.Save('cmyth.conf', env)

env.Clean('distclean', [ '.sconsign.dblite', '.sconf_temp', 'config.log',
                         'cmyth.conf' ])
