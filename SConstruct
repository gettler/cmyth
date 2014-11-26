#
# SCons build script for libcmyth
# http://cmyth.github.com/
#

import os
import sys
import string
import subprocess
import shutil
import atexit

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
    return None

def binary_exists(self, filename):
    if self.find_binary(filename):
        return True
    else:
        return False

def run_command(self, cmd):
    command = subprocess.Popen(cmd,
                               shell=True,
                               stdin=subprocess.PIPE,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
    stdout_value, stderr_value = command.communicate()
    rc = command.wait()
    return rc,stdout_value,stderr_value

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
    prefix = env['PREFIX']
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

def create_binding(env, language):
    try:
        if language == 'clojure':
            return env['CMD_CLOJURE'] != None
        elif language == 'java':
            return env['CMD_JAVAC'] != None and env['JAVA_HOME'] != None
        elif language == 'lisp':
            return env['CMD_LISP'] != None
        elif language == 'lua':
            return env['CMD_LUA'] != None and env['LUA_CFLAGS'] != None and \
                env['LUA_LDFLAGS'] != None
        elif language == 'perl':
            return env['CMD_PERL'] != None and env['PERL_ARCH'] != None
        elif language == 'php':
            return env['CMD_PHP'] != None and env['CMD_PHPCONFIG'] != None \
                and env['PHP_INCLUDE'] != None
        elif language == 'python':
            return env['CMD_PYTHON'] != None and env['PYTHON_INC'] != None
        elif language == 'ruby':
            return env['CMD_RUBY'] != None and env['CMD_GEM'] != None and \
                env['RUBY_HDRDIRS'] != None
        elif language == 'scala':
            return env['CMD_SCALA'] != None
        else:
            return False
    except:
        return False

#
# Initialize the build environment
#
env = Environment()

env.AddMethod(find_binary, 'find_binary')
env.AddMethod(binary_exists, 'binary_exists')
env.AddMethod(run_command, 'run_command')
env.AddMethod(shlibsuffix, 'shlibsuffix')
env.AddMethod(soname, 'soname')
env.AddMethod(build_shared, 'build_shared')
env.AddMethod(shared_library, 'CMSharedLibrary')
env.AddMethod(install_shared, 'InstallShared')
env.AddMethod(gen_version, 'GenVersion')
env.AddMethod(create_binding, 'CreateBinding')

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
    env.Append(CPPFLAGS = ' -DHAS_MYSQL')

conf = Configure(env, custom_tests = { 'CheckBinary' : CheckBinary })
env['CMD_PYSIDEUIC'] = conf.CheckBinary('pyside-uic')
if conf.CheckLib('c', 'arc4random_uniform', autoadd=0):
    env.Append(CPPFLAGS = ' -DHAS_ARC4RANDOM')
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
# Check for binaries that might be required
#
cs = env.find_binary('cscope')
dox = env.find_binary('doxygen')

#
# Check the command line targets
#
do_distclean = env.GetOption('clean') and 'distclean' in COMMAND_LINE_TARGETS
build_cscope = False
build_doxygen = False
if 'cscope' in COMMAND_LINE_TARGETS:
    if cs == None:
        raise SCons.Errors.StopError('cscope command not found!')
    build_cscope = True
if 'doxygen' in COMMAND_LINE_TARGETS:
    if dox == None:
        raise SCons.Errors.StopError('doxygen command not found!')
    build_doxygen = True
if 'all' in COMMAND_LINE_TARGETS:
    if env.binary_exists('doxygen'):
        build_doxygen = True
    if env.binary_exists('cscope'):
        build_cscope = True

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
if (build_cscope and cs) or do_distclean:
    cscope_files = [ Glob('src/*.[ch]'),
                     Glob('src/*.cc'),
                     Glob('src/*.cpp'),
                     Glob('lib*/*.[ch]'),
                     Glob('lib*/*.cc'),
                     Glob('lib*/*.cpp'),
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
    env.Clean(cscope, [ 'cscope.files' ])
    env.Alias('cscope', [cscope])
    all += [cscope]

#
# doxygen target
#
if (build_doxygen and dox) or do_distclean:
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

#
# misc build targets
#
env.Alias('install', [prefix])
env.Alias('all', all)
if env.GetOption('clean'):
    env.Alias('distclean', all)
env.Default(targets)

#
# cleanup
#
if 'all' in COMMAND_LINE_TARGETS or do_distclean:
    env.Clean(all, ['doc/all', 'doc/api', 'doc/man', 'xcode/build'])
if 'doxygen' in COMMAND_LINE_TARGETS or do_distclean:
    env.Clean(all, ['doc/all', 'doc/api', 'doc/man'])

if not env.GetOption('clean'):
    vars.Save('cmyth.conf', env)
    if 'distclean' in COMMAND_LINE_TARGETS:
        raise SCons.Errors.StopError('The distclean target is only valid for cleanup!')

def distclean():
    print 'distclean: cleanup scons data'
    files = [ '.sconsign.dblite', 'config.log', 'cmyth.conf' ]
    files += [ str(name) for name in Glob('scons/*.pyc') ]
    dirs = [ '.sconf_temp' ]
    for f in files:
        try:
            os.remove(f)
        except:
            pass
    for d in dirs:
        try:
            shutil.rmtree(d)
        except:
            pass

if env.GetOption('clean') and 'distclean' in COMMAND_LINE_TARGETS:
    atexit.register(distclean)
