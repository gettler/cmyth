"""
SCons Builder for building Android NDK shared libraries
"""

import SCons.Action
import SCons.Builder
import SCons.Util

from SCons.Script import *
 
class ToolNDKWarning(SCons.Warnings.Warning):
    pass

class NDKNotFound(ToolNDKWarning):
    pass

SCons.Warnings.enableWarningClass(ToolNDKWarning)

def _detect(env):
    """Detect the Android NDK"""
    try: 
        return env['NDK_PATH']
    except KeyError: 
        pass
    ndk = env.WhereIs('ndk-build', os.environ['PATH'])
    if ndk:
        return ndk
    raise SCons.Errors.StopError(NDKNotFound,
                                 "Could not detect the Android NDK")
    return None

#
# Builders
#
_ndk_builder = SCons.Builder.Builder(
    action = SCons.Action.Action('$NDK_COM','$NDK_COMSTR'),
    suffix = '$NDK_SUFFIX',
    prefix = '$NDK_PREFIX',
    src_suffix = [ '$NDK_CCSUFFIX', '$NDK_CPPSUFFIX' ])

_ndk_copy_builder = SCons.Builder.Builder(
    action = SCons.Action.Action('$NDK_COPY_COM','$NDK_COPY_COMSTR'),
    suffix = '$NDK_SUFFIX',
    prefix = '$NDK_PREFIX',
    src_suffix = [ '$NDK_CCSUFFIX', '$NDK_CPPSUFFIX' ])

def generate_makefile(env, target, source, includes, libs, libdirs):

    path = os.path.dirname(Dir(target[0]).abspath)
    lib = os.path.basename(Dir(target[0]).abspath)
    src = [ '../' + str(s) for s in source ]
    incs = [ path + '/' + str(i) for i in includes ]
    libs = [ '-l' + str(l) for l in libs ]
    libdirs = [ '-L' + path + '/' + str(l) for l in libdirs ]

    prefix = os.path.dirname(_detect(env))

    incs += [ prefix + '/sources/cxx-stl/gnu-libstdc++/include',
              prefix + '/sources/cxx-stl/gnu-libstdc++/libs/armeabi/include',
              prefix + '/sources/cxx-stl/gnu-libstdc++/4.4.3/include',
              prefix + '/sources/cxx-stl/gnu-libstdc++/4.4.3/libs/armeabi/include' ]

    makefile = [ '# Automatically generated',
                 '# Do not edit!',
                 'LOCAL_PATH := $(call my-dir)',
                 'include $(CLEAR_VARS)',
                 'LOCAL_C_INCLUDES := \\',
                 ' '.join(incs),
                 'LOCAL_MODULE := ' + lib,
                 'LOCAL_SRC_FILES := \\',
                 ' '.join(src),
                 'LOCAL_LDLIBS := -L$(SYSROOT)/usr/lib -llog \\',
                 ' '.join(libdirs) + ' \\',
                 ' '.join(libs),
                 'LOCAL_CPPFLAGS := -fexceptions',
                 'include $(BUILD_SHARED_LIBRARY)' ]

    try:
        os.makedirs(path + '/jni')
    except:
        pass
    
    f = open(path + '/jni/Android.mk', 'w')
    f.write('\n'.join(makefile))
    f.write('\n')
    f.close()

    return 0

def AndroidNDK(env, target, source=None, *args, **kw):
    """Android NDK builder"""

    if not SCons.Util.is_List(target):
        target = [target]
    if not source:
        source = target[:]
    if not SCons.Util.is_List(source):
        source = [source]

    if 'INCLUDES' in kw:
        includes = kw['INCLUDES']
    else:
        includes = []

    if 'LIBS' in kw:
        libs = kw['LIBS']
    else:
        libs = []

    if 'LIBDIRS' in kw:
        libdirs = kw['LIBDIRS']
    else:
        libdirs = []

    kw['NDK_PROJECT_PATH'] = os.path.dirname(Dir(target[0]).abspath)

    generate_makefile(env, target, source, includes, libs, libdirs)

    lib = kw['NDK_PROJECT_PATH'] + '/libs/armeabi/lib' + target[0] + '.so'

    result = []
    rc = _ndk_builder.__call__(env, lib, source, **kw)
    result.extend(rc)
    rc = _ndk_copy_builder.__call__(env, target, lib, **kw)
    result.extend(rc)

    return result

def generate(env):
    """Add Builders and construction variables to the Environment."""
    env['NDK_PATH'] = _detect(env)
    env.SetDefault(
        NDK_SUFFIX = '.so',
        NDK_PREFIX = 'lib',
        NDK_CCSUFFIX = '.c',
        NDK_CPPSUFFIX = '.cpp',
        NDK_HSUFFIX = '.h',
        NDK_OPTS = '', #'V=1 -B',
        NDK_COM = 'NDK_PROJECT_PATH=$NDK_PROJECT_PATH $NDK_PATH $NDK_OPTS',
        NDK_COMSTR = '',
        NDK_COPY_COM = 'cp $SOURCE $TARGET',
        NDK_COPY_COMSTR = '',
        )
    env.AddMethod(AndroidNDK, "NDK")
    env['BUILDERS']['NDKbuild'] = _ndk_builder

def exists(env):
    return _detect(env)
