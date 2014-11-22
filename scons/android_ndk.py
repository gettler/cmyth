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

def generate_makefile(env, target, source, includes, libs, libdirs, abi):
    """Generate Android NDK makefiles"""

    path = os.path.dirname(Dir(target).abspath)
    libname = os.path.basename(Dir(target).abspath)
    src = [ '../' + str(s) for s in source ]
    incs = [ path + '/' + str(i) for i in includes ]
    local_libs = [ str(l) for l in libs ]
    local_src = [ str(d) + '/libs/$(TARGET_ARCH_ABI)/' + str(l) for d,l in zip(libdirs,libs) ]
    libdirs = [ '-L' + os.path.join(path, str(l), 'libs', abi) for l in libdirs ]

    prefix = os.path.dirname(_detect(env))

    prebuilt = []

    for lib in libs:
        libdir = 'lib%s' % lib
        library = 'lib%s.so' % lib
        prebuilt += [
            'include $(CLEAR_VARS)',
            'LOCAL_MODULE := %s' % lib,
            'LOCAL_SRC_FILES := ../../%s/libs/$(TARGET_ARCH_ABI)/%s' % (libdir,library),
            'include $(PREBUILT_SHARED_LIBRARY)',
            '' ]

    android = [ '# Automatically generated',
                 '# Do not edit!',
                 'LOCAL_PATH := $(call my-dir)',
                 '',
                 '\n'.join(prebuilt),
                 '',
                 'include $(CLEAR_VARS)',
                 'LOCAL_C_INCLUDES := %s' % ' '.join(incs),
                 'LOCAL_MODULE := %s' % libname,
                 'LOCAL_SRC_FILES := %s'% ' '.join(src),
                 'LOCAL_LDLIBS := -L$(SYSROOT)/usr/lib -llog',
                 'LOCAL_CPPFLAGS := -fexceptions',
                 'LOCAL_SHARED_LIBRARIES := %s' % ' '.join(local_libs),
                 'include $(BUILD_SHARED_LIBRARY)' ]

    app = [ '# Automatically generated',
            '# Do not edit!',
            'APP_ABI := %s' % abi,
            'APP_STL := gnustl_static',
            'APP_CPPFLAGS += -fexceptions -std=c++11',
            'LOCAL_SRC_FILES := %s' % ' '.join(local_src) ]

    try:
        os.makedirs(path + '/jni')
    except:
        pass
    
    android_mk = '%s/jni/Android.mk' % path
    f = open(android_mk, 'w')
    f.write('\n'.join(android))
    f.write('\n')
    f.close()

    application_mk = '%s/jni/Application.mk' % path
    f = open(application_mk, 'w')
    f.write('\n'.join(app))
    f.write('\n')
    f.close()

    return [ File(android_mk), File(application_mk) ]

def AndroidNDK(env, target, source=None, *args, **kw):
    """Android NDK builder"""

    if SCons.Util.is_List(target):
        if len(target) > 1:
            SCons.Errors.StopError('Android NDK builder only supports a single target!')
        target = target[0]

    if not source:
        source = target
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

    if 'ABI' in kw:
        abi = kw['ABI']
    else:
        abi = 'armeabi'

    kw['NDK_PROJECT_PATH'] = os.path.dirname(Dir(target).abspath)
    lib = kw['NDK_PROJECT_PATH'] + '/libs/' + abi + '/lib' + target + '.so'

    result = []
    rc = generate_makefile(env, target, source, includes, libs, libdirs, abi)
    result.extend(rc)
    rc = _ndk_builder.__call__(env, lib, source, **kw)
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
