"""
SCons Builder for building a Python setup file
"""

import SCons.Action
import SCons.Builder
import SCons.Util

from SCons.Script import *
 
class ToolGemspecWarning(SCons.Warnings.Warning):
    pass

class GemspecNotFound(ToolGemspecWarning):
    pass

SCons.Warnings.enableWarningClass(ToolGemspecWarning)

def gen_setup(target, source, env):
    """Generate Python setup builder"""

    if not SCons.Util.is_List(target):
        target = [target]
    if not source:
        source = target[:]
    if not SCons.Util.is_List(source):
        source = [source]

    output = [ 'from distutils.core import setup',
               '',
               'setup(' ]

    for i in [ 'NAME', 'VERSION', 'AUTHOR', 'AUTHOR_EMAIL', 'LICENSE',
               'URL', 'DESCRIPTION' ]:
        try:
            var = i.lower()
            value = env['SETUP_' + i.upper()]
            output += [ '    %s = "%s",' % (var,value) ]
        except:
            pass

    files = '[ '
    for s in source:
        if 'SETUP_DIR' in env:
            files = '%s"%s",' % (files,str(s).split(env['SETUP_DIR'] + '/')[1])
        else:
            files = '%s"%s",' % (files,s)
    files += ' ]'

    directory = '"."'

    output += [ '    packages = [ "%s" ],' % env['SETUP_NAME'],
                '    package_dir = { "%s" : %s },' % (env['SETUP_NAME'],
                                                      directory),
                '    package_data = { "%s" : %s },' % (env['SETUP_NAME'],
                                                       files) ]

    output += [ ')' ]

    f = open(str(target[0]), 'w')
    f.write('\n'.join(output))
    f.write('\n')
    f.close()

    return 0

def generate(env):
     env.Append(BUILDERS = {
             'GenSetup': env.Builder(
                 action = SCons.Action.Action(gen_setup,
                                              "generating '$TARGET'"),
                 target_factory = env.fs.File,
                 )
             })

def exists(env):
    return True
