"""
SCons Builder for building a Common Lisp ASDF package
"""

import os

import SCons.Action
import SCons.Builder
import SCons.Util

from SCons.Script import *
 
class ToolASDFWarning(SCons.Warnings.Warning):
    pass

class ASDFNotFound(ToolASDFWarning):
    pass

SCons.Warnings.enableWarningClass(ToolASDFWarning)

def gen_asdf(target, source, env):
    """Generate Common Lisp ASDF builder"""

    if not SCons.Util.is_List(target):
        target = [target]
    if not source:
        source = target[:]
    if not SCons.Util.is_List(source):
        source = [source]

    output = [ ';;;; %s' % os.path.basename(str(target[0])),
               ';;;; Automatically generated.  Do not edit.',
               '',
               '(asdf:defsystem #:%s' % env['ASDF_NAME'],
               '  :serial t',
               '  :description "%s"' % env['ASDF_DESCRIPTION'],
               '  :author "%s"' % env['ASDF_AUTHOR'],
               '  :license "%s"' % env['ASDF_LICENSE'] ]

    if 'ASDF_DEPENDS' in env:
        output += [ '  :depends-on (' ]
        for d in env['ASDF_DEPENDS']:
            output += [ '      #:%s' % d ]
        output += [ '  )' ]

    output += [ '  :components (' ]

    for s in source:
        output += [ '    (:file "%s")' % os.path.basename(str(s)).split('.lisp')[0] ]

    output += [ '  ))' ]

    f = open(str(target[0]), 'w')
    f.write('\n'.join(output))
    f.write('\n')
    f.close()

    return 0

def generate(env):
     env.Append(BUILDERS = {
             'GenASDF': env.Builder(
                 action = SCons.Action.Action(gen_asdf,
                                              "generating '$TARGET'"),
                 target_factory = env.fs.File,
                 )
             })

def exists(env):
    return True
