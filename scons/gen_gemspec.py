"""
SCons Builder for building a Ruby gemspec file
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

def gen_gemspec(target, source, env):
    """Generate Ruby gempsec builder"""

    if not SCons.Util.is_List(target):
        target = [target]
    if not source:
        source = target[:]
    if not SCons.Util.is_List(source):
        source = [source]

    gem = [ 'Gem::Specification.new do |s|' ]

    for i in [ 'NAME', 'VERSION', 'SUMMARY', 'DESCRIPTION', 'EMAIL',
               'HOMEPAGE' ]:
        try:
            var = i.lower()
            value = env['GEM_' + i.upper()]
            gem += [ '  s.%s = "%s"' % (var,value) ]
        except:
            pass

    try:
        authors = env['GEM_AUTHORS']
        if type(authors).__name__ == 'str':
            authors = [ authors ]
        line = '  s.authors = [ '
        for a in authors:
            line = '%s"%s",' % (line,a)
        line = '%s]' % line
        gem += [ line ]
    except:
        pass

    line = '  s.files = [ '
    for s in source:
        if 'GEM_DIR' in env:
            line = '%s"%s",' % (line,str(s).split(env['GEM_DIR'] + '/')[1])
        else:
            line = '%s"%s",' % (line,s)
    line = '%s]' % line
    gem += [ line ]

    gem += [ 'end' ]

    f = open(str(target[0]), 'w')
    f.write('\n'.join(gem))
    f.write('\n')
    f.close()

    return 0

def generate(env):
     env.Append(BUILDERS = {
             'GenGemspec': env.Builder(
                 action = SCons.Action.Action(gen_gemspec,
                                              "generating '$TARGET'"),
                 target_factory = env.fs.File,
                 )
             })

def exists(env):
    return True
