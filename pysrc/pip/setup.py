#!/usr/bin/python
#
# distutils setup.py file for building python pip module
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#

from distutils.core import setup, Extension
pipmodule = Extension('pip',
                      define_macros = [('MAJOR_VERSION', '1'),
                                       ('MINOR_VERSION', '0')],
                      sources = ['pipwrapper.c',
                                 'parse_params.c'])

setup (name = 'pip',
       version = '1.0',
       description = 'Python module for PIP',
       author = 'Tor Mohling',
       author_email = 'tor@bio3d.colorado.edu',
       maintainer = 'David Mastronarde',
       maintainer_email = 'mast@colorado.edu',
       url = 'http://bio3d.colorado.edu/imod',
       long_description = '''
Python module for PIP parameter parsing interface
''',
       ext_modules = [pipmodule])
