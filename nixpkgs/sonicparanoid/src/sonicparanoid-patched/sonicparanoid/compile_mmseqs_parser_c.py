# In order to compile execute
# python3 compile_mmseqs_parser_c.py build_ext --inplace

from distutils.core import setup
from Cython.Build import cythonize

setup(ext_modules = cythonize('mmseqs_parser_c.pyx', compiler_directives={'language_level': 3}))
