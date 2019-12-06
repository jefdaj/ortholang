# In order to compile execute
# python3 compile_inpyranoid_c.py build_ext --inplace

from distutils.core import setup
from Cython.Build import cythonize

setup(ext_modules = cythonize('inpyranoid_c.pyx', compiler_directives={'language_level': 3}))
