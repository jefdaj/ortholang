#!/usr/bin/env python

from setuptools import setup

setup(
  name = 'bblast',
  version = '0.1',
  packages = ['bblast'],
  entry_points = {'console_scripts': ['bblast=bblast:main']}
)
