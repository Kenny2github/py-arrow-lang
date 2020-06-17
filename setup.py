from setuptools import setup

with open('README.rst') as f:
    long_description = f.read()

setup(name='py-arrow-lang',
      version='2020.06.17',
      description='An implementation of Arrow ' \
      '(https://github.com/jacob-g/arrow-lang) in Python.',
      long_description=long_description,
      classifiers=[
          'Development Status :: 3 - Alpha',
          'License :: OSI Approved :: MIT License',
          'Programming Language :: Python :: 3.7',
          'Topic :: Software Development :: Interpreters',
      ],
      keywords='arrow language interpreter',
      url='https://github.com/Kenny2github/py-arrow-lang',
      author='Kenny2github',
      author_email='kenny2minecraft@gmail.com',
      license='MIT',
      packages=['arrowlang'],
      entry_points={
          'console_scripts': [
              'arrow=arrowlang.__main__:main',
          ],
      },
)
