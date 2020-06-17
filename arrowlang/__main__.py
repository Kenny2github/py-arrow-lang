"""For running `python -m arrowlang`."""
import sys
import argparse
from arrowlang import run

def main(args=None):
    """The entry point of the module."""
    argparser = argparse.ArgumentParser()
    argparser.add_argument('file', nargs='?', help='A file to run. '
                           'Use - for stdin without prompt.')
    args = argparser.parse_args(args)
    if args.file:
        if args.file == '-':
            source = sys.stdin.read()
        else:
            with open(args.file) as sfile:
                source = sfile.read()
    else:
        print('Enter Arrow program. Type ^D (Linux) or ^Z then Enter (Windows)'
              ' to finish.')
        source = sys.stdin.read()
    run(source)

if __name__ == '__main__':
    main()
