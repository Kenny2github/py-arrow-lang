py-arrow-lang
-------------

An implementation of `Arrow <https://github.com/jacob-g/arrow-lang>`_ in Python.

Running Arrow code
==================

.. code-block:: text

    $ arrow --help

    usage: arrow [-h] [file]

    positional arguments:
      file        A file to run. Use - for stdin without prompt.

    optional arguments:
      -h, --help  show this help message and exit

Example Arrow program
=====================

.. code-block:: text

    function
    /--> int factorial(int n)
    | require not (n < 0)
    | int return
    | /--< n != 0
    | | return = 1
    | \-->
    | /--< n == 0
    | | return = n * factorial(n - 1)
    | \-->
    ^ return

    main
    int n
    print "Enter number:"
    n = input int
    print "Factorial of", n, "is", factorial(n)

Which outputs:

.. code-block:: text

    Enter number:
    5
    Factorial of 5 is 120
