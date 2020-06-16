"""
An implementation of Arrow[1] in Python.

[1]: https://github.com/jacob-g/arrow-lang
"""
from __future__ import annotations
from .parser import ArrowParser
from .runner import ArrowRunner

__version__ = '0.0.0'
__all__ = ['run', 'ArrowRunner', 'ArrowParser']

def run(source: str, **kwargs) -> None:
    """Run Arrow ``source``."""
    ArrowRunner(source).run(**kwargs)
