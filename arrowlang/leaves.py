"""Leaves of the parse tree."""
from __future__ import annotations
import re
import json # just for string representation
from enum import Enum
from typing import Optional, Union, List, Type, get_type_hints

class MemEnum(Enum):
    """Adds a method to enums to check if a value is an enum value."""

    @classmethod
    def has(cls, value: str) -> bool:
        """Returns True if the value of an enum == value."""
        values = set(item.value for item in cls)
        return value in values

    def __str__(self):
        return self.value

class Operator(MemEnum):
    """Operators ABC."""
    pass

class BinaryOperator(Operator):
    """Binary operators."""
    EQUAL = '=='
    INEQUAL = '!='
    LESS = '<'
    MORE = '>'
    PLUS = '+'
    MINUS = '-'
    TIMES = '*'
    DIVIDE = '/'
    MODULO = '%'
    AND = 'and'
    OR = 'or'

BINOPS = re.compile(r'(==|!=|<|>|\+|-|\*|\/|%|and\b|or\b)')
EQUALS = re.compile(r'(?<![=!])=(?!=)')

class UnaryOperator(Operator):
    """Unary operators."""
    NEG = '-'
    POS = '+'
    NOT = 'not'
    LEN = 'length'

UNOPS = re.compile(r'(\+|-|not\b|length\b)')

ARRREG = re.compile(r'\s*\[\]')

PRECEDENCE = {
    # These are arbitrary numbers. They are only compared.
    # The spacing of 10 is in case more operators are added.
    UnaryOperator.LEN: 80,

    UnaryOperator.POS: 70,
    UnaryOperator.NEG: 70,

    BinaryOperator.TIMES: 60,
    BinaryOperator.DIVIDE: 60,
    BinaryOperator.MODULO: 60,

    BinaryOperator.PLUS: 50,
    BinaryOperator.MINUS: 50,

    BinaryOperator.LESS: 40,
    BinaryOperator.MORE: 40,
    BinaryOperator.INEQUAL: 40,
    BinaryOperator.EQUAL: 40,

    UnaryOperator.NOT: 30,
    BinaryOperator.AND: 20,
    BinaryOperator.OR: 10,
}

class VarType(MemEnum):
    """Variable types."""
    BOOL = 'bool'
    INT = 'int'
    CHAR = 'char'

    def __hash__(self):
        return hash(str(self))

    def __eq__(self, other):
        return hash(self) == hash(other)

BOOLOPS = {BinaryOperator.AND, BinaryOperator.OR, UnaryOperator.NOT}
INTOPS = {BinaryOperator.LESS, BinaryOperator.MORE, BinaryOperator.PLUS,
          BinaryOperator.MINUS, BinaryOperator.TIMES, BinaryOperator.DIVIDE,
          BinaryOperator.MODULO, UnaryOperator.NEG, UnaryOperator.POS}
SAMEOPS = {BinaryOperator.EQUAL, BinaryOperator.INEQUAL}
ARROPS = {UnaryOperator.LEN}

RETURNOPS = {
    VarType.BOOL: {
        BinaryOperator.EQUAL, BinaryOperator.INEQUAL, BinaryOperator.LESS,
        BinaryOperator.MORE, BinaryOperator.AND, BinaryOperator.OR,
        UnaryOperator.NOT
    },
    VarType.INT: {
        BinaryOperator.PLUS, BinaryOperator.MINUS, BinaryOperator.TIMES,
        BinaryOperator.DIVIDE, BinaryOperator.MODULO, UnaryOperator.NEG,
        UnaryOperator.POS, UnaryOperator.LEN
    }
}
OPRETURNS = {_op: _ret for _ret, _ops in RETURNOPS.items() for _op in _ops}

def _dataclass(cls: type) -> type:
    """Decorator to add kwarg-only __init__"""
    def typingcheck(value, typ):
        try:
            #if typ is Any:
            #    return True
            if getattr(typ, '__origin__', None) is Union:
                typ = typ.__args__
            if getattr(typ, '__origin__', None) is type:
                return issubclass(value, typ.__args__)
            return isinstance(value, typ)
        except TypeError:
            print(value, typ)
            raise
    def __init__(self, **kwargs):
        cls = type(self)
        cls.__types__ = get_type_hints(cls)
        for var in cls.__types__:
            if cls.__types__[var] is None:
                cls.__types__[var] = type(None)
            elif getattr(cls.__types__[var], '__origin__', None) is list:
                cls.__types__[var] = list
        for var, typ in self.__types__.items():
            if var not in kwargs and not hasattr(cls, var):
                raise TypeError('__init__() missing required '
                                f'keyword argument: {var!r}')
            elif var not in kwargs:
                kwargs[var] = getattr(cls, var)
            if not typingcheck(kwargs[var], typ):
                if isinstance(typ, tuple):
                    typ = ', '.join(f'{i.__name__!r}' for i in typ)
                else:
                    typ = f'{typ!r}'
                raise TypeError(f'{var!r} is not {typ}: {kwargs[var]!r}')
            setattr(self, var, kwargs[var])
    def __repr__(self):
        ret = type(self).__name__
        ret += '('
        ret += ', '.join(
            f'{k!s}={[...] if isinstance(v, list) else v!r}'
            for k, v in self.__dict__.items()
            if not k.startswith('__')
            and self.__types__.get(k, None) is not None
        )
        ret += ')'
        return ret
    def __setattr__(self, key, value):
        if key.startswith('__'):
            super(cls, self).__setattr__(key, value)
            return
        typ = self.__types__[key]
        if typ in (None, type(None)):
            super(cls, self).__setattr__(key, value)
            return
        if getattr(typ, '__origin__', None) is Union:
            typ = self.__types__[key] = typ.__args__
        if not typingcheck(value, typ):
            raise TypeError(f'{key!r} is not {typ.__name__!r}: {value!r}')
        super(cls, self).__setattr__(key, value)
    cls.__init__ = __init__
    cls.__repr__ = __repr__
    cls.__setattr__ = __setattr__
    cls.__hash__ = lambda self: hash(str(self))
    cls.__eq__ = lambda self, other: hash(self) == hash(other)
    return cls

@_dataclass
class ArrayType:
    """Array types."""
    # None means empty
    type: Union[None, VarType, 'ArrayType']

    def __str__(self):
        return str(self.type) + '[]'

# ABCs

@_dataclass
class Statement:
    """ABC for a single statement."""
    lineno: Optional[int] = None

class Expression(Statement):
    """ABC for expressions."""
    pass

# Possible kinds of expressions

class Variable(Expression):
    """Variables are expressions"""
    name: str

    def __str__(self):
        return self.name

class Number(Expression):
    """Numbers are expressions"""
    value: int

    def __str__(self):
        return str(self.value)

class Boolean(Expression):
    """Booleans are expressions"""
    value: bool

    def __str__(self):
        return str(self.value).lower()

class Character(Expression):
    """Character literals"""
    value: str

    def __str__(self):
        return repr(self.value)

class String(Expression):
    """String literals"""
    value: str

    def __str__(self):
        return json.dumps(self.value)

class Array(Expression):
    """Array literals"""
    values: List[Expression]

    def __str__(self):
        return '{' + ', '.join(map(str, self.values)) + '}'

class Operation(Expression):
    """Operation ABC."""
    operator: Operator

class BinaryOperation(Operation):
    """Binary operator"""
    arg1: Expression
    arg2: Expression
    operator: BinaryOperator

    def __str__(self):
        return f'({self.arg1!s} {self.operator.value} {self.arg2!s})'

class UnaryOperation(Operation):
    """Unary operator"""
    arg: Expression
    operator: UnaryOperator

    def __str__(self):
        return f'({self.operator.value}{" " if len(self.operator.value) != 1 else ""}{self.arg!s})'

class Call(Expression):
    """Function call"""
    name: Variable
    args: List[Expression]

    def __str__(self):
        return f'{self.name!s}({", ".join(map(str, self.args))})'

# subclasses Variable because it's
# an assignment target too
class Index(Variable):
    """Array indexing"""
    name: Expression
    index: Expression

    def __str__(self):
        return f'{self.name!s}[{self.index!s}]'

class Input(Expression):
    """Getting input from user"""
    type: Union[VarType, ArrayType]

    def __str__(self):
        return f'(input {self.type.value})'

# Possible kinds of statements

class Declaration(Statement):
    """Variable type declaration"""
    type: Union[VarType, ArrayType]
    name: Variable

    def __str__(self):
        return f'{self.type!s} {self.name!s}'

class Assignment(Statement):
    """Assigning variable value"""
    name: Variable
    value: Expression

    def __str__(self):
        return f'{self.name!s} = {self.value!s}'

class Require(Statement):
    """Strict assert"""
    expr: Expression

    def __str__(self):
        return f'require {self.expr!s}'

class Print(Statement):
    """Output"""
    values: List[Expression]

    def __str__(self):
        return 'print ' + ', '.join(map(str, self.values))

class Main(Statement):
    """Start of the program."""
    _instance = None
    def __new__(cls):
        """There is only one ``main`` statement."""
        if cls._instance is None:
            cls._instance = Statement.__new__(cls)
        return cls._instance

    def __str__(self):
        return 'main'

# Statements come together in blocks

@_dataclass
class Block:
    """Multiple statements (or blocks!)"""

    def _indent(self, body):
        return '\n'.join('| ' + line for line in str(body).splitlines())

class Body(Block):
    """Body of a block"""
    stmts: List[Union[Block, Statement]]

    def __str__(self):
        return '\n'.join(map(str, self.stmts))

class JumpDown(Block):
    r"""
    /--< condition
    | block
    \-->
    """
    condition: Optional[Expression]
    body: Body

    @property
    def lineno(self) -> int:
        """Line number of JumpDown is line number of its condition."""
        return self.body.stmts[0].lineno - 1

    def __str__(self):
        body = self._indent(self.body)
        return f'/--< {self.condition!s}\n{body!s}\n\\-->'

class JumpUp(Block):
    r"""
    /-->
    | block
    \--< condition
    """
    condition: Optional[Expression]
    body: Body

    @property
    def lineno(self) -> int:
        """Line number of JumpUp is line number of its condition."""
        return self.body.stmts[-1].lineno + 1

    def __str__(self):
        body = self._indent(self.body)
        return f'/-->\n{body!s}\n\\--> {self.condition!s}'

class Function(Block):
    """
    /--> type funcname(type params)
    | block
    ^ retval
    """
    rettype: Union[VarType, ArrayType]
    name: Variable
    params: List[Declaration] # abuse this type
    retval: Optional[Expression]
    body: Body

    @property
    def lineno(self) -> int:
        """Line number of Function is line number of the function keyword."""
        return self.body.stmts[0].lineno - 2

    def __str__(self):
        body = self._indent(self.body)
        return f'function\n/--> {self.rettype!s} {self.name!s}' \
               f'({", ".join(map(str, self.params))})' \
               f'\n{body!s}\n^ {self.retval!s}'

@_dataclass
class Program:
    """The root."""
    body: Body

    def __str__(self):
        return str(self.body)

@_dataclass
class End:
    """Marks the end of a block."""
    cls: Type[Block]
    value: Optional[Expression]
