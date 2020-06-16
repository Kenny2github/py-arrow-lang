"""Typechecker and runner for Arrow."""
from typing import Optional, Union, Mapping, Tuple, Type, Any
from .leaves import (
    Program, Main, Function, Declaration, VarType, ArrayType, JumpDown,
    BOOLOPS, INTOPS, SAMEOPS, ARROPS, OPRETURNS, Assignment, JumpUp, Block,
    Require, Print, Expression, Index, Number, Boolean, Character, String,
    Variable, Array, Operation, Call, BinaryOperation, UnaryOperation, Input,
)
from .parser import ArrowParser

class TypecheckFailed(TypeError):
    """Typechecking failed."""
    def __init__(self, message, lineno, line):
        super().__init__(f'(text) line {lineno}: {line.strip()}\n{message}')
        self.lineno = lineno
        self.line = line

class ArrowError(RuntimeError):
    """Typechecking failed."""
    def __init__(self, message, lineno, line):
        super().__init__(f'(text) line {lineno}: {line.strip()}\n{message}')
        self.lineno = lineno
        self.line = line

class ArrowTyping:
    """Typecheck Arrow code."""

    source: Optional[Program] = None

    # same name can be reused in function,
    # so record types per function (and main)
    vartypes: Mapping[Union[Main, Function],
                      Mapping[Variable, VarType]]
    # map function name to function obj
    functions: Mapping[Variable, Function]

    def __init__(self, source: Optional[Program] = None):
        """Initialize the typechecker, optionally with source."""
        self.source = source
        self.vartypes = {Main(): {}}
        self.functions = {}

    def check(self, source: Optional[Program] = None) -> Tuple[
            Mapping[Union[Main, Function],
                    Mapping[Variable, VarType]],
            Mapping[Variable, Function]
        ]:
        """Typechecks the Arrow source.
        Raises TypeError if no text is given or set,
        or TypecheckFailed (which subclasses TypeError!)
        if typechecking found an error.
        """
        if source is None:
            if self.source is None:
                raise TypeError('No source in param or at init')
            source = self.source
        self.check_stmts(source)
        return (self.vartypes, self.functions)

    def check_stmts(self, block: Union[Program, Block],
                    func: Union[Main, Function] = Main()):
        """Recursively typecheck a block."""
        try:
            for stmt in block.body.stmts:
                if isinstance(stmt, Function):
                    assert not isinstance(block, Function), \
                           'Cannot have nested functions'
                    self.functions[stmt.name] = stmt
                    self.vartypes[stmt] = {}
                    # declare variables
                    for param in stmt.params:
                        self.set_vartype(stmt, param)
                    self.check_stmts(stmt, stmt)
                    self.check_expr(stmt, stmt.retval, stmt.rettype)
                elif isinstance(stmt, Block):
                    # CONFIRM: with jvvg whether there's truthiness
                    self.check_expr(func, stmt.condition, VarType.BOOL)
                    # recursively check that block
                    self.check_stmts(stmt, func)
                elif isinstance(stmt, Declaration):
                    # type varname
                    self.set_vartype(func, stmt)
                elif isinstance(stmt, Assignment):
                    # var = value
                    self.check_expr(func, stmt.value,
                                    self.get_vartype(func, stmt.name))
                elif isinstance(stmt, Require):
                    # CONFIRM: with jvvg whether there's truthiness
                    self.check_expr(func, stmt.expr, VarType.BOOL)
                elif isinstance(stmt, Print):
                    # print expr, expr, expr
                    for expr in stmt.values:
                        self.check_expr(func, expr, None)
                elif isinstance(stmt, Main):
                    pass
                else:
                    self.check_expr(func, stmt, None)
        except AssertionError as exc:
            raise TypecheckFailed(exc.args[0], stmt.lineno, str(stmt))

    def set_vartype(self, func: Union[Main, Function],
                    decl: Declaration):
        """Declare a variable's type.
        Raises AssertionError if already declared.
        """
        # setdefault 1 adds the new block to the mapping,
        # setdefault 2 will only set [decl.name] = decl
        # if decl.name was not already in the mapping.
        # if it was, the value will not is decl
        # and we raise hell. Otherwise it's already set.
        name = decl.name
        names = 0
        while isinstance(name, Index):
            name = name.name
            names += 1
        typ = decl.type
        types = 0
        while isinstance(typ, ArrayType):
            typ = typ.type
            types += 1
        assert types >= names, f'{name!s} has too many dimensions declared ' \
               f'({names} > {types})'
        assert self.vartypes.setdefault(func, {})\
               .setdefault(name, decl) is decl, \
               f'{name!s} is already declared at line ' \
               f'{self.vartypes[func][name].lineno}'

    def get_vartype(self, func: Union[Main, Function],
                    name: Variable):
        """Return the type of a variable.
        Raises AssertionError if not declared.
        """
        if isinstance(name, Index):
            return self.get_vartype(func, name.name).type
        assert name in self.vartypes[func] or name in self.vartypes[Main()], \
               f'unknown variable {name!s}'
        return self.vartypes[func][name].type

    def check_expr(self, func: Union[Main, Function], expr: Expression,
                   typ: Union[None, type(...), VarType, Type[ArrayType]]=None) \
                   -> Union[None, VarType, ArrayType]:
        """Recursively check an expression's type.
        None means the result of the expression need not be typechecked
        but its components still do need it.
        """
        inst = typ is ArrayType
        if isinstance(expr, Index):
            self.check_expr(func, expr.index, VarType.INT)
            self.check_expr(func, expr.name, ArrayType)
        if isinstance(expr, Variable) and typ is not None:
            vartype = self.get_vartype(func, expr)
            if typ is ...:
                return vartype
            if inst:
                assert isinstance(vartype, typ), \
                       f'{expr!s} is {vartype!s}, not array'
            else:
                assert typ == vartype, \
                       f'{expr!s} is {vartype!s}, not {typ!s}'
        elif isinstance(expr, Number) and typ is not None:
            if typ is ...:
                return VarType.INT
            if inst:
                typ = 'array'
            assert typ == VarType.INT, f'{expr!s} is int, not {typ!s}'
        elif isinstance(expr, Boolean) and typ is not None:
            if typ is ...:
                return VarType.BOOL
            if inst:
                typ = 'array'
            assert typ == VarType.BOOL, f'{expr!s} is bool, not {typ!s}'
        elif isinstance(expr, Character) and typ is not None:
            if typ is ...:
                return VarType.CHAR
            if inst:
                typ = 'array'
            assert typ == VarType.CHAR, f'{expr!s} is char, not {typ!s}'
        elif isinstance(expr, String) and typ is not None:
            if typ is ...:
                return ArrayType(type=VarType.CHAR)
            if not inst: # strings are arrays
                assert typ == ArrayType(type=VarType.CHAR), \
                       f'{expr!s} is char[], not {typ!s}'
        elif isinstance(expr, Array) and typ is not None:
            if expr.values:
                itype = self.check_expr(func, expr.values[0], ...)
                for val in expr.values:
                    try:
                        self.check_expr(func, val, itype)
                    except AssertionError:
                        raise AssertionError('array must be '
                                             'all one type') from None
            if typ is ...:
                return ArrayType(type=itype) \
                       if expr.values \
                       else ArrayType(type=None)
            if not inst:
                assert typ == ArrayType(type=itype) \
                       if expr.values \
                       else isinstance(typ, ArrayType), \
                       f'{expr!s} is an array, not {typ!s}'
        elif isinstance(expr, Operation):
            opertype = self.check_oper(func, expr)
            if typ is ...:
                return opertype
            if typ is not None:
                if inst:
                    assert isinstance(opertype, typ), f'{expr!s} is ' \
                           f'{opertype!s}, not array'
                else:
                    assert typ == opertype, f'{expr!s} is {opertype!s}, ' \
                           f'not {typ!s}'
        elif isinstance(expr, Call):
            assert expr.name in self.functions, 'unknown function {expr.name!s}'
            called = self.functions[expr.name]
            if typ is ...:
                return called.rettype
            if typ is not None:
                if inst:
                    assert isinstance(called.rettype, typ), \
                           f'{called.name!s}() returns {called.rettype!s}, ' \
                           'not array'
                else:
                    assert typ == called.rettype, \
                           f'{called.name!s}() returns {called.rettype!s}, ' \
                           f'not {typ!s}'
            assert len(expr.args) == len(called.params), \
                   f'{called.name!s}() expects {len(called.params)} ' \
                   f'arguments, not {len(expr.args)}'
            for arg, param in zip(expr.args, called.params):
                self.check_expr(func, arg, param.type)
        elif isinstance(expr, Input) and typ is not None:
            if typ is ...:
                return expr.type
            if inst:
                assert isinstance(expr.type, typ), 'expected array but ' \
                       f'got {expr.type!s}'
            else:
                assert typ == expr.type, f'expected {typ!s} but got ' \
                       f'{expr.type!s}'
        elif typ is None:
            return None
        else:
            raise TypeError('How did you get here? Report this as a bug.')
        return None

    def check_oper(self, func: Union[Main, Function],
                   expr: Operation) -> VarType:
        """Check an operation's arguments depending on the operation."""
        if isinstance(expr, UnaryOperation):
            if expr.operator in INTOPS:
                self.check_expr(func, expr.arg, VarType.INT)
            elif expr.operator in BOOLOPS:
                # CONFIRM: with jvvg whether there's truthiness
                self.check_expr(func, expr.arg, VarType.BOOL)
            elif expr.operator in ARROPS:
                try:
                    self.check_expr(func, expr.arg, ArrayType)
                except AssertionError:
                    raise AssertionError('length x expected an array') from None
            else:
                raise TypeError('How did you get here? Report this as a bug.')
            return OPRETURNS[expr.operator]
        if expr.operator in INTOPS:
            self.check_expr(func, expr.arg1, VarType.INT)
            self.check_expr(func, expr.arg2, VarType.INT)
        elif expr.operator in BOOLOPS:
            self.check_expr(func, expr.arg1, VarType.BOOL)
            self.check_expr(func, expr.arg2, VarType.BOOL)
        elif expr.operator in SAMEOPS:
            typ1 = self.check_expr(func, expr.arg1, ...)
            typ2 = self.check_expr(func, expr.arg2, ...)
            assert typ1 == typ2, f'Operands to {expr.operator!s} ' \
                   'must be of same type'
        else:
            raise TypeError('How did you get here? Report this as a bug.')
        return OPRETURNS[expr.operator]

class ArrowRunner:
    """Run Arrow code."""

    source: Union[Program, str, None] = None
    # these two attributes come from those in ArrowTyping
    vartypes: Mapping[Union[Main, Function],
                      Mapping[Variable, VarType]]
    functions: Mapping[Variable, Function]
    # these are specific
    variables: Mapping[Union[Main, Function],
                       Mapping[Variable, Any]]

    def __init__(self, source: Union[Program, str, None] = None):
        """Initialize the runner, optionally with source."""
        self.source = source

    def run(self, source: Union[Program, str, None] = None) -> None:
        """Runs the Arrow source.
        Raises TypeError if no text is given or set.
        """
        if source is None:
            if self.source is None:
                raise TypeError('No source in param or at init')
            source = self.source
        else:
            self.source = source
        if isinstance(source, str):
            self.source = ArrowParser(self.source).parse()
        self.vartypes, self.functions = ArrowTyping(self.source).check()
        self.variables = {}
        self.run_func(self.source, Main())

    def run_func(self, block: Union[Program, Block],
                 func: Union[Main, Function]):
        """Call a function."""
        # a key thing to note here is that it's expected that
        # the typechecker has already been run.
        # This means there is a lot less validation as to
        # whether the variable should or shouldn't exist, for
        # example - the typechecker should have caught that.
        # For example, the Assignment branch assumes a Declaration
        # has already been encountered, since for it to have passed
        # the typechecker it in fact has to have been.
        try:
            for stmt in block.body.stmts:
                if isinstance(stmt, Function) and isinstance(func, Main):
                    self.variables[stmt] = {}
                    continue
                if stmt is Main():
                    continue
                if isinstance(stmt, JumpDown):
                    if not self.run_expr(stmt.condition, func):
                        self.run_func(stmt, func)
                elif isinstance(stmt, JumpUp):
                    self.run_func(stmt, func)
                    while self.run_expr(stmt.condition, func):
                        self.run_func(stmt, func)
                elif isinstance(stmt, Declaration):
                    self.variables.setdefault(func, {})
                    typ = stmt.type
                    while isinstance(typ, ArrayType):
                        typ = typ.type
                    ddims = []
                    name = stmt.name
                    while isinstance(name, Index):
                        ddims.append(name.index)
                        name = name.name
                    def copy(value: Union[list, None]) -> Union[list, None]:
                        """Shallow copy a list, do nothing to None."""
                        if value is None:
                            return None
                        return value[:]
                    bottom = None
                    while ddims:
                        bottom = [copy(bottom)
                                  for _ in range(self.run_expr(ddims.pop(),
                                                               func))]
                    self.variables.setdefault(func, {})[name] = bottom
                elif isinstance(stmt, Assignment):
                    name = stmt.name
                    values = []
                    while isinstance(name, Index):
                        values.append(name.index)
                        name = name.name
                    if values:
                        var = self.variables[func][name]
                        for idx in values[:-1]:
                            var = var[self.run_expr(idx, func)]
                        var[self.run_expr(values[-1], func)] \
                            = self.run_expr(stmt.value, func)
                    else:
                        self.variables[func][name] = self.run_expr(stmt.value,
                                                                   func)
                elif isinstance(stmt, Require):
                    if not self.run_expr(stmt.expr, func):
                        # don't use assert because require is *strict* assert
                        raise AssertionError('require statement failed')
                elif isinstance(stmt, Print):
                    values = []
                    for val in stmt.values:
                        val = self.run_expr(val, func)
                        if isinstance(val, list) and isinstance(val[0], str):
                            val = ''.join(val)
                        values.append(val)
                    print(*values)
        except Exception as exc:
            raise ArrowError(exc.args[0], stmt.lineno, str(stmt))

    def run_expr(self, expr: Expression, func: Union[Main, Function]) -> Any:
        """Evaluate and return the semantic value of an expression."""
        if isinstance(expr, Variable):
            name = expr
            idxs = []
            while isinstance(name, Index):
                idxs.append(name.index)
                name = name.name
            var = self.variables[func][name]
            for idx in idxs:
                assert var is not None, 'using uninitialized array'
                var = var[self.run_expr(idx, func)]
            return var
        if isinstance(expr, (Number, Boolean, Character)):
            return expr.value
        if isinstance(expr, String):
            return list(expr.value)
        if isinstance(expr, Array):
            return [self.run_expr(i, func) for i in expr.values]
        if isinstance(expr, BinaryOperation):
            return self.run_binop(expr, func)
        if isinstance(expr, UnaryOperation):
            return self.run_unop(expr, func)
        if isinstance(expr, Call):
            return self.call(expr, func)
        if isinstance(expr, Input):
            inp = input()
            if expr.type == VarType.INT:
                return int(inp)
            if expr.type == ArrayType(type=VarType.CHAR):
                return list(inp)
            raise TypeError('How did you get here? Report this as a bug.')
        raise TypeError('How did you get here? Report this as a bug.')

    def run_binop(self, expr: BinaryOperation,
                  func: Union[Main, Function]) -> Any:
        """Evaluate and return the semantic value of a binary operation."""
        arg1 = self.run_expr(expr.arg1, func)
        arg2 = self.run_expr(expr.arg2, func)
        oper = str(expr.operator)
        if oper == '+':
            return arg1 + arg2
        if oper == '-':
            return arg1 - arg2
        if oper == '*':
            return arg1 * arg2
        if oper == '/':
            return arg1 // arg2 # arrow division is integer division
        if oper == '%':
            return arg1 % arg2
        if oper == '==':
            return arg1 == arg2
        if oper == '!=':
            return arg1 != arg2
        if oper == '<':
            return arg1 < arg2
        if oper == '>':
            return arg1 > arg2
        if oper == 'and':
            return arg1 and arg2
        if oper == 'or':
            return arg1 or arg2
        raise TypeError('How did you get here? Report this as a bug.')

    def run_unop(self, expr: UnaryOperation,
                 func: Union[Main, Function]) -> Any:
        """Evaluate and return the semantic value of a unary operation."""
        arg = self.run_expr(expr.arg, func)
        oper = str(expr.operator)
        # pylint doesn't seem to realize that arg can be int
        # pylint: disable=invalid-unary-operand-type
        if oper == '-':
            return -arg
        if oper == '+':
            return +arg
        if oper == 'not':
            return not arg
        if oper == 'length':
            return len(arg)
        # pylint: enable=invalid-unary-operand-type
        raise TypeError('How did you get here? Report this as a bug.')

    def call(self, expr: Call, func: Union[Main, Function]) -> Any:
        """Call a function."""
        called = self.functions[expr.name]
        for param, value in zip(called.params, expr.args):
            name = param.name
            while isinstance(name, Index):
                name = name.name
            self.variables[called][name] = self.run_expr(value, func)
        self.run_func(called, called)
        return self.run_expr(called.retval, called)
