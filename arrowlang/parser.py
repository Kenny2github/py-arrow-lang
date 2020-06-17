"""Lexer for Arrow."""
from typing import Optional, Union, Tuple, List
from .leaves import (
    Block, Body, Program, JumpUp, JumpDown, Function, End,
    Expression, Statement, Number, Boolean, Variable, String,
    Character, Array, BinaryOperator, UnaryOperator, BinaryOperation,
    UnaryOperation, Call, Index, Main, Operator, Assignment,
    PRECEDENCE, UNOPS, BINOPS, EQUALS, ARRREG, VarType, ArrayType,
    Input, Print, Declaration, Require
)

__all__ = ['ParsingFailed', 'ArrowParser']

class ParsingFailed(ValueError):
    """Parsing Arrow source failed."""
    def __init__(self, message, lineno, line):
        super().__init__(f'(text) line {lineno}: {line.strip()}\n{message}')
        self.lineno = lineno
        self.line = line

class ArrowParser:
    """Parse Arrow source."""

    source: Optional[str] = None

    def __init__(self, source: Optional[str] = None):
        """Initialize the parser, optionally with source."""
        self.source = source

    def parse(self, source: Optional[str] = None) -> Program:
        """Returns a Program representing the entire program.
        Raise ParsingFailed if parsing fails
        or TypeError if no text is given or set.
        """
        if source is None:
            if self.source is None:
                raise TypeError('No source in param or at init')
            source = self.source
        source = source.replace('\\\n', '')
        result = Program(body=Body(stmts=[]))
        stack = [result]
        mained = False
        try:
            for lineno, line in enumerate(source.splitlines()):
                statement = self.parse_line(line, len(stack))
                if statement is None:
                    continue
                if isinstance(statement, Block):
                    stack[-1].body.stmts.append(statement)
                    stack.append(statement)
                elif isinstance(statement, End):
                    if not isinstance(stack[-1], statement.cls):
                        raise AssertionError(
                            'Expecting end to {.__name__} '
                            'but got end to {.__name__} '
                            'instead'.format(
                                type(stack[-1]),
                                statement.cls
                            )
                        )
                    if statement.cls is JumpUp:
                        stack[-1].condition = statement.value
                    elif statement.cls is Function:
                        stack[-1].retval = statement.value
                    stack.pop()
                else:
                    stack[-1].body.stmts.append(statement)
                    if statement is Main():
                        assert not mained, 'Cannot have more than one main'
                        mained = True
                    statement.lineno = lineno + 1 # used in runner for errors
        except AssertionError as exc:
            raise ParsingFailed(exc.args[0], lineno + 1, line)
        return result

    def parse_line(self, line: str, stacklen: int) -> Union[
            None, Statement, Block, End
        ]:
        """Returns a single Statement,
        which can be None if the line was empty or a comment.
        """
        line = line.split('//', 1)[0].strip()
        if not line.lstrip('| \t'):
            return None # empty after removing comments
        # these are not affected by indents - they
        # must always be at the top level
        if line == 'function':
            # this line can be ignored for parsing purposes
            return None
        if line == 'main':
            return Main()
        try:
            # program               stacklen    start
            # /-->                  1           /-->
            # | /--< condition1     2           | /--<
            # | | /-->              3           | /-->
            # | | | do_thing        4           | do_thing
            # | | \--< condition2   4           \--<
            # | \-->                3           \-->
            # \--< condition3       2           \--<
            # conclusion: endings have no extra indents,
            # startings do, except on top level
            start = self.indent(0, line, stacklen)
            start, statement = self.statement(start, line, stacklen)
            return statement
        except IndexError:
            raise AssertionError('unexpected EOL')

    def whitespace(self, start: int, text: str, required: bool = False) -> int:
        """Skip past whitespace."""
        end = start
        try:
            while text[end].isspace():
                end += 1
        except IndexError:
            end = len(text)
        assert not (required and end == start), \
               f'column {start}: required whitespace not present'
        return end

    def indent(self, start: int, text: str, stacklen: int) -> int:
        """Skip past (and require) indentation."""
        # - 1 for root, - 1 for current level
        needed = max(stacklen - 2, 0)
        while needed > 0:
            assert text[start] in r'\|' or text[start].isspace(), \
                   f'Missing {needed} indent(s)'
            if text[start] == '|':
                needed -= 1
            start += 1
        start = self.whitespace(start, text)
        #if start == len(text):
        #    raise IndexError
        return start

    def statement(self, start: int, text: str,
                  stacklen: int) -> Tuple[int, Union[None, Statement,
                                                     Block, End]]:
        """Parse a statement."""
        arg = None
        if text.startswith(r'\--<', start):
            # end JumpUp and parse condition
            start = self.whitespace(start + 4, text)
            start, cond = self.expression(start, text)
            arg = End(cls=JumpUp, value=cond)
        elif text.startswith(r'\-->', start):
            # end JumpDown and None condition
            start += 4
            arg = End(cls=JumpDown, value=None)
        elif text.startswith('^', start):
            start = self.whitespace(start + 1, text)
            start, retval = self.expression(start, text)
            arg = End(cls=Function, value=retval)
        if arg is not None:
            return (start, arg)
        arg = None
        if stacklen > 1:
            start = self.indent(start, text, 3) # chop off last indent
        if text.startswith('/--<', start):
            # start JumpDown and parse condition
            start = self.whitespace(start + 4, text)
            start, cond = self.expression(start, text)
            arg = JumpDown(body=Body(stmts=[]), condition=cond)
        elif text.startswith('/-->', start):
            # two possibilities - either start JumpUp or Function
            start = self.whitespace(start + 4, text)
            try:
                start, typ = self.type(start, text)
            except AssertionError:
                # no type encountered, it's JumpUp
                arg = JumpUp(condition=None, body=Body(stmts=[]))
            else:
                start = self.whitespace(start, text, True)
                start, name = self.variable(start, text)
                start, args = self.args(start + 1, text)
                arg = Function(rettype=typ, name=name, params=args,
                               retval=None, body=Body(stmts=[]))
        elif text.startswith('print', start):
            start = self.whitespace(start + 5, text, True)
            start, arg = self.printstmt(start, text)
        elif text.startswith('require', start):
            start = self.whitespace(start + 7, text, True)
            start, arg = self.expression(start, text)
            arg = Require(expr=arg)
        if arg is not None:
            return (start, arg)
        try:
            start, arg = self.declaration(start, text)
        except AssertionError:
            pass # not a declaration
        else:
            return (start, arg)
        match = EQUALS.search(text, start)
        if match:
            start, name = self.expression(start, text[:match.start()])
            assert isinstance(name, Variable), \
                   f'Expected assignment target before column {start}'
            start = self.whitespace(match.end(), text)
            start, value = self.expression(start, text)
            arg = Assignment(name=name, value=value)
        else:
            start, arg = self.expression(start, text)
        return (start, arg)

    def printstmt(self, start: int, text: str) -> Print:
        """Parse a print statement."""
        end, args = self.call(start, text + ')')
        return (end - 1, Print(values=args))

    def expression(self, start: int, text: str, paren: bool = False) \
            -> Tuple[int, Optional[Expression]]:
        """Parse an expression."""
        if not text.strip():
            return (start, None)
        start, tokens = self.tokenize(start, text, paren)
        value = self.precedence(tokens)
        return (start, value)

    def precedence(self, tokens: List[Union[Expression, Operator]]) \
            -> Expression:
        """Decide precedence by converting infix to RPN.

        Uses the shunting-yard algorithm:
        https://en.wikipedia.org/wiki/Shunting-yard_algorithm
        """
        if len(tokens) == 1:
            assert isinstance(tokens[0], Expression), 'Unexpected lone operator'
            # skip all the stack manipulation
            return tokens[0]
        output = []
        stack = []
        while tokens:
            token = tokens.pop(0)
            if isinstance(token, Expression):
                output.append(token)
            elif isinstance(token, Operator):
                try:
                    while (isinstance(stack[-1], Operator)
                           and (PRECEDENCE[stack[-1]] >= PRECEDENCE[token])):
                        output.append(stack.pop())
                except IndexError:
                    pass # stack is empty, no need to pop anything
                stack.append(token)
        while stack:
            # transfer rest of stack to output
            output.append(stack.pop())
        # construct (a op b)s from RPN
        stack.clear()
        for item in output:
            if isinstance(item, Expression):
                stack.append(item)
            elif isinstance(item, BinaryOperator):
                arg2 = stack.pop()
                arg1 = stack.pop()
                stack.append(BinaryOperation(arg1=arg1, arg2=arg2,
                                             operator=item))
            elif isinstance(item, UnaryOperator):
                stack.append(UnaryOperation(arg=stack.pop(), operator=item))
        assert len(stack) == 1, 'Something has gone wrong - report as bug'
        return stack[0]

    def tokenize(self, start: int, text: str, paren: bool = False) \
            -> Tuple[int, List[Union[Expression, Operator]]]:
        """Split expression string into tokenized expressions/operators."""
        #breakpoint()
        tokens = []
        while start < len(text):
            while start < len(text):
                # test unary operators, add them if valid
                match = UNOPS.match(text, start)
                try:
                    if match:
                        tokens.append(UnaryOperator(match.group(0)))
                        start += len(match.group(0))
                    else:
                        break
                finally:
                    start = self.whitespace(start, text)
            # parse another arbitrary expression
            # (which could be a parenthesized full expr)
            start, arg = self.expression_token(start, text, paren)
            tokens.append(arg)
            start = self.whitespace(start, text)
            if start >= len(text):
                break # no more tokens
            # test binary operators, add one if valid
            match = BINOPS.match(text, start)
            if match:
                tokens.append(BinaryOperator(match.group(0)))
                start += len(match.group(0))
            elif text[start] in ')]},':
                break # expressions end here no matter what
            else:
                raise AssertionError(f'Expected operator at column {start}')
            start = self.whitespace(start, text)
        assert tokens and isinstance(tokens[-1], Expression), \
               f'Expected expression at column {start}'
        return (start, tokens)

    def expression_token(self, start: int, text: str, paren: bool = False) \
            -> Tuple[int, Expression]:
        """Expression with no operators, for use as a token."""
        if text.startswith('true', start):
            # boolean value
            start += 4
            arg1 = Boolean(value=True)
        elif text.startswith('false', start):
            start += 5
            arg1 = Boolean(value=False)
        elif text.startswith('input', start):
            start += 5
            start = self.whitespace(start, text, True)
            if text.startswith('int', start):
                arg = VarType.INT
            elif text.startswith('char[]', start):
                arg = ArrayType(type=VarType.CHAR)
            else:
                raise AssertionError('input type must be int or char[]')
            start += len(str(arg))
            arg1 = Input(type=arg)
        elif text[start].isdecimal():
            # number value
            start, arg1 = self.number(start, text)
        elif text[start].isidentifier():
            # variable
            start, arg1 = self.variable(start, text)
            if start < len(text):
                if text[start] == '(':
                    # function call
                    start, args = self.call(start + 1, text)
                    assert text[start] == ')', f'Missing ) at column {start}'
                    start += 1
                    start = self.whitespace(start, text)
                    arg1 = Call(name=arg1, args=args)
                while start < len(text) and text[start] == '[':
                    # array indexation
                    start, index = self.expression(start + 1, text, True)
                    assert text[start] == ']', f'Missing ] at column {start}'
                    start += 1
                    start = self.whitespace(start, text)
                    arg1 = Index(name=arg1, index=index)
        elif text[start] == "'":
            # character
            start, arg1 = self.character(start + 1, text)
        elif text[start] == '"':
            # string
            start, arg1 = self.string(start + 1, text)
        elif text[start] == '{':
            # array
            start, arg1 = self.array(start + 1, text)
        elif text[start] == '(':
            # recursion!
            start, arg1 = self.expression(start + 1, text, True)
        else:
            raise AssertionError(f'Expected expression at column {start}')
        if start < len(text) and paren and text[start] == ')':
            start += 1
        return (start, arg1)

    def number(self, start: int, text: str) -> Tuple[int, Number]:
        """Parse a number."""
        end = start
        while end < len(text) and text[end].isdecimal():
            end += 1
        return (end, Number(value=int(text[start:end])))

    def variable(self, start: int, text: str) -> Tuple[int, Variable]:
        """Parse a variable (or function) name."""
        end = start + 1
        while end < len(text) and (
                text[end].isidentifier()
                or text[end].isdecimal()
        ):
            end += 1
        assert text[start:end].isidentifier(), \
               f'Invalid identifier at column {start}'
        return (end, Variable(name=text[start:end]))

    def character(self, start: int, text: str) -> Tuple[int, Character]:
        """Parse a character literal."""
        end = start
        while text[end] != "'":
            if text[end] == '\\':
                end += 1
                if text[end] in r"\'":
                    end += 1
            else:
                end += 1
        char = text[start:end].encode().decode('unicode-escape')
        assert len(char) == 1, f'Invalid character literal at column {start}'
        return (end + 1, Character(value=char))

    def string(self, start: int, text: str) -> Tuple[int, String]:
        """Parse a string literal."""
        end = start
        while text[end] != '"':
            if text[end] == '\\':
                end += 1
                if text[end] in r'\"':
                    end += 1
            else:
                end += 1
        stri = text[start:end].encode().decode('unicode-escape')
        return (end + 1, String(value=stri))

    def array(self, start: int, text: str) -> Tuple[int, Array]:
        """Parse an array literal."""
        end = start
        values = []
        while text[end] != '}':
            end = self.whitespace(end, text)
            end, value = self.expression(end, text)
            values.append(value)
            end = self.whitespace(end, text)
            if text[end] == ',':
                end += 1
        return (end + 1, Array(values=values))

    def call(self, start: int, text: str) -> Tuple[int, List[Expression]]:
        """Parse arguments of a function call."""
        end = start
        args = []
        while end < len(text) and text[end] != ')':
            end = self.whitespace(end, text)
            end, value = self.expression(end, text)
            args.append(value)
            end = self.whitespace(end, text)
            if end < len(text) and text[end] == ',':
                end += 1
        return (end, args)

    def args(self, start: int, text: str) -> Tuple[int, List[Declaration]]:
        """Parse arguments of a function definition."""
        end = start
        args = []
        while end < len(text) and text[end] != ')':
            end = self.whitespace(end, text)
            end, value = self.declaration(end, text, True)
            args.append(value)
            end = self.whitespace(end, text)
            if end < len(text) and text[end] == ',':
                end += 1
        return (end + 1, args)

    def declaration(self, start: int, text: str, param: bool = False) \
            -> Tuple[int, Declaration]:
        """Parse a variable/parameter declaration."""
        end, typ = self.type(start, text)
        end = self.whitespace(end, text, True)
        if param:
            end, expr = self.variable(end, text)
        else:
            end, expr = self.expression(end, text)
            assert isinstance(expr, Variable), \
                   f'Invalid declaration at column {start}'
        return (end, Declaration(type=typ, name=expr))

    def type(self, start: int, text: str) \
            -> Tuple[int, VarType]:
        """Parse a type."""
        for item in VarType:
            # pylint: disable=no-member
            if text.startswith(str(item), start):
                arg = item
                start += len(str(item))
                while ARRREG.match(text, start):
                    start = self.whitespace(start, text)
                    arg = ArrayType(type=arg)
                    start += 2
                break
            # pylint: enable=no-member
        else:
            raise AssertionError(f'Expected type at column {start}')
        return (start, arg)
