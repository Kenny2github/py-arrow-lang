"""Test arrowlang.parser"""
import pytest
from arrowlang import leaves
from arrowlang.parser import ArrowParser

parser = ArrowParser()

class TestArrowParser:
    """Test the methods that ArrowParser provides."""

    def test_whitespace(self):
        """Test whitespace skipping."""
        assert parser.whitespace(1, 'a \t\t \tb') == 6
        with pytest.raises(AssertionError):
            parser.whitespace(1, 'ab', True)

    def test_indent(self):
        """Test indent skipping."""
        assert parser.indent(0, '| | ', 3) == 2
        with pytest.raises(AssertionError):
            parser.indent(0, '| ', 4)

    def test_number(self):
        """Test number parsing."""
        end, num = parser.number(0, '123 ')
        assert end == 3
        assert isinstance(num, leaves.Number)
        assert num.value == 123
        with pytest.raises(AssertionError):
            parser.number(0, 'abc')

    def test_variable(self):
        """Test identifier parsing."""
        end, var = parser.variable(0, 'abc')
        assert end == 3
        assert isinstance(var, leaves.Variable)
        assert var.name == 'abc'
        with pytest.raises(AssertionError):
            parser.variable(0, '1st')

    def test_character(self):
        """Test character literal parsing."""
        end, char = parser.character(1, "'a'")
        assert end == 3
        assert isinstance(char, leaves.Character)
        assert char.value == 'a'
        with pytest.raises(AssertionError):
            parser.character(1, "'a")
        with pytest.raises(AssertionError):
            parser.character(1, "'abc'")

    def test_string(self):
        """Test string literal parsing."""
        end, string = parser.string(1, r'"\abc"')
        assert end == 6
        assert isinstance(string, leaves.String)
        assert string.value == '\abc'
        with pytest.raises(AssertionError):
            parser.string(1, '"abc')

    def test_array(self):
        """Test array literal parsing."""
        # note that this is just parsing,
        # not typechecking - that comes later.
        end, arr = parser.array(1, '{a, "b", c()}')
        assert end == 13
        assert isinstance(arr, leaves.Array)
        assert len(arr.values) == 3
        assert isinstance(arr.values[0], leaves.Variable)
        assert isinstance(arr.values[1], leaves.String)
        assert isinstance(arr.values[2], leaves.Call)
        assert arr.values[0].name == 'a'
        assert arr.values[1].value == 'b'
        assert arr.values[2].name.name == 'c'
        assert len(arr.values[2].args) == 0
        with pytest.raises(AssertionError):
            parser.array(1, '{a, b')

    def test_call(self):
        """Test function call argument parsing."""
        end, call = parser.call(2, 'a(b, "c")')
        assert end == 8 # stops before )
        assert isinstance(call, list)
        assert isinstance(call[0], leaves.Variable)
        assert isinstance(call[1], leaves.String)
        assert call[0].name == 'b'
        assert call[1].value == 'c'
        with pytest.raises(AssertionError):
            parser.call(2, 'a(b, c')

    def test_type(self):
        """Test type parsing."""
        end, typ = parser.type(0, 'int')
        assert end == 3
        assert isinstance(typ, leaves.VarType)
        assert typ == leaves.VarType.INT
        end, typ = parser.type(0, 'bool[][]')
        assert end == 8
        assert isinstance(typ, leaves.ArrayType)
        assert isinstance(typ.type, leaves.ArrayType)
        assert isinstance(typ.type.type, leaves.VarType)
        assert typ.type.type == leaves.VarType.BOOL
        with pytest.raises(AssertionError):
            parser.type(0, 'long')

    def test_declaration(self):
        """Test declaration parsing."""
        end, decl = parser.declaration(0, 'int i[12]')
        assert end == 9
        assert isinstance(decl, leaves.Declaration)
        assert isinstance(decl.type, leaves.VarType)
        assert decl.type == leaves.VarType.INT
        assert isinstance(decl.name, leaves.Index)
        assert isinstance(decl.name.name, leaves.Variable)
        assert decl.name.name.name == 'i'
        assert isinstance(decl.name.index, leaves.Number)
        assert decl.name.index.value == 12
        with pytest.raises(AssertionError):
            parser.declaration(0, 'int[] 123')
        end, decl = parser.declaration(0, 'int i[12]', True)
        assert end == 5

    def test_args(self):
        """Test function argument declaration parsing."""
        end, args = parser.args(1, '(int a, bool[] b)')
        assert end == 17
        assert isinstance(args, list)
        assert len(args) == 2
        assert all(isinstance(decl, leaves.Declaration) for decl in args)
        assert args[0].type == leaves.VarType.INT
        assert args[0].name.name == 'a'
        assert args[1].type == leaves.ArrayType(type=leaves.VarType.BOOL)
        assert args[1].name.name == 'b'
        with pytest.raises(AssertionError):
            parser.args(1, '(int a, bool[] b')
        with pytest.raises(AssertionError):
            parser.args(1, '(int[] a[12], bool[] b)')

    def test_expression_token(self):
        """Test other types of single expressions."""
        end, value = parser.expression_token(0, 'true')
        assert end == 4
        assert isinstance(value, leaves.Boolean)
        assert value.value is True
        end, value = parser.expression_token(0, 'false')
        assert end == 5
        assert isinstance(value, leaves.Boolean)
        assert value.value is False
        end, value = parser.expression_token(0, 'input int')
        assert end == 9
        assert isinstance(value, leaves.Input)
        assert isinstance(value.type, leaves.VarType)
        assert value.type == leaves.VarType.INT
        end, value = parser.expression_token(0, 'input char[]')
        assert end == 12
        assert isinstance(value.type, leaves.ArrayType)
        assert value.type == leaves.ArrayType(type=leaves.VarType.CHAR)
        with pytest.raises(AssertionError):
            parser.expression_token(0, 'input bool')
        with pytest.raises(AssertionError):
            parser.expression_token(0, '=a')

    def test_tokenize(self):
        """Test tokenization of expression and operator chains."""
        end, tokens = parser.tokenize(0, '-a * "b" + 3')
        assert end == 12
        assert isinstance(tokens, list)
        assert len(tokens) == 6
        assert tokens[0] == leaves.UnaryOperator.NEG
        assert isinstance(tokens[1], leaves.Variable)
        assert tokens[1].name == 'a'
        assert tokens[2] == leaves.BinaryOperator.TIMES
        assert isinstance(tokens[3], leaves.String)
        assert tokens[3].value == 'b'
        assert tokens[4] == leaves.BinaryOperator.PLUS
        assert isinstance(tokens[5], leaves.Number)
        assert tokens[5].value == 3
        with pytest.raises(AssertionError):
            parser.tokenize(0, 'a * / b')
        with pytest.raises(AssertionError):
            parser.tokenize(0, 'a * b /')

    def test_expression(self):
        """Test parsing compound expressions."""
        end, expr = parser.expression(0, '-+a / 2')
        assert end == 7
        assert isinstance(expr, leaves.BinaryOperation)
        assert isinstance(expr.arg1, leaves.UnaryOperation)
        assert isinstance(expr.arg1.arg, leaves.UnaryOperation)
        assert isinstance(expr.arg1.arg.arg, leaves.Variable)
        assert expr.arg1.arg.arg.name == 'a'
        assert expr.operator == leaves.BinaryOperator.DIVIDE
        assert isinstance(expr.arg2, leaves.Number)
        assert expr.arg2.value == 2
        with pytest.raises(AssertionError):
            parser.expression(0, '+')

    def test_printstmt(self):
        """Test parsing print statements."""
        end, stmt = parser.printstmt(6, 'print "Hello, World!"')
        assert end == 21
        assert isinstance(stmt, leaves.Print)
        assert len(stmt.values) == 1
        assert isinstance(stmt.values[0], leaves.String)
        assert stmt.values[0].value == "Hello, World!"

    def test_jump_up(self):
        """Test do-while/jump-up loop."""
        # start
        end, stmt = parser.statement(0, '/-->', 1)
        assert end == 4
        assert isinstance(stmt, leaves.JumpUp)
        assert stmt.condition is None
        assert len(stmt.body.stmts) == 0
        # end
        end, stmt = parser.statement(0, r'\--< true', 1)
        assert end == 9
        assert isinstance(stmt, leaves.End)
        assert stmt.cls is leaves.JumpUp
        assert isinstance(stmt.value, leaves.Boolean)
        assert stmt.value.value is True
        with pytest.raises(AssertionError):
            parser.statement(0, r'\--<', 1)

    def test_jump_down(self):
        """Test if-not/jump-down block."""
        # start
        end, stmt = parser.statement(0, '/--< true', 1)
        assert end == 9
        assert isinstance(stmt, leaves.JumpDown)
        assert isinstance(stmt.condition, leaves.Boolean)
        assert stmt.condition.value is True
        assert len(stmt.body.stmts) == 0
        with pytest.raises(AssertionError):
            parser.statement(0, '/--<', 1)
        # end
        end, stmt = parser.statement(0, r'\-->', 1)
        assert end == 4
        assert isinstance(stmt, leaves.End)
        assert stmt.cls is leaves.JumpDown
        assert stmt.value is None

    def test_function(self):
        """Test parsing functions."""
        # start
        end, stmt = parser.statement(0, '/--> int func()', 1)
        assert end == 15
        assert isinstance(stmt, leaves.Function)
        assert stmt.rettype == leaves.VarType.INT
        assert stmt.name.name == 'func'
        assert len(stmt.params) == 0
        assert stmt.retval is None
        assert len(stmt.body.stmts) == 0
        # return value
        end, stmt = parser.statement(0, '^ false', 1)
        assert end == 7
        assert isinstance(stmt, leaves.End)
        assert stmt.cls is leaves.Function
        assert isinstance(stmt.value, leaves.Boolean)
        assert stmt.value.value is False
        with pytest.raises(AssertionError):
            parser.statement(0, '^', 1)

    def test_require(self):
        """Test parsing require statement."""
        end, stmt = parser.statement(0, 'require true', 1)
        assert end == 12
        assert isinstance(stmt, leaves.Require)
        assert isinstance(stmt.expr, leaves.Boolean)
        assert stmt.expr.value is True

    def test_misc_statements(self):
        """Test miscellaneous statements."""
        # declaration
        end, stmt = parser.statement(0, 'int ret', 1)
        assert end == 7
        assert isinstance(stmt, leaves.Declaration)
        # leave it at that - we've already gone over decls in detail
        # assignment
        end, stmt = parser.statement(0, 'ret = 1', 1)
        assert end == 7
        assert isinstance(stmt, leaves.Assignment)
        assert stmt.name.name == 'ret'
        assert isinstance(stmt.value, leaves.Number)
        assert stmt.value.value == 1
        with pytest.raises(AssertionError):
            parser.statement(0, '1 = 1', 1)
        # expression
        end, stmt = parser.statement(0, 'dothing()', 1)
        assert end == 9
        assert isinstance(stmt, leaves.Call)
        assert stmt.name.name == 'dothing'
        assert len(stmt.args) == 0
