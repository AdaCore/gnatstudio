"""
A plugin that adds support for a basic python interpreter in the
omni-search.
In particular, it can be used to perform basic math operations
directly from the omni-search.
"""

import GPS
import ast
import operator as op


class OmniSearchResult(GPS.Search_Result):
    def __init__(self, expression, result):
        self.short = "%s" % (expression, )
        self.long = "= %s\t%s\t%s" % (result, hex(result), bin(result))


class OmniSearchCalc(GPS.Search):
    def __init__(self):
        # Override default so that we can build instances of the class
        pass

    def set_pattern(self, pattern, flags=0):
        self.pattern = pattern

    def get(self):
        try:
            # Do not use eval (or searching "GPS.exit()" would crash)
            r = Calculator.eval(self.pattern)
            return (False, OmniSearchResult(self.pattern, r))
        except:
            return (False, None)  # No further search results


GPS.Search.register("Calculator", OmniSearchCalc(), rank=1)


# Idea from
# http://stackoverflow.com/questions/2371436/
#   evaluating-a-mathematical-expression-in-a-string

class Calculator:
    """
    A simple calculator, that supports combination of standard mathematical
    operators.
       >>> 1 + 2*3**(4^5) / (6 + -7)
       -5.0
    """

    operators = {ast.Add: op.add, ast.Sub: op.sub, ast.Mult: op.mul,
                 ast.Div: op.truediv, ast.Pow: op.pow, ast.BitXor: op.xor,
                 ast.USub: op.neg}

    @staticmethod
    def eval(expr):
        return Calculator._eval(ast.parse(expr, mode='eval').body)

    @staticmethod
    def _eval(node):
        if isinstance(node, ast.Num):
            return node.n
        elif isinstance(node, ast.BinOp):
            return Calculator.operators[type(node.op)](
                Calculator._eval(node.left),
                Calculator._eval(node.right))
        elif isinstance(node, ast.UnaryOp):
            return Calculator.operators[type(node.op)](
                Calculator._eval(node.operand))
        else:
            raise TypeError(node)
