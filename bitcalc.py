from copy import copy
from functools import reduce
from typing import Any, Iterator, NoReturn, Sequence, Union


class Expr:
    __match_args__ = ("x",)
    x: Any

    @staticmethod
    def empty() -> "Empty":
        return Empty(None)

    def eval(self, bits: int, vars: list[str]) -> int:
        """
        Evaluates the expression by assigning the `i`-th bit of `bits` to the variable `vars[i]`.
        The result is a single bit integer.
        """
        match self:
            case Bit(n) if n in (0, 1):
                return n
            case Bit(x) if type(x) is str:
                return (bits >> vars.index(x)) & 1
            case Not(e):
                return 1 & ~e.eval(bits, vars)
            case And(x):
                return reduce(lambda a, b: a & b, (e.eval(bits, vars) for e in x))
            case Or(x):
                return reduce(lambda a, b: a | b, (e.eval(bits, vars) for e in x))
            case Xor(x):
                return reduce(lambda a, b: a ^ b, (e.eval(bits, vars) for e in x))
            case Empty(_):
                raise ValueError("Cannot evaluate an empty expression.")
            case _:
                unreachable()

    def simplify(self) -> "Expr":
        match self:
            case Bit(_):
                return self
            case Not(Not(x)):
                return x.simplify()
            case Not(x):
                return Not(x.simplify())
            case And(_):
                x, s = [], [e.simplify() for e in self.x]
                for e in s:
                    if Not(e) in x:
                        return Bit(0)
                    else:
                        x.append(e)
                return And(x)
            case Or(_):
                x, s = [], [e.simplify() for e in self.x]
                for e in s:
                    if Not(e) in x:
                        return Bit(1)
                    else:
                        x.append(e)
                return Or(x)
            case Xor(_):
                bits, x, s = [], [], [e.simplify() for e in self.x]
                for e in s:
                    if e in x:
                        x.remove(e)
                        bits.append(0)
                    elif Not(e) in x:
                        x.remove(Not(e))
                        bits.append(1)
                    else:
                        x.append(e)
                if bits and reduce(lambda a, b: a ^ b, bits):
                    x.append(Bit(1))
                return Xor(x)
            case _:
                return self

    def substitute(self, var: str, expr: "Expr") -> "Expr":
        """
        Substitutes all occurrences of `var` in the expression with `expr`
        """
        match self:
            case Bit(x) if x == var:
                return expr
            case Not(x):
                return Not(x.substitute(var, expr))
            case And(x):
                return And([e.substitute(var, expr) for e in x])
            case Or(x):
                return Or([e.substitute(var, expr) for e in x])
            case Xor(x):
                return Xor([e.substitute(var, expr) for e in x])
            case _:
                return self

    @property
    def symbol(self) -> str:
        """
        The operator symbol of the expression
        """
        match self:
            case Bit(_):
                return ""
            case Not(_):
                return "~"
            case And(_):
                return "&"
            case Or(_):
                return "|"
            case Xor(_):
                return "^"
            case _:
                return "()"

    def to_anf(self) -> "Expr":
        """
        Returns the expression in algebraic normal form, i.e. as a XOR of ANDs
        """
        return self.to_tt().to_anf()

    def to_cnf(self) -> "Expr":
        """
        Returns the expression in conjunctive normal form, i.e. as an AND of ORs
        """
        return self.to_tt().to_cnf()

    def to_dnf(self) -> "Expr":
        """
        Returns the expression in disjunctive normal form, i.e. as an OR of ANDs
        """
        return self.to_tt().to_dnf()

    def to_int(self) -> int | None:
        """
        Tries to convert the expression to an integer. If the expression contains any bits other
        than `0` or `1`, returns `None`.
        """
        match self:
            case Bit(n) if n in (0, 1):
                return n
            case _:
                return None

    def to_tt(self) -> "TT":
        """
        Returns the truth table of the expression
        """
        match self:
            case Bit(0):
                return TT.false()
            case Bit(1):
                return TT.true([])
            case Bit(x) if type(x) is str:
                return TT.bit(x)
            case Not(x):
                return ~(x.to_tt())
            case And(x):
                tts = [e.to_tt() for e in x]
                return reduce(lambda a, b: a & b, tts)
            case Or(x):
                tts = [e.to_tt() for e in x]
                return reduce(lambda a, b: a | b, tts)
            case Xor(x):
                tts = [e.to_tt() for e in x]
                return reduce(lambda a, b: a ^ b, tts)
            case _:
                raise ValueError("Cannot convert an empty expression to a truth table.")

    def vars(self) -> list[str]:
        """
        Returns the variables in the expression
        """
        match self:
            case Bit(x) if type(x) is str:
                return [x]
            case Not(x):
                return x.vars()
            case And(x) | Or(x) | Xor(x):
                vars = []
                for e_vars in [e.vars() for e in x]:
                    vars.extend(e_vars)
                return sorted(set(vars))
            case _:
                return []

    def __eq__(self, other: object) -> bool:
        """
        Compares two expressions for equality
        """
        match self, other:
            case Bit(x), Bit(y):
                return x == y
            case (Empty(_), Expr()) | (Expr(), Empty(_)):
                return False
            case Expr(), Expr() if str(self) == str(other):
                return True
            case Expr(), Expr():
                return self.to_tt() == other.to_tt()
            case _:
                return False

    def __and__(self, other: "Expr") -> "Expr":
        """
        Returns the conjunction of two expressions
        """
        match self, other:
            case Bit(n), Bit(m) if n in (0, 1) and m in (0, 1):
                return Bit(n & m)
            case (Bit(0), _) | (_, Bit(0)):
                return Bit(0)
            case (Bit(1), expr) | (expr, Bit(1)) | (Empty(_), expr) | (expr, Empty(_)):
                return expr
            case (Bit(x), Bit(y)) if x == y:
                return Bit(x)
            case (And(x), And(y)):
                return And(x + y)
            case (a, And(x)) | (And(x), a):
                return And([a] + x)
            case _:
                return And([self, other])

    def __init__(self, x: Union[int, str, "Expr", list["Expr"], None]):
        """
        Constructs an expression from a value or a list of expressions
        """
        self.x = x

    def __invert__(self) -> "Expr":
        """
        Returns the negation of the expression
        """
        match self:
            case Bit(n) if n in (0, 1):
                return Bit(~n & 1)
            case Not(expr):
                return expr
            case Empty(_):
                return self
            case _:
                return Not(self)

    def __iter__(self) -> Iterator["Expr"]:
        """
        Returns an iterator over the operands. For unary expressions, returns an empty
        iterator.
        """
        match self:
            case And(x) | Or(x) | Xor(x):
                return iter(x)
            case _:
                return iter([])

    def __len__(self) -> int:
        """
        Returns the number of operands in the expression
        """
        match self:
            case Empty(_):
                return 0
            case Bit(_):
                return 1
            case Not(x):
                return len(x)
            case And(x) | Or(x) | Xor(x):
                return sum(len(e) for e in x)
            case _:
                raise RuntimeError("Reached code that should be unreachable.")

    def __or__(self, other: "Expr") -> "Expr":
        """
        Returns the disjunction of two expressions
        """
        match self, other:
            case Bit(n), Bit(m) if n in (0, 1) and m in (0, 1):
                return Bit(n | m)
            case (Bit(1), _) | (_, Bit(1)):
                return Bit(1)
            case (Bit(x), Bit(y)) if x == y:
                return Bit(x)
            case (Bit(0), expr) | (expr, Bit(0)) | (Empty(_), expr) | (expr, Empty(_)):
                return expr
            case (Or(x), Or(y)):
                return Or(x + y)
            case (o, Or(x)) | (Or(x), o):
                return Or([o] + x)
            case _:
                return Or([self, other])

    def __str__(self):
        """
        Returns the display representation of the expression
        """
        match self:
            case Bit(x):
                return f"{x}"
            case Not(x):
                return f"~{x}"
            case Empty():
                return "()"
            case And(x) | Or(x) | Xor(x):
                operands = sorted(
                    [str(e) for e in x],
                    key=lambda e: (len(e), str(e)),
                )
                return f"({f' {self.symbol} '.join(operands)})"
            case _:
                raise RuntimeError("Reached code that should be unreachable.")

    def __repr__(self) -> str:
        """
        Returns the debug representation of the expression
        """
        match self:
            case Bit(n) if n in (0, 1):
                return f"Bit({n})"
            case Bit(x):
                return f"Bit('{x}')"
            case Not(x):
                return f"Not({repr(x)})"
            case And(x) | Or(x) | Xor(x):
                return f"{type(self).__name__}({', '.join(repr(a) for a in x)})"
            case Empty(_):
                return "Empty"
            case _:
                raise RuntimeError("Reached code that should be unreachable.")

    def __xor__(self, other: "Expr") -> "Expr":
        """
        Returns the exclusive disjunction of two expressions
        """
        match self, other:
            case Bit(n), Bit(m) if n in (0, 1) and m in (0, 1):
                return Bit(n ^ m)
            case (Bit(0), expr) | (expr, Bit(0)) | (Empty(_), expr) | (expr, Empty(_)):
                return expr
            case (Bit(x), Bit(y)) if x == y:
                return Bit(0)
            case (Xor(x), Xor(y)):
                return Xor(x + y)
            case (e, Xor(x)) | (Xor(x), e):
                return Xor([e] + x)
            case _:
                return Xor([self, other])


class Bit(Expr):
    """
    A single bit constant or variable
    """

    x: int | str

    @staticmethod
    def __new__(cls, x: Union[str, int]) -> "Bit":
        assert (
            type(x) is str or type(x) is int and x in (0, 1)
        ), f"Invalid argument. Cannot construct a Bit from {x}"
        return super(Expr, Bit).__new__(Bit)


class Not(Expr):
    """
    A negated expression
    """

    x: Expr

    @staticmethod
    def __new__(cls, x: Expr) -> "Expr":
        assert isinstance(
            x, Expr
        ), f"Invalid argument. Cannot construct a Not expression from {x}"
        if isinstance(x, Bit) and type(x.x) is int:
            return Bit(~x.x & 1)
        elif isinstance(x, Not):
            return x.x
        else:
            return super(Expr, Not).__new__(Not)


class And(Expr):
    """
    A conjunction of expressions
    """

    x: list[Expr]

    @staticmethod
    def __new__(cls, x: list[Expr]) -> "Expr":
        assert type(x) is list and all(
            isinstance(e, Expr) for e in x
        ), f"Invalid argument. Cannot construct an And expression from {x}"
        while len(x) > 1 and Bit(1) in x:
            x.remove(Bit(1))
        if len(x) == 0:
            return Empty(None)
        elif len(x) == 1:
            return x[0]
        elif Bit(0) in x:
            return Bit(0)
        else:
            return super(Expr, And).__new__(And)


class Or(Expr):
    """
    A disjunction of expressions
    """

    x: list[Expr]

    @staticmethod
    def __new__(cls, x: list[Expr]) -> "Expr":
        assert type(x) is list and all(
            isinstance(e, Expr) for e in x
        ), f"Invalid argument. Cannot construct an And expression from {x}"
        while len(x) > 1 and Bit(0) in x:
            x.remove(Bit(0))
        if len(x) == 0:
            return Empty(None)
        elif len(x) == 1:
            return x[0]
        elif Bit(1) in x:
            return Bit(1)
        else:
            return super(Expr, Or).__new__(Or)


class Xor(Expr):
    """
    An exclusive disjunction of expressions
    """

    x: list[Expr]

    @staticmethod
    def __new__(cls, x: list[Expr]) -> "Expr":
        assert type(x) is list and all(
            isinstance(e, Expr) for e in x
        ), f"Invalid argument. Cannot construct an And expression from {x}"
        index, bits = 0, []
        while index < len(x):
            if isinstance(x[index], Bit) and x[index].x in (0, 1):
                bits.append(x.pop(index).x)
            else:
                index += 1
        if bits and reduce(lambda a, b: a ^ b, bits):
            x.append(Bit(1))
        if len(x) == 0:
            return Empty(None)
        elif len(x) == 1:
            return x[0]
        else:
            return super(Expr, Xor).__new__(Xor)


class Empty(Expr):
    """
    An empty expression
    """

    @staticmethod
    def __new__(cls, x: None) -> "Empty":
        return super(Expr, Empty).__new__(Empty)


class TT:
    """
    A truth table
    """

    rows: list[int]
    vars: list[str]

    def align_with(self, other: "TT") -> "TT":
        """
        Aligns the truth table with `other` by adding the missing variables from `other`.
        """
        vars = sorted(set(self.vars + other.vars))
        missing = [i for i, v in enumerate(vars) if v not in self.vars]
        mask = 2 ** len(vars) - 1
        rows = self.rows
        for i in missing:
            low = 2**i - 1
            high = (mask ^ low) << 1
            zeros = [((r << 1) & high) | (r & low) for r in rows]
            ones = [((r << 1) & high) | (r & low) | (1 << i) for r in rows]
            rows = zeros + ones
        return TT(sorted(rows), vars)

    @classmethod
    def bit(cls, label: str) -> "TT":
        """
        Returns a truth table with a single bit variable
        """
        return TT([1], [label])

    def can_prune(self, v: str) -> bool:
        """
        Returns whether the variable is redundant and should be pruned. A variable is redundant
        if the result doesn't depend on it, or if it's not present in the expression.
        """
        if v in self.vars:
            x = 1 << self.vars.index(v)
            r00 = [r for r in self.rows if not r & x]
            if len(r00) == len(self.rows) // 2:
                r11 = [r for r in self.rows if r & x]
                r01 = [r | x for r in r00]
                r10 = [r ^ x for r in r11]
                return r00 == r10 and r01 == r11
        return False

    @classmethod
    def false(cls) -> "TT":
        """
        Returns an empty truth table
        """
        return TT([], [])

    def prune(self) -> "TT":
        """
        Removes redundant variables from the truth table
        """
        if self.rows == []:
            return TT([], [])
        elif self.rows == [0]:
            return self
        elif len(self.rows) == 2 ** len(self.vars):
            return TT([0], [])
        else:
            vars = copy(self.vars)
            rows = copy(self.rows)
            extra = [v for v in vars if self.can_prune(v)]
            while extra:
                mask = 2 ** len(vars) - 1
                index = vars.index(extra.pop())
                vars.pop(index)
                low = 2**index - 1
                high = mask ^ low
                rows = sorted(set(((r >> 1) & high) | (r & low) for r in rows))
            return TT(rows, vars)

    def to_anf(self) -> "Expr":
        """
        Converts the truth table to an algebraic normal form expression, i.e. a XOR of ANDs
        """
        tt = self.prune()
        rows, vars = tt.rows, list(enumerate(tt.vars))
        n = 2 ** len(vars)
        match len(rows):
            case 0:
                return Bit(0)
            case l if l == n:
                return Bit(1)
            case _:
                coeff, parts, temp = [], [], []
                for x in range(n):
                    for row in rows:
                        if row > 0 and row - 1 not in rows:
                            temp.append(row - 1)
                        if row < n - x and row + 1 not in rows:
                            temp.append(row)
                    if rows and rows[0] == 0:
                        coeff.append(x)
                    rows, temp = temp, []
                for x in coeff:
                    bits = [Bit(1)] + [Bit(v) for i, v in vars if x & (1 << i)]
                    parts.append(reduce(lambda a, b: a & b, bits))
                anf = reduce(lambda a, b: a ^ b, parts)
                return anf

    def to_cnf(self) -> "Expr":
        """
        Converts the truth table to a conjunctive normal form expression, i.e. an AND of ORs
        """
        tt = self.prune()
        rows, vars = tt.rows, list(enumerate(tt.vars))
        n = 2 ** len(vars)
        match len(rows):
            case 0:
                return Bit(0)
            case l if l == n:
                return Bit(1)
            case _:
                parts = []
                for x in range(n):
                    if x not in rows:
                        bits = [~Bit(v) if x & (1 << i) else Bit(v) for i, v in vars]
                        parts.append(reduce(lambda a, b: a | b, bits))
                cnf = reduce(lambda a, b: a & b, parts)
                return cnf

    def to_dnf(self) -> "Expr":
        """
        Converts the truth table to a disjunctive normal form expression, i.e. an OR of ANDs
        """
        tt = self.prune()
        rows, vars = tt.rows, list(enumerate(tt.vars))
        n = 2 ** len(vars)
        match len(rows):
            case 0:
                return Bit(0)
            case l if l == n:
                return Bit(1)
            case _:
                parts = []
                for x in rows:
                    bits = [Bit(v) if x & (1 << i) else ~Bit(v) for i, v in vars]
                    parts.append(reduce(lambda a, b: a & b, bits))
                dnf = reduce(lambda a, b: a | b, parts)
                return dnf

    @classmethod
    def true(cls, vars: list[str]) -> "TT":
        """
        Returns a truth table with all rows set to 1
        """
        size = 2 ** len(vars)
        rows = [r for r in range(size)]
        return TT(rows, vars)

    def __init__(self, rows: list[int], vars: list[str]):
        """
        Constructs a truth table from a list of rows and variables
        """
        self.rows = rows
        self.vars = vars

    def __eq__(self, other: object) -> bool:
        """
        Compares two truth tables for equality
        """
        if type(other) is not TT:
            return False
        else:
            a, b = self.prune(), other.prune()
            return a.vars == b.vars and a.rows == b.rows

    def __and__(self, other: "TT") -> "TT":
        """
        Computes the intersection of two truth tables
        """
        a = self.align_with(other)
        b = other.align_with(self)
        rows = [r for r in a.rows if r in b.rows]
        a.rows = rows
        return a

    def __or__(self, other: "TT"):
        """
        Computes the union of two truth tables
        """
        a = self.align_with(other)
        b = other.align_with(self)
        rows = sorted(set(a.rows + b.rows))
        a.rows = rows
        return a

    def __xor__(self, other: "TT"):
        """
        Computes the symmetric difference of two truth tables
        """
        a = self.align_with(other)
        b = other.align_with(self)
        a_rows = [r for r in a.rows if r not in b.rows]
        b_rows = [r for r in b.rows if r not in a.rows]
        a.rows = sorted(set(a_rows + b_rows))
        return a

    def __invert__(self) -> "TT":
        """
        Inverts the truth table
        """
        n = 2 ** len(self.vars)
        rows = [r for r in range(n) if r not in self.rows]
        return TT(rows, self.vars)

    def __iter__(self):
        """
        Returns an iterator over the rows
        """
        return iter(self.rows)

    def __repr__(self) -> str:
        return f"TT({self.rows}, {self.vars})"


class UInt:
    __match_args__ = ("bits",)
    bits: list[Expr]

    def __init__(self, bits, width=None):
        """
        Constructs a UInt from an integer, a label and width or a list of bit expressions.
        See the methods `from_int`, `from_label`, and `from_exprs` for more details.
        ```python
        UInt(0b01011010)  # An 8-bit UInt representing the value `0b01011010`
        UInt('x', 8)      # An 8-bit UInt with bits ['x[7]', 'x[6]', 'x[5]', 'x[4]', 'x[3]', 'x[2]', 'x[1]', 'x[0]']
        UInt([Bit(a), Bit(1), Bit(0), Bit(1)])  # A 4-bit UInt representing the value `0ba101`, where `a` is a bit variable.
        ```
        """
        if type(bits) is int:
            this = UInt.from_value(bits, width)
        elif type(bits) is str:
            assert (
                width is not None
            ), "Width must be provided when constructing from a label."
            this = UInt.from_label(bits, width)
        else:
            this = UInt.from_exprs(list(reversed(bits)))
        self.bits = this.bits

    def __new__(cls, *args, **kwargs):
        this = object.__new__(cls)
        this.bits = []
        return this

    def __getitem__(self, index: int | slice) -> Union[Expr, "UInt"]:
        """
        Depending on the type of the index, returns either a single bit or a UInt containing the bits at the given index.
        """
        if type(index) is slice:
            return UInt.from_exprs(self.bits[index])
        elif type(index) is int:
            bits = self.bits[index]
            return bits
        else:
            raise ValueError(f"Invalid index: {index}")

    def __setitem__(self, index, value):
        """
        Sets the bits at the given index.
        """
        self.bits[index] = value

    def __add__(self, other) -> "UInt":
        """
        Unsigned, modular addition.
        """
        self.assert_similar(other)
        zero = UInt.from_value(0, self.width())
        a, b = copy(self), copy(other)
        sum = a ^ b
        carry = a & b
        while carry != zero:
            shifted = carry << 1
            carry = sum & shifted
            sum ^= shifted
        return sum

    def __sub__(self, other) -> "UInt":
        """
        Unsigned, modular subtraction.
        """
        self.assert_similar(other)
        x = [Bit(1)] + [Bit(0)] * (self.width() - 1)
        negated = ~other + UInt.from_exprs(x)
        return self + negated

    def __mul__(self, other) -> "UInt":
        """
        Unsigned, modular multiplication.
        """
        self.assert_similar(other)
        a, b = copy(self), copy(other)
        zero = UInt.from_value(0, self.width())
        prod = zero
        while a != zero:
            prod += UInt.from_exprs([(Bit(1) & a.bits[0])] * self.width()) & b
            a >>= 1
            b <<= 1
        return prod

    def __and__(self, other) -> "UInt":
        """
        Bitwise `and` operation
        """
        self.assert_similar(other)
        return UInt.from_exprs([(a & b) for a, b in zip(self.bits, other.bits)])

    def __or__(self, other) -> "UInt":
        """
        Bitwise `or` operation
        """
        self.assert_similar(other)
        return UInt.from_exprs([(a | b) for a, b in zip(self.bits, other.bits)])

    def __xor__(self, other) -> "UInt":
        """
        Bitwise `xor` operation
        """
        self.assert_similar(other)
        return UInt.from_exprs([(a ^ b) for a, b in zip(self.bits, other.bits)])

    def __invert__(self) -> "UInt":
        """
        Bitwise `not` operation
        """
        return UInt.from_exprs([~a for a in self.bits])

    def __iter__(self):
        """
        Returns an iterator over the bits
        """
        return iter(self.bits)

    def __lshift__(self, n) -> "UInt":
        """
        Shifts the bits left by `n` positions, filling the right with zeros.
        """
        assert n >= 0, "Shift must be non-negative."
        shifted = [Bit(0)] * n + self.bits[:-n] if n > 0 else self.bits
        return UInt.from_exprs(shifted)

    def __rshift__(self, n) -> "UInt":
        """
        Shifts the bits right by `n` positions, filling the left with zeros.
        """
        assert n >= 0, "Shift must be non-negative."
        shifted = self.bits[n:] + [Bit(0)] * n
        return UInt.from_exprs(shifted)

    def __eq__(self, other) -> bool:
        """
        Equality comparison
        """
        self.assert_similar(other)
        return all(a == b for a, b in zip(self.bits, other.bits))

    def __repr__(self) -> str:
        return f"UInt({self.bits})"

    def __str__(self) -> str:
        return " ".join(str(bit) for bit in reversed(self.bits))

    def __int__(self) -> int:
        """
        Converts the UInt to an integer. If the UInt contains any bits other
        than `0` or `1`, raises a ValueError.
        """
        result = 0
        for i, bit in enumerate(self.bits):
            match bit.to_int():
                case None:
                    raise ValueError(
                        f"Cannot convert to int. {self} contains non-binary bits."
                    )
                case 1:
                    result |= 1 << i
        return result

    def assert_similar(self, other):
        """
        Checks that `other` is an UInt and has the same length as `self`. Raises
        a ValueError if the conditions are not met.
        """
        if not isinstance(other, UInt):
            raise ValueError("Operand must be an UInt.")
        if self.width() != other.width():
            raise ValueError("UInt bit widths must be equal.")

    def concat(self, other: "UInt") -> "UInt":
        """
        Concatenates two UInts
        """
        return UInt.from_exprs(self.bits + other.bits)

    @classmethod
    def from_exprs(cls, exprs: Sequence[Expr]) -> "UInt":
        """
        Constructs a UInt from the list of expressions, interpreting each expression as a bit. Note that the first
        expression in the list will become the most significant bit.

        Raises a ValueError if `exprs` is not a list of expressions.
        """
        if not all(isinstance(expr, Expr) for expr in exprs):
            raise ValueError(f"Expected a list of expressions, got {exprs}.")
        uint = cls.__new__(cls)
        uint.bits = list(exprs)
        return uint

    @classmethod
    def from_value(cls, value: int, width: int | None = None) -> "UInt":
        """
        Constructs a UInt from an integer value and optional bit width. If the width is not provided, the UInt
        will have the minimum width required to represent the value. Raises a ValueError if the value is not an
        integer or if the width is less than the minimum required to represent the value.
        """
        if type(value) is not int:
            raise ValueError(f"Expected an integer value, got {value}.")
        uint = cls.__new__(cls)
        while value > 0:
            uint.bits.append(Bit(value & 1))
            value >>= 1
        if width is not None:
            if len(uint.bits) < width:
                uint.bits = uint.bits + [Bit(0)] * (width - len(uint.bits))
            elif len(uint.bits) > width:
                raise ValueError(
                    f"UInt({value}, width={width}): {value} cannot be represented in {width} bits."
                )
        return uint

    @classmethod
    def from_label(cls, label: str, width: int) -> "UInt":
        """
        Constructs a UInt from a label and width. The label will be used as a prefix for the bit names.
        """
        uint = cls.__new__(cls)
        zwidth = len(str(width - 1))
        uint.bits = [Bit(f"{label}[{str(i).zfill(zwidth)}]") for i in range(width)]
        return uint

    def reverse_bits(self) -> "UInt":
        """
        Reverses the order of the bits
        """
        return UInt.from_exprs(self.bits[::-1])

    def rotate_left(self, n) -> "UInt":
        """
        Rotates the bits left by `n` positions
        """
        assert n >= 0, "Shift must be non-negative."
        rotated = self.bits[-n:] + self.bits[:-n] if n > 0 else self.bits
        return UInt.from_exprs(rotated)

    def rotate_right(self, n) -> "UInt":
        """
        Rotates the bits right by `n` positions
        """
        assert n >= 0, "Shift must be non-negative."
        rotated = self.bits[n:] + self.bits[:n]
        return UInt.from_exprs(rotated)

    def set_width(self, width):
        """
        Sets the bit width to `width`. If `width` is less than the current width,
        the most significant bits will be removed. If `width` is greater than the
        current width, the `UInt` will be zero-extended.
        """
        if width < self.width():
            self.bits = self.bits[:width]
        elif width > self.width():
            self.bits = self.bits + [Bit(0)] * (width - self.width())
        return self

    def width(self) -> int:
        """
        Returns the value's bit width
        """
        return len(self.bits)


def unreachable() -> NoReturn:
    """
    Raises a RuntimeError with a message indicating that the code should be unreachable.
    """
    raise RuntimeError("Reached code that should be unreachable.")
