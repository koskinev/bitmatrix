from typing import Union


class Expr:
    __match_args__ = ("x",)
    x: Union[int, str, "Expr", list["Expr"]]

    def anf(self, ignore=[]) -> "Expr":
        """
        Returns the algebraic normal form of the expression
        """
        match self.simplify():
            case expr if expr.is_anf():
                return expr
            case expr:
                # Select the variable to substitute and add it to the ignore list
                vars = [v for v in expr.vars() if v not in ignore]
                v = vars.pop()
                ig = ignore + [v]

                # Substitute the variable with 0 and 1
                x0 = expr.subst(v, Bit(0))
                x1 = expr.subst(v, Bit(1))

                # Recursively calculate the ANF
                g = x0.anf(ig)
                h = And(Bit(v), Xor(x0, x1)).anf(ig)
                return Xor(g, h).simplify()

    def is_anf(self) -> bool:
        """
        Returns True if the expression is in algebraic normal form
        """
        match self:
            case Bit(_):
                return True
            case Xor(x):
                return all(a.is_anf() for a in x)
            case And(x):
                return all(type(a) is Bit for a in x)
            case _:
                return False

    @property
    def name(self) -> str:
        """
        The name of the expression
        """
        return type(self).__name__

    def simplify(self) -> "Expr":
        """
        Simplifies the expression
        """
        return self

    def subst(self, var: str, expr: "Expr") -> "Expr":
        """
        Substitutes the variable with the given expression and returns the resulting expression.
        If the variable is not found, returns the original expression.
        """
        match self:
            case Bit(x) if x == var:
                return expr
            case Bit(_) as bit:
                return bit
            case Not(x):
                return Not(x.subst(var, expr)).simplify()
            case And(x) | Or(x) | Xor(x):
                x = [a.subst(var, expr) for a in x]
                result = type(self)(*x)
                return result.simplify()

    def to_int(self) -> int | None:
        """
        Tries to convert the expression to an integer. If the expression contains any bits other
        than `0` or `1`, returns `None`.
        """
        match self.simplify():
            case Bit(0):
                return 0
            case Bit(1):
                return 1
            case _:
                return None

    def vars(self) -> set[str]:
        """
        Returns the variables in the expression as a set
        """
        match self:
            case Bit(0) | Bit(1):
                return {}
            case Bit(x):
                return {x}
            case Not(x):
                return x.vars()
            case And(x) | Or(x) | Xor(x):
                return set().union(*[a.vars() for a in x])

    def __eq__(self, other: "Expr") -> bool:
        repr_self = repr(self.anf())
        repr_other = repr(other.anf())
        return repr_self == repr_other

    def __hash__(self) -> int:
        return hash(repr(self.anf()))

    def __lt__(self, other: "Expr") -> bool:
        match self, other:
            case Bit(_), Bit(_):
                if type(self.x) is type(other.x):
                    return self.x < other.x
                else:
                    return type(self.x) is int
            case Bit(_), Expr(_):
                return True
            case Expr(_), Bit(_):
                return False
            case And(_), And(_) | Or(_), Or(_) | Xor(_), Xor(_):
                if len(self.x) < len(other.x):
                    return True
                elif len(self.x) > len(other.x):
                    return False
                else:
                    lhs = [sorted(self.x)]
                    rhs = [sorted(other.x)]
                    return lhs < rhs
            case _:
                return self.name < other.name

    def __and__(self, other: "Expr") -> "Expr":
        return And(self, other)

    def __invert__(self) -> "Expr":
        return Not(self)

    def __or__(self, other: "Expr") -> "Expr":
        return Or(self, other)

    def __str__(self):
        match self:
            case Bit(x):
                return f"{x}"
            case Not(x):
                return f"~{x}"
            case And(x):
                return f"({'&'.join([str(a) for a in sorted(x)])})"
            case Or(x):
                return f"({'|'.join([str(a) for a in sorted(x)])})"
            case Xor(x):
                return f"({'^'.join([str(a) for a in sorted(self.x)])})"

    def __repr__(self) -> str:
        match self:
            case Bit(x):
                return f"Bit({x})"
            case Not(x):
                return f"Not({repr(x)})"
            case And(x):
                return f"And({repr([repr(a) for a in sorted(x)])})"
            case Or(x):
                return f"Or({repr([repr(a) for a in sorted(x)])})"
            case Xor(x):
                return f"Xor({repr([repr(a) for a in sorted(x)])})"

    def __xor__(self, other: "Expr") -> "Expr":
        return Xor(self, other)


class Bit(Expr):
    x: int | str

    def __init__(self, x: int | str):
        assert x in (0, 1) or isinstance(
            x, str
        ), f"Invalid argument: {x}. Expected 0, 1, or a string."
        self.x = x

    def simplify(self) -> "Expr":
        return self


class Not(Expr):
    x: "Expr"

    def __init__(self, x: "Expr"):
        assert isinstance(
            x, Expr
        ), f"Invalid argument for a Not expression: {x}. Expected an expression."
        self.x = x

    def simplify(self) -> "Expr":
        match self.x.simplify():
            case Bit(0):
                return Bit(1)
            case Bit(1):
                return Bit(0)
            case Not(x):
                return x
            case x:
                return Not(x)


class And(Expr):
    x: list[Expr]

    def __init__(self, *args: "Expr"):
        assert (
            len(args) > 1 and all(isinstance(a, Expr) for a in args)
        ), f"Invalid arguments for an And expression: {args}. Expected a list of expressions."
        self.x = list(args)

    def simplify(self) -> "Expr":
        ops = []
        for a in self.x:
            expr = a.simplify()
            if expr == Bit(0):
                return Bit(0)
            elif expr == Bit(1):
                continue
            elif type(expr) is And:
                ops.extend(expr.x)
            else:
                ops.append(expr)

        i = 1
        while i < len(ops):
            if ops[i] in ops[:i]:
                ops.remove(ops[i])
            elif Not(ops[i]) in ops[:i]:
                return Bit(0)
            else:
                i += 1

        if len(ops) == 0:
            return Bit(1)
        if len(ops) == 1:
            return ops[0]
        else:
            return And(*ops)


class Or(Expr):
    x: list[Expr]

    def __init__(self, *args: "Expr"):
        assert (
            len(args) > 1 and all(isinstance(a, Expr) for a in args)
        ), f"Invalid arguments for an Or expression: {args}. Expected a list of expressions."
        self.x = list(args)

    def simplify(self) -> "Expr":
        ops = []
        for a in self.x:
            expr = a.simplify()
            if expr == Bit(1):
                return Bit(1)
            elif expr == Bit(0):
                continue
            elif type(expr) is Or:
                ops.extend(expr.x)
            else:
                ops.append(expr)

        i = 1
        while i < len(ops):
            if ops[i] in ops[:i]:
                ops.remove(ops[i])
            elif Not(ops[i]) in ops[:i]:
                return Bit(1)
            else:
                i += 1

        if len(ops) == 0:
            return Bit(0)
        if len(ops) == 1:
            return ops[0]
        else:
            return Or(*ops)


class Xor(Expr):
    x: list[Expr]

    def __init__(self, *args: "Expr"):
        assert (
            len(args) > 1 and all(isinstance(a, Expr) for a in args)
        ), f"Invalid arguments for an Xor expression: {args}. Expected a list of expressions."
        self.x = list(args)

    def simplify(self) -> "Expr":
        ops = []
        for a in self.x:
            expr = a.simplify()
            if expr == Bit(0):
                continue
            elif type(expr) is Xor:
                ops.extend(expr.x)
            else:
                ops.append(expr)

        i = 1
        while i < len(ops):
            if ops[i] in ops[:i]:
                j = ops[:i].index(ops[i])
                ops.pop(i)
                ops.pop(j)
                ops.append(Bit(0))
            else:
                i += 1

        if len(ops) == 0:
            return Bit(0)
        if len(ops) == 1:
            return ops[0]
        else:
            return Xor(*ops)


class UInt:
    __match_args__ = ("bits",)
    bits: list[Bit]

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
            this = UInt.from_label(bits, width)
        else:
            this = UInt.from_exprs(list(reversed(bits)))
        self.bits = this.bits

    def __new__(cls, *args, **kwargs):
        this = object.__new__(cls)
        this.bits = []
        return this

    def __getitem__(self, index) -> Union[Bit, list[Bit]]:
        """
        Depending on the type of the index, returns either a single bit or a UInt containing the bits at the given index.
        """
        bits = self.bits[index]
        if type(index) is slice:
            return UInt.from_exprs(bits)
        else:
            return bits

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
        sum = self ^ other
        carry = self & other
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
        negated = ~other + UInt.from_exprs([Bit(0)] * (self.width() - 1) + [Bit(1)])
        return self + negated

    def __mul__(self, other) -> "UInt":
        """
        Unsigned, modular multiplication.
        """
        self.assert_similar(other)
        a, b = self, other
        zero = UInt.from_value(0, self.width())
        prod = zero
        while a != zero:
            prod += UInt.from_exprs([(Bit(1) & a[0]).simplify()] * self.width()) & b
            a >>= 1
            b <<= 1
        return prod

    def __and__(self, other) -> "UInt":
        """
        Bitwise `and` operation
        """
        self.assert_similar(other)
        return UInt.from_exprs(
            [(a & b).simplify() for a, b in zip(self.bits, other.bits)]
        )

    def __or__(self, other) -> "UInt":
        """
        Bitwise `or` operation
        """
        self.assert_similar(other)
        return UInt.from_exprs(
            [(a | b).simplify() for a, b in zip(self.bits, other.bits)]
        )

    def __xor__(self, other) -> "UInt":
        """
        Bitwise `xor` operation
        """
        self.assert_similar(other)
        return UInt.from_exprs(
            [(a ^ b).simplify() for a, b in zip(self.bits, other.bits)]
        )

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
    def from_exprs(cls, exprs: list[Expr]) -> "UInt":
        """
        Constructs a UInt from the list of expressions, interpreting each expression as a bit. Note that the first
        expression in the list will become the most significant bit.

        Raises a ValueError if `exprs` is not a list of expressions.
        """
        if not all(isinstance(expr, Expr) for expr in exprs):
            raise ValueError(f"Expected a list of expressions, got {exprs}.")
        uint = cls.__new__(cls)
        uint.bits = exprs
        return uint

    @classmethod
    def from_value(cls, value: int, width: int = None) -> "UInt":
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

    def print(self, bits_per_line):
        """
        Prints the value in chunks of `bits_per_line` bits
        """
        for i in range(0, len(self.bits), bits_per_line):
            print(
                " ".join(str(bit) for bit in reversed(self.bits[i : i + bits_per_line]))
            )

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
        rotated = self.bits[n:] + self.bits[:n]
        return UInt.from_exprs(rotated)

    def rotate_right(self, n) -> "UInt":
        """
        Rotates the bits right by `n` positions
        """
        assert n >= 0, "Shift must be non-negative."
        rotated = self.bits[-n:] + self.bits[:-n] if n > 0 else self.bits
        return UInt.from_exprs(rotated)

    def set_width(self, width):
        """
        Sets the bit width to `width`. If `width` is less than the current width, the most significant bits will be removed.
        If `width` is greater than the current width, the `UInt` will be zero-extended.
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
