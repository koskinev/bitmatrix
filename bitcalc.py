from copy import copy
from typing import Union


def expr_eq(a: "Expr", b: "Expr", depth=0) -> bool:
    match a.eval(), b.eval():
        # Bit to Bit
        # a = a
        case (Bit(a), Bit(b)):
            return a == b

        # Negated Bit to Negated Bit or double negated Bit to Bit
        # ~a = ~a, ~(~a) = a
        case (Not(a), Not(b)) | (Not(Not(a)), b):
            return a == b

        # Distributive laws
        # a & (b | c) == (a & b) | (a & c)
        case (And([a, Or(b)]), Or(c)) | (And([a, Xor(b)]), Xor(c)):
            return all(type(x) is And and a in x.x for x in c)

        # Associative and commutative laws
        # a & b & c == c & b & a, a | b | c == c | b | a, ...
        case a, b if type(a) is type(b):
            (x, y) = (a.x, b.x)
            return all(a in y for a in x) and all(b in x for b in y)

        # De Morgan's laws
        # ~(a & b) = ~a | ~b, ~(a | b) = ~a & ~b, ...
        case (Not(And(a)), Or(b)) | (Not(Or(a)), And(b)):
            return all(Not(x) in b for x in a) and all(Not(y) in a for y in b)

        case _ if depth == 0:
            return expr_eq(b, a, 1)
        case _:
            return False


class Expr:
    __match_args__ = ("x",)
    x: Union[int, str, "Expr", list["Expr"]]

    def anf(self) -> "Expr":
        """
        Returns the algebraic normal form of the expression
        """
        match self:
            case Bit(_):
                return self
            case Not(x):
                return Xor(Bit(1), x.anf())
            case And(x) if any(x == Bit(0) for x in x):
                return Bit(0)
            case And(x) if all(type(x) is Bit for x in x):
                bits = []
                for expr in x:
                    if expr != Bit(1) and expr not in bits:
                        bits.append(expr)
                if len(bits) < 2:
                    return bits[0]
                else:
                    return And(*bits)
            case Or(x) if any(x == Bit(1) for x in x):
                return Bit(1)
            case Xor(x):
                anfs = []
                rest = []
                for expr in x:
                    if type(expr) is Bit and expr not in anfs:
                        anfs.append(expr)
                    if (
                        type(expr) is And
                        and all(type(x) is Bit for x in expr.x)
                        and expr not in anfs
                    ):
                        anfs.append(expr)
                    else:
                        rest.append(expr.anf())
                return Xor(*anfs, *rest)
            case _ if len(self.vars()) > 1:
                var = self.vars().pop()
                x0 = self.subst(var, Bit(0))
                x1 = self.subst(var, Bit(1))
                a = x0.anf()
                b = And(Bit(var), x0).anf()
                c = And(Bit(var), x1).anf()
                return Xor(a, b, c)
            case _:
                return self

    def eval(self) -> "Expr":
        """
        Evaluates the expression
        """
        return NotImplementedError(
            "The eval method should be implemented by the expression type."
        )

    @property
    def name(self) -> str:
        """
        Returns the expression type as a string
        """
        return type(self).__name__

    def subst(self, var: str, expr: "Expr") -> "Expr":
        """
        Substitutes the variable with the given expression and returns the resulting expression.
        If the variable is not found, returns the original expression.
        """
        return NotImplementedError(
            "The subst method should be implemented by the expression type."
        )

    def vars(self) -> set[str]:
        """
        Returns the set of all the variables in the expression
        """
        match self:
            case Bit(x) if type(x) is str:
                return {x}
            case Bit(_):
                return {}
            case NaryExpr(x):
                return set().union(*[x.vars() for x in x])
            case _:
                return self.x.vars()

    def __eq__(self, other: "Expr") -> bool:
        return repr(self.anf()) == repr(other.anf())
        # return expr_eq(self, other)

    def __lt__(self, other: "Expr") -> bool:
        match self, other:
            case Bit(_), Bit(_):
                return self.x < other.x
            case Bit(_), Expr(_):
                return True
            case Expr(_), Bit(_):
                return False
            case NaryExpr(_), NaryExpr(_) if type(self) is type(other):
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

    def __or__(self, other: "Expr") -> "Expr":
        return Or(self, other)

    def __xor__(self, other: "Expr") -> "Expr":
        return Xor(self, other)

    def __invert__(self) -> "Expr":
        return Not(self)


class NaryExpr(Expr):
    x: list["Expr"]

    def __init__(self, *args: "Expr"):
        assert len(args) > 1 and all(
            isinstance(a, Expr) for a in args
        ), f"Invalid argument for {self.name}: {args}. Expected a list of expressions."
        x = []
        for a in args:
            if type(a) is type(self):
                x.extend(a.x)
            else:
                x.append(a)
        self.x = x


class Bit(Expr):
    x: int | str

    def __init__(self, x: int | str):
        assert x in (0, 1) or isinstance(
            x, str
        ), f"Invalid argument: {x}. Expected 0, 1, or a string."
        self.x = x

    def __repr__(self) -> str:
        return f"Bit({self.x})"

    def __str__(self) -> str:
        return f"{self.x}"

    def eval(self) -> "Expr":
        return self

    def subst(self, var: str, expr: "Expr") -> "Expr":
        if self.x == var:
            return copy(expr)
        else:
            return self


class Not(Expr):
    x: "Expr"

    def __init__(self, x: "Expr"):
        assert isinstance(x, Expr), f"Invalid argument: {x}. Expected an expression."
        self.x = x.eval()

    def __repr__(self) -> str:
        return f"Not({repr(self.x)})"

    def __str__(self) -> str:
        match self.x:
            case Bit(0 as x) | Bit(1 as x):
                return f"{x ^ 1}"
            case x:
                return f"(~{x})"

    def eval(self) -> "Expr":
        match self.x:
            case Bit(0 as x) | Bit(1 as x):
                return Bit(x ^ 1)
            case Not(a):
                return a
            case x:
                return Not(x)

    def subst(self, var: str, expr: "Expr") -> "Expr":
        result = self.x.subst(var, expr)
        match result:
            case Bit(0 as x) | Bit(1 as x):
                return Bit(x ^ 1)
            case x:
                return Not(x)


class And(NaryExpr):
    def __repr__(self) -> str:
        return f"And({repr([repr(a) for a in sorted(self.x)])})"

    def __str__(self) -> str:
        return f"({'&'.join([str(a) for a in sorted(self.x)])})"

    def eval(self) -> "Expr":
        res = [x.eval() for x in self.x]
        # If any of the operands is 0 or an operand is the negation of another, return 0
        if any(x == Bit(0) or Not(x) in res for x in res):
            return Bit(0)
        # Remove duplicates and 1s
        res = [x for i, x in enumerate(res) if x not in res[:i] and x != Bit(1)]
        match res:
            # If there is only one operand, return it
            case [x]:
                return x
            # If there are no operands, return 1
            case []:
                return Bit(1)
            # Otherwise, return the remaining operands as an And expression
            case _:
                return And(*res)

    def subst(self, var: str, expr: "Expr") -> "Expr":
        res = []
        for x in self.x:
            r = x.subst(var, expr)
            if r == Bit(0):
                return Bit(0)
            elif r != Bit(1):
                res.append(r)
        return And(*res)


class Or(NaryExpr):
    def __repr__(self) -> str:
        return f"Or({repr([repr(a) for a in sorted(self.x)])})"

    def __str__(self) -> str:
        return f"({'|'.join([str(a) for a in sorted(self.x)])})"

    def eval(self) -> "Expr":
        res = [x.eval() for x in self.x]
        # If any of the operands is 1 or an operand is the negation of another, return 1
        if any(x == Bit(1) or Not(x) in res for x in res):
            return Bit(1)
        # Remove duplicates and 0s
        res = [x for i, x in enumerate(res) if x not in res[:i] and x != Bit(0)]
        match res:
            # If there is only one operand, return it
            case [x]:
                return x
            # If there are no operands, return 0
            case []:
                return Bit(0)
            # Otherwise, return the remaining operands as an Or expression
            case _:
                return Or(*res)

    def subst(self, var: str, expr: "Expr") -> "Expr":
        res = []
        for x in self.x:
            r = x.subst(var, expr)
            if r == Bit(1):
                return Bit(1)
            elif r != Bit(0):
                res.append(r)
        if len(res) == 0:
            return Bit(0)
        elif len(res) == 1:
            return res[0]
        else:
            return Or(*res)


class Xor(NaryExpr):
    def __repr__(self) -> str:
        return f"Xor({repr([repr(a) for a in sorted(self.x)])})"

    def __str__(self) -> str:
        return f"({'^'.join([str(a) for a in sorted(self.x)])})"

    def eval(self) -> "Expr":
        res = [x.eval() for x in self.x]
        while True:
            # Filter out duplicate and negated operands
            dups = [x for x in res if res.count(x) > 1]
            negs = [x for x in res if Not(x) in res]
            res = [x for x in res if x not in dups and x not in negs]
            # If there are no duplicates or negations, break
            if not dups and not negs:
                break
            # Otherwise, replace each pair of duplicates with a 0 and each pair of negations with a 1
            else:
                res += [Bit(0)] * (len(dups) // 2)
                res += [Bit(1)] * (len(negs) // 2)
        match res:
            # If there is only one operand, return it
            case [x]:
                return x
            # If there are two operands and either is 0, return the other
            case [Bit(0), x] | [x, Bit(0)]:
                return x
            # If there are two operands and either is 1, return the negation of the other
            case [Bit(1), x] | [x, Bit(1)]:
                return Not(x)
            # Otherwise, return the remaining operands as an Xor expression
            case _:
                return Xor(*res)

    def subst(self, var: str, expr: "Expr") -> "Expr":
        res = []
        for x in self.x:
            r = x.subst(var, expr)
            if r == Bit(0):
                continue
            else:
                res.append(r)
        return Xor(*res)


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
        UInt([Bit(a), Bit(1), Bit(0), Bit(1)])  # A 4-bit UInt representing the value `0ba010`, where `a` is an unknown bit.
        ```
        """
        if type(bits) is int:
            this = UInt.from_value(bits, width)
        elif type(bits) is str:
            this = UInt.from_label(bits, width)
        else:
            this = UInt.from_exprs(bits)
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
            prod += UInt.from_exprs([Bit(1) & a[0]] * self.width()) & b
            a >>= 1
            b <<= 1
        return prod

    def __and__(self, other) -> "UInt":
        """
        Bitwise `and` operation
        """
        self.assert_similar(other)
        return UInt.from_exprs([(a & b).eval() for a, b in zip(self.bits, other.bits)])

    def __or__(self, other) -> "UInt":
        """
        Bitwise `or` operation
        """
        self.assert_similar(other)
        return UInt.from_exprs([(a | b).eval() for a, b in zip(self.bits, other.bits)])

    def __xor__(self, other) -> "UInt":
        """
        Bitwise `xor` operation
        """
        self.assert_similar(other)
        return UInt.from_exprs([(a ^ b).eval() for a, b in zip(self.bits, other.bits)])

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
        if not all(isinstance(b.eval().x, int) for b in self.bits):
            raise ValueError(f"Cannot convert to int. {self} contains non-binary bits.")
        return int("".join(str(bit) for bit in self.bits), 2)

    def assert_similar(self, other):
        """
        Checks that `other` is an UInt and has the same length as `self`. Raises
        a ValueError if the conditions are not met.
        """
        if not isinstance(other, UInt):
            raise ValueError("Operand must be an UInt.")
        if self.width() != other.width():
            raise ValueError("UInt bit widths must be equal.")

    def compress(self, mask) -> "UInt":
        """
        Moves the masked bits in `x` to the right and sets the rest to 0.
        """
        from math import log2

        x = self
        steps = int(log2(x.width()))
        assert 2**steps == x.width(), "Bit width must be a power of 2."

        x &= mask
        mk = ~mask << 1
        for i in range(steps):
            p = 1
            mp = mk ^ (mk << p)
            for _ in range(steps - 1):
                p *= 2
                mp = mp ^ (mp << p)
            mv = mp & mask
            mask = (mask ^ mv) | (mv >> (1 << i))
            t = x & mv
            x = x ^ t | (t >> (1 << i))
            mk &= ~mp
        return x

    def concat(self, other: "UInt") -> "UInt":
        """
        Concatenates two UInts
        """
        return UInt.from_exprs(self.bits + other.bits)

    def delta_swap(self, mask, shift) -> "UInt":
        """
        Returns a value with the masked bits in `self` moved to the left by `shift` positions. For this function
        to work properly, the mask and the shifted mask should not overlap, ie. `mask & (mask << shift) == 0`
        and no bits should be shifted out of the 64-bit integer, ie. `((mask << shift) >> shift) == mask`.
        """
        x = self
        t = ((x >> shift) ^ x) & mask
        return (x ^ t) ^ (t << shift)

    def delta_exchange(self, other, mask, shift) -> tuple["UInt", "UInt"]:
        """
        Exchanges the masked bits in `self` with the bits in `other` masked by `mask << shift` and returns
        resulting UInts without modifying the original values. For this function to work properly, no bits
        should be shifted out of the 64-bit integers, ie. `((mask << shift) >> shift) == mask`.
        """
        x = self
        t = ((other >> shift) ^ x) & mask
        return (x ^ t), (other ^ (t << shift))

    def exchange(self, other, mask) -> tuple["UInt", "UInt"]:
        """
        Exchanges the masked bits between `self` and `other` and returns the resulting UInts without modifying the
        original values.
        """
        x = self ^ other
        y = other ^ (x & mask)
        x = x ^ y
        return x, y

    @classmethod
    def from_exprs(cls, exprs: list[Expr]) -> "UInt":
        """
        Constructs a UInt from the list of expressions, interpreting each expression as a bit. The first expression
        in the list will be the least significant bit. Raises a ValueError if `exprs` is not a list of expressions.
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
