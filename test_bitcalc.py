from bitcalc import TT, Bit, Expr, Not, And, Or, Xor, UInt
from functools import reduce


def test_expr_anf():
    a = Bit("a")
    b = Bit("b")
    c = Bit("c")
    one = Bit(1)

    assert a.to_anf() == a
    assert (~a).to_anf() == one ^ a
    assert (a | b).to_anf() == a ^ b ^ (a & b)
    assert (~(a | b)).to_anf() == one ^ a ^ b ^ (a & b)
    assert ((~a & ~c) | ~b | ~c).to_anf() == one ^ (b & c)
    assert ((Bit(1) ^ ~b) | (~b ^ ~c) | ~a | ~c).to_anf() == one
    assert ((a | b) & c).to_anf() == (a & c) ^ (b & c) ^ (a & b & c)
    assert ((a | b) ^ (b | c)).to_anf() == a ^ c ^ (a & b) ^ (b & c)
    assert ((a ^ c) | (c ^ ~b)).to_anf() == b ^ (a & b) ^ (a & c) ^ (b & c) ^ one


def test_expr_eq():
    a = Bit("a")
    b = Bit("b")
    c = Bit("c")
    one = Bit(1)
    zero = Bit(0)

    assert a == a
    assert a != b
    assert ~a != one
    assert a & b != a | b
    assert a & ~a == zero
    assert a | ~a == one
    assert a & zero == zero
    assert a | one == one
    assert a ^ a == zero
    assert a ^ zero == a

    # Double negation
    assert a == ~(~a)

    # Commutativity
    assert a & b == b & a
    assert a | b == b | a
    assert a ^ b == b ^ a

    # Associativity
    assert a & (b & c) == (a & b) & c == a & b & c
    assert a | (b | c) == (a | b) | c == a | b | c
    assert a ^ (b ^ c) == (a ^ b) ^ c == a ^ b ^ c

    # Distributivity
    assert a & (b | c) == (a & b) | (a & c)
    assert a | (b & c) == (a | b) & (a | c)

    # Idempotence
    assert a & a == a
    assert a | a == a
    assert a ^ a == Bit(0)

    # Absorption
    assert a & a == a
    assert a | a == a
    assert a & (a | b) == a
    assert a & one == a
    assert a | zero == a
    assert (a | b) | (b | c) == a | b | c

    # De Morgan's laws
    assert ~(a & b) == ~a | ~b

    # Misc.
    assert one ^ b ^ (a & b) != one
    assert (b | (c ^ (~b & ~c))) != ~c


def test_expr_fuzz():
    from random import choices, randrange

    BITS = 3
    MIN_EQUAL = 30
    TEST_INEQUAL = 10

    exprs, equal = {}, 0

    def rand_literal() -> Expr:
        x = chr(ord("a") + randrange(BITS))
        match randrange(2):
            case 0:
                return Bit(x)
            case 1:
                return ~Bit(x)
            case _:
                raise ValueError("Invalid random value")

    def rand_op(a: Expr, b: Expr) -> "Expr":
        match randrange(3):
            case 0:
                return a & b
            case 1:
                return a | b
            case 2:
                return a ^ b
            case _:
                raise ValueError("Invalid random value")

    def rand_expr() -> "Expr":
        expr = Expr.empty()
        while len(expr.vars()) < BITS:
            lit = rand_literal()
            expr = rand_op(expr, lit)
            print(repr(expr))
        return expr

    # Loop while the number of equal expressions found is less than MIN_EQUAL.
    while equal < MIN_EQUAL:
        # Generate a random expression.
        expr = rand_expr()

        # Simplify
        simp = expr.simplify()

        # Convert the expression to ANF, CNF, DNF and verify that they are equal.
        anf = expr.to_anf()
        cnf = expr.to_cnf()
        dnf = expr.to_dnf()

        assert simp == expr
        assert anf == expr
        assert cnf == expr
        assert dnf == expr
        assert anf == cnf
        assert anf == dnf
        assert cnf == dnf

        # Verify that the expressions are equal for all possible bit combinations.
        tt, vars = 0, [chr(ord("a") + i) for i in range(BITS)]
        for bits in range(1 << BITS):
            e = expr.eval(bits, vars)
            a = anf.eval(bits, vars)
            c = cnf.eval(bits, vars)
            d = dnf.eval(bits, vars)
            assert e in (0, 1)
            assert e == a == d == c
            tt |= e << bits

        if tt in exprs:
            assert expr == exprs[tt]
            equal += 1
        else:
            exprs[tt] = expr

        # Assert that every anf with a different truth table is not equal to the current expression.
        others = [o for k, o in exprs.items() if k != tt]
        if len(others) > TEST_INEQUAL:
            others = choices(others, k=TEST_INEQUAL)
        for other in others:
            assert expr != other


def test_expr_init():
    a = Bit("a")
    b = Bit("b")
    one = Bit(1)
    zero = Bit(0)

    not_one = Not(one)
    assert type(not_one) is Bit
    assert str(not_one) == "0"

    and_a1 = And([a, one])
    assert type(and_a1) is Bit
    assert str(and_a1) == "a"

    and_ab0 = And([a, b, zero])
    assert type(and_ab0) is Bit
    assert str(and_ab0) == "0"

    or_ab1 = Or([a, b, one])
    assert type(or_ab1) is Bit
    assert str(or_ab1) == "1"

    or_ab0 = Or([a, b, zero])
    assert type(or_ab0) is Or
    assert str(or_ab0) == "(a | b)"

    xor_ab01 = Xor([a, b, zero, one])
    assert type(xor_ab01) is Xor
    assert str(xor_ab01) == "(1 ^ a ^ b)"

    xor_ab00 = Xor([a, b, zero, zero])
    assert type(xor_ab00) is Xor
    assert str(xor_ab00) == "(a ^ b)"

    xor_ab001 = Xor([a, b, zero, zero, one])
    assert type(xor_ab001) is Xor
    assert str(xor_ab001) == "(1 ^ a ^ b)"


def test_expr_repr():
    a = Bit("a")
    b = Bit("b")
    c = Bit("c")
    one = Bit(1)

    assert repr(a) == "Bit('a')"
    assert repr(one) == "Bit(1)"
    assert repr(~a) == "Not(Bit('a'))"
    assert repr(a & b) == "And(Bit('a'), Bit('b'))"
    assert repr(b | a) == "Or(Bit('b'), Bit('a'))"
    assert repr((a | b) & c) == "And(Or(Bit('a'), Bit('b')), Bit('c'))"
    assert repr((a ^ b) & c) == "And(Xor(Bit('a'), Bit('b')), Bit('c'))"


def test_expr_simplify():
    a = Bit("a")
    b = Bit("b")
    c = Bit("c")

    def check(expr: Expr):
        simp = expr.simplify()
        assert simp == expr
        assert len(str(simp)) < len(str(expr))

    check((~b ^ (b & c & ~b & (b ^ b ^ ~c))))
    check((a ^ a ^ b ^ b ^ b ^ c ^ ~a ^ ~a))


def test_expr_str():
    a = Bit("a")
    b = Bit("b")
    c = Bit("c")

    assert str(a) == "a"
    assert str(~a) == "~a"
    assert str(a & b) == "(a & b)"
    assert str(b | a) == "(a | b)"
    assert str(a ^ b) == "(a ^ b)"
    assert str((c | b) & a) == "(a & (b | c))"
    assert str((a & b) | c) == "(c | (a & b))"
    assert str((c & b) ^ a) == "(a ^ (b & c))"


def test_expr_to_int():
    assert Bit(0).to_int() == 0
    assert Bit(1).to_int() == 1
    assert Bit("a").to_int() is None


def test_expr_tt():
    a = Bit("a")
    b = Bit("b")
    c = Bit("c")
    zero = Bit(0)

    assert a.to_tt() == TT([1], ["a"])
    assert (~a).to_tt() == TT([0], ["a"])
    assert (~a).to_tt() != zero.to_tt()
    assert (a & b).to_tt() == TT([3], ["a", "b"])
    assert (~(a & b)).to_tt() == TT([0, 1, 2], ["a", "b"])
    assert (a | b).to_tt() == TT([1, 2, 3], ["a", "b"])
    assert (a ^ b).to_tt() == TT([1, 2], ["a", "b"])
    assert ((Bit(1) ^ ~b) | (~b ^ ~c) | ~a | ~c).to_tt().prune() == TT([0], [])


def test_uint_reprs():
    x = UInt("x", 2)
    assert repr(x) == "UInt([Bit('x[0]'), Bit('x[1]')])"
    assert str(x) == "x[1] x[0]"


def test_uint_simple():
    import pytest

    print("Testing simple uint ops...")
    # Basic operations:

    one = UInt(1, 2)
    two = UInt(2, 2)
    assert int(one) == 1
    assert int(two) == 2
    assert one + one == two
    assert two - one == one
    assert two * one == two

    # Overflow:
    assert UInt(3, 2) + UInt(3, 2) == UInt(2, 2)

    a, b, c, d = Bit("a"), Bit("b"), Bit("c"), Bit("d")
    _0 = Bit(0)
    _1 = Bit(1)

    x = UInt([d, c, b, a])
    y = UInt(0b1100)

    assert int(y) == 0b1100
    assert x & y == UInt([d, c, _0, _0])
    assert x | y == UInt([_1, _1, b, a])
    assert x ^ y == UInt([~d, ~c, b, a])
    assert ~x == UInt([~d, ~c, ~b, ~a])
    assert x << 2 == UInt([b, a, _0, _0])
    assert x >> 2 == UInt([_0, _0, d, c])
    assert x.reverse_bits() == UInt([a, b, c, d])
    assert x.rotate_left(1) == UInt([c, b, a, d])
    assert x.rotate_right(1) == UInt([a, d, c, b])

    with pytest.raises(ValueError):
        int(x)  # Cannot convert to int if not all bits are known

    with pytest.raises(ValueError):
        x.assert_similar(a)  # UInt and Bit are not similar

    with pytest.raises(ValueError):
        x.assert_similar(one)  # UInts with different widths are not similar


def test_uint_fuzz():
    from random import randrange

    COUNT = 1000
    WIDTH = 8
    MAX = 2**WIDTH
    MASK = MAX - 1

    def rotl(x, n):
        n %= WIDTH
        return trunc((x << n) | (x >> (WIDTH - n)))

    def rotr(x, n):
        n %= WIDTH
        return trunc((x >> n) | (x << (WIDTH - n)))

    def shl(x, n):
        return trunc(x << n)

    def shr(x, n):
        return trunc(x >> n)

    def trunc(x):
        return MASK & x

    for _ in range(COUNT):
        [a, b] = sorted([randrange(MAX), randrange(MAX)])

        uint_a = UInt(a, WIDTH)
        uint_b = UInt(b, WIDTH)

        assert trunc(a) == int(uint_a)
        assert trunc(b) == int(uint_b)
        assert trunc(shl(a, 2)) == int(uint_a << 2)
        assert trunc(shr(a, 2)) == int(uint_a >> 2)
        assert trunc(rotl(a, 2)) == int(uint_a.rotate_left(2))
        assert trunc(rotr(a, 2)) == int(uint_a.rotate_right(2))

        assert trunc(a + b) == int(uint_a + uint_b)
        assert trunc(b - a) == int(uint_b - uint_a)
        assert trunc(a * b) == int(uint_a * uint_b)
        assert trunc(a & b) == int(uint_a & uint_b)
        assert trunc(a | b) == int(uint_a | uint_b)
        assert trunc(a ^ b) == int(uint_a ^ uint_b)
        assert trunc(~a) == int(~uint_a)


def test_uint_matmul8x8():
    COL = UInt(0x0101010101010101, 64)
    ROW = UInt(0x00000000000000FF, 64)

    a = UInt("a", 64)
    b = UInt("b", 64)

    res = (COL & a) * (ROW & b)
    res ^= (COL & a >> 1) * (ROW & b >> 8)
    res ^= (COL & a >> 2) * (ROW & b >> 16)
    res ^= (COL & a >> 3) * (ROW & b >> 24)
    res ^= (COL & a >> 4) * (ROW & b >> 32)
    res ^= (COL & a >> 5) * (ROW & b >> 40)
    res ^= (COL & a >> 6) * (ROW & b >> 48)
    res ^= (COL & a >> 7) * (ROW & b >> 56)

    print8x8(res)

    def idx(i: int, j: int):
        return f"{(8 * i + j)}".zfill(2)

    for i in range(8):
        for j in range(8):
            a_bits = [Bit(f"a[{idx(i,k)}]") for k in range(8)]
            b_bits = [Bit(f"b[{idx(k,j)}]") for k in range(8)]
            parts = [a & b for a, b in zip(a_bits, b_bits)]
            sum = reduce(lambda x, y: x ^ y, parts)
            assert res[i * 8 + j] == sum


def test_uint_transpose8x8():
    labels = [[f"{chr(ord("a") + j)}{i}" for i in reversed(range(8))] for j in range(8)]
    rows = [UInt([Bit(label) for label in row]) for row in labels]

    def delta_swap(x: "UInt", mask: "UInt", shift: int) -> "UInt":
        t = ((x >> shift) ^ x) & mask
        res = (x ^ t) ^ (t << shift)
        for i, bit in enumerate(res):
            res[i] = bit.to_anf()
        return res

    mask0 = UInt(0x00AA00AA00AA00AA, 64)
    mask1 = UInt(0x0000CCCC0000CCCC, 64)
    mask2 = UInt(0x00000000F0F0F0F0, 64)

    m = reduce(lambda x, y: x.concat(y), rows)
    print8x8(m)
    t = delta_swap(m, mask0, 7)
    print8x8(t)
    t = delta_swap(t, mask1, 14)
    print8x8(t)
    t = delta_swap(t, mask2, 28)
    print8x8(t)

    for i in range(8):
        for j in range(8):
            assert m[i * 8 + j] == t[j * 8 + i]


def print8x8(matrix: UInt):
    print()
    for i in range(8):
        s = i * 8
        e = s + 8
        bits = matrix[s:e]
        print(" ".join([str(bit) for bit in bits]))
