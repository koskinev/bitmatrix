from bitcalc import Expr, Bit, Not, And, Or, Xor, UInt


def test_eq():
    a = Bit("a")
    b = Bit("b")
    c = Bit("c")

    assert a == a
    assert a != b

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
    assert a & (a | b) == a

    # De Morgan's laws
    assert ~(a & b) == ~a | ~b


def test_randomized():
    from random import randrange

    BITS = 3
    MIN_COUNT = 10
    tt_exprs = {}

    def rand_bit() -> Bit:
        return Bit(chr(ord("a") + randrange(BITS)))

    def rand_op() -> "Expr":
        match randrange(3):
            case 0:
                return And
            case 1:
                return Or
            case 2:
                return Xor

    # Generate at least MIN_COUNT truth tables with at least two expressions each.
    while sum(1 for e in tt_exprs.values() if len(e) > 1) < MIN_COUNT:
        expr = rand_bit()
        while len(expr.vars()) < BITS:
            rhs = rand_op()(rand_bit(), rand_bit())
            expr = rand_op()(expr, rhs).simplify()

        tt = str(expr.truth_table())

        # If the expression is already in the dictionary, skip it.
        if repr(expr) not in [repr(e) for e in tt_exprs.get(tt, [])]:
            tt_exprs[tt] = tt_exprs.get(tt, []) + [expr]

    # Check that all expressions with the same truth table are equal.
    for tt, exprs in tt_exprs.items():
        for i, expr_a in enumerate(exprs):
            for expr_b in exprs[:i]:
                assert expr_a == expr_b

    # Keep the first expression for each truth table.
    exprs = [exprs[0] for exprs in tt_exprs.values()]

    # Check that no two expressions in the list are equal.
    for a in exprs:
        for b in exprs:
            if a == b:
                assert repr(a) == repr(b)


def test_simplify():
    a = Bit("a")

    def test(expr: "Expr", expect_repr: str):
        assert str(expr.simplify()) == expect_repr

    # Simplify a AND a
    test(a & a, "a")

    # Simplify a OR a
    test(a | a, "a")

    # Simplify a AND NOT a
    test(a & ~a, "0")

    # Simplify a OR NOT a
    test(a | ~a, "1")

    # Simplify a AND 1
    test(a & Bit(1), "a")

    # Simplify a OR 0
    test(a | Bit(0), "a")

    # Simplify a AND 0
    test(a & Bit(0), "0")

    # Simplify a OR 1
    test(a | Bit(1), "1")

    # Simplify a XOR a
    test(a ^ a, "0")

    # Simplify a XOR 0
    test(a ^ Bit(0), "a")


def test_str():
    a = Bit("a")
    b = Bit("b")
    c = Bit("c")

    assert str(a) == "a"
    assert str(Not(a)) == "~a"
    assert str(And(a, b)) == "(a & b)"
    assert str(Or(b, a)) == "(a | b)"
    assert str(Xor(a, b)) == "(a ^ b)"
    assert str(And(Or(a, b), c)) == "((a | b) & c)"
    assert str(Or(c, And(a, b))) == "((a & b) | c)"
    assert str(Xor(And(a, b), c)) == "((a & b) ^ c)"


def test_to_int():
    assert Bit(0).to_int() == 0
    assert Bit(1).to_int() == 1
    assert Bit("a").to_int() is None


def test_truth_table():
    a = Bit("a")
    b = Bit("b")

    assert a.truth_table() == [1]
    assert Not(a).truth_table() == [0]
    assert And(a, b).truth_table() == [3]
    assert Or(a, b).truth_table() == [1, 2, 3]
    assert Xor(a, b).truth_table() == [1, 2]


def test_uint_simple():
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


def test_uint_randomized():
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
