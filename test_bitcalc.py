from bitcalc import Expr, Bit, Not, And, Or, Xor, UInt


def test_eq():
    a = Bit("a")
    b = Bit("b")
    c = Bit("c")

    assert a == a
    assert a != b

    # Double negation
    assert a == Not(Not(a))

    # Commutativity
    assert And(a, b) == And(b, a)
    assert Or(a, b) == Or(b, a)
    assert Xor(a, b) == Xor(b, a)

    # Associativity
    assert And(a, And(b, c)) == And(And(a, b), c)
    assert Or(a, Or(b, c)) == Or(Or(a, b), c)
    assert Xor(a, Xor(b, c)) == Xor(Xor(a, b), c)

    # Distributivity
    assert And(a, Or(b, c)) == Or(And(a, b), And(a, c))
    assert Or(a, And(b, c)) == And(Or(a, b), Or(a, c))

    # Idempotence
    assert And(a, a) == a
    assert Or(a, a) == a
    assert Xor(a, a) == Bit("0")

    # Absorption
    assert Or(a, And(a, b)) == a

    # De Morgan's laws
    assert Not(And(a, b)) == Or(Not(a), Not(b))


def test_uint_simple():
    print("Testing simple uint ops...")
    # Basic operations:

    one = UInt(1, 2)
    two = UInt(2, 2)
    assert int(one) == 1
    assert int(two) == 2
    assert one + one == two
    assert two * one == two

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


def test_simplify():
    a = Bit("a")

    def test(expr: "Expr", expect_repr: str):
        assert str(expr.simplify()) == expect_repr

    # Simplify a AND a
    test(And(a, a), "a")

    # Simplify a OR a
    test(Or(a, a), "a")

    # Simplify a AND NOT a
    test(And(a, Not(a)), "0")

    # Simplify a OR NOT a
    test(Or(a, Not(a)), "1")

    # Simplify a AND 1
    test(And(a, Bit("1")), "a")

    # Simplify a OR 0
    test(Or(a, Bit("0")), "a")

    # Simplify a AND 0
    test(And(a, Bit("0")), "0")

    # Simplify a OR 1
    test(Or(a, Bit("1")), "1")

    # Simplify a XOR a
    test(Xor(a, a), "0")

    # Simplify a XOR 0
    test(Xor(a, Bit("0")), "a")
