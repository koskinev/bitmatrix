from bitcalc import Bit, And, Or, Xor


def test_anf_simple():
    expr = Or(Bit("x"), Bit("y"))
    anf = expr.anf()
    assert anf == Xor(Bit("x"), Bit("y"), And(Bit("x"), Bit("y")))
