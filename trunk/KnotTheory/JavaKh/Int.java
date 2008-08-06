import java.math.BigInteger;
public class Int extends BaseRing {
	private static final BigInteger MINUSONE = BigInteger.valueOf(-1);
	
    BigInteger n;

    public Int(int i) {
	n = BigInteger.valueOf(i);
    }

    public Int(BigInteger i) {
	n = i;
    }

    public boolean equals(Object o) {
	if (!(o instanceof Int))
	    return false;
	Int i = (Int) o;
	return n.equals(i.n);
    }

    public boolean isInvertible() {
	return n.equals(BigInteger.ONE) || n.equals(MINUSONE);
    }

    public BaseRing inverse() {
	assert isInvertible();
	return this;
    }

    public BaseRing multiply(BaseRing br) {
	Int i = (Int) br;
	return new Int(n.multiply(i.n));
    }

    public BaseRing multiply(int n) {
	return new Int(this.n.multiply(BigInteger.valueOf(n)));
    }

    public BaseRing add(BaseRing br) {
	Int i = (Int) br;
	return new Int(n.add(i.n));
    }

    public boolean isZero() {
	return n.equals(BigInteger.ZERO);
    }

    public String toString() {
	return n.toString();
    }
}
