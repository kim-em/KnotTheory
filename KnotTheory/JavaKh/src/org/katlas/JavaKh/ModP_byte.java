package org.katlas.JavaKh;

public class ModP_byte extends BaseRing {
    static byte p, inv[];
    byte n;

    public static void setP(int i) {
    if(i < -128 || i > 127) {
    	throw new IllegalArgumentException();
    }
	p = (byte)i;
	inv = new byte[p];
	for (int j = 1; j < p; j++)
	    for (byte k = 1; k < p; k++)
		if ((j * k) % p == 1) {
		    inv[j] = k;
		    break;
		}
    }

    public ModP_byte(int i) {
	n = (byte)(i % p);
	if (n < 0)
	    n += p;
    }

    public boolean equals(Object o) {
	if (!(o instanceof ModP_byte))
	    return false;
	ModP_byte mp = (ModP_byte) o;
	return n == mp.n;
    }

    public boolean isInvertible() {
	return n != 0;
    }

    public BaseRing inverse() {
	assert inv[n] != 0;
	return new ModP_byte(inv[n]);
    }

    public BaseRing multiply(BaseRing br) {
	ModP_byte mp = (ModP_byte) br;
	return new ModP_byte((int)n * (int)(mp.n));
    }

    public BaseRing multiply(int n) {
	return new ModP_byte((int)(this.n) * n);
    }

    public BaseRing add(BaseRing br) {
	ModP_byte mp = (ModP_byte) br;
	return new ModP_byte((int)n + (int)(mp.n));
    }

    public boolean isZero() {
	return n == 0;
    }

    public String toString() {
	return Integer.toString(n);
    }
}
