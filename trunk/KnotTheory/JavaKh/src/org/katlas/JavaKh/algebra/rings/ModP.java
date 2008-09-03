package org.katlas.JavaKh.algebra.rings;

import java.io.Serializable;

import org.katlas.JavaKh.algebra.Ring;

public class ModP implements Ring<ModP>, Serializable {
    /**
	 * 
	 */
	private static final long serialVersionUID = 6755912305388859426L;
	static int p, inv[];
    final int n;

    public static void setP(int i) {
	p = i;
	inv = new int[p];
	for (int j = 1; j < p; j++)
	    for (int k = 1; k < p; k++)
		if ((j * k) % p == 1) {
		    inv[j] = k;
		    break;
		}
    }

    public ModP(int i) {
	int n_ = i % p;
	if (n_ < 0)
	    n_ += p;
	n = n_;
    }

    public boolean equals(Object o) {
	if (!(o instanceof ModP))
	    return false;
	ModP mp = (ModP) o;
	return n == mp.n;
    }

    public boolean isInvertible() {
	return n != 0;
    }

    public ModP inverse() {
	assert inv[n] != 0;
	return new ModP(inv[n]);
    }

    public ModP multiply(ModP mp) {
	return new ModP(n * mp.n);
    }

    public ModP multiply(int n) {
	return new ModP(this.n * n);
    }

    public ModP add(ModP mp) {
	return new ModP(n + mp.n);
    }

    public boolean isZero() {
	return n == 0;
    }

    public String toString() {
	return Integer.toString(n);
    }
}
