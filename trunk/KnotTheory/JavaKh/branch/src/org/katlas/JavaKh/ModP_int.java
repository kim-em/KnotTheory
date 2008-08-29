package org.katlas.JavaKh;

import java.io.Serializable;

public class ModP_int extends BaseRing implements Serializable {
    /**
	 * 
	 */
	private static final long serialVersionUID = 6755912305388859426L;
	static int p, inv[];
    int n;

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

    public ModP_int(int i) {
	n = i % p;
	if (n < 0)
	    n += p;
    }

    public boolean equals(Object o) {
	if (!(o instanceof ModP_int))
	    return false;
	ModP_int mp = (ModP_int) o;
	return n == mp.n;
    }

    public boolean isInvertible() {
	return n != 0;
    }

    public BaseRing inverse() {
	assert inv[n] != 0;
	return new ModP_int(inv[n]);
    }

    public BaseRing multiply(BaseRing br) {
	ModP_int mp = (ModP_int) br;
	return new ModP_int(n * mp.n);
    }

    public BaseRing multiply(int n) {
	return new ModP_int(this.n * n);
    }

    public BaseRing add(BaseRing br) {
	ModP_int mp = (ModP_int) br;
	return new ModP_int(n + mp.n);
    }

    public boolean isZero() {
	return n == 0;
    }

    public String toString() {
	return Integer.toString(n);
    }
}
