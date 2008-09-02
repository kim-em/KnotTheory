package org.katlas.JavaKh;

import org.katlas.JavaKh.algebra.Morphism;

public interface ICannedCobordism extends Morphism<Cap, ICannedCobordism> {

	public ICannedCobordism compose(int start, ICannedCobordism cc, int cstart, int nc);
	public boolean isIsomorphism();
	
	public void reverseMaps(); // planning to get rid of this!
	
}
