/**
 * 
 */
package org.katlas.JavaKh;

import org.katlas.JavaKh.algebra.Ring;

interface CobMatrixRow<R extends Ring<R>> {

	void put(int j, LCCC<R> lc);
	Iterable<Integer> keys();
	LCCC<R> get(int j);
	boolean containsKey(int k);
	void remove(int i);
	void compact();

}