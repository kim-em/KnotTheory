/**
 * 
 */
package org.katlas.JavaKh;

interface CobMatrixRow {

	void put(int j, LCCC lc);
	Iterable<Integer> keys();
	LCCC get(int j);
	boolean containsKey(int k);
	void remove(int i);
	void compact();

}