/**
 * 
 */
package org.katlas.JavaKh.rows;


public interface MatrixRow<F> {

	void decrementIndexesAbove(int key);
	void put(int key, F f);
	Iterable<Integer> keys();
	F get(int key);
	boolean containsKey(int key);
	void remove(int key);
	void clear();
	void compact();

}