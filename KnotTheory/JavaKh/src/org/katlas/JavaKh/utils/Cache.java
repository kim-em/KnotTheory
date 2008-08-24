package org.katlas.JavaKh.utils;

public interface Cache<E> {
	
	public E cache(E e);
	public int size();
	public void flush();
	public int getNumberOfChecks();
	public int getNumberOfHits();
	
}
