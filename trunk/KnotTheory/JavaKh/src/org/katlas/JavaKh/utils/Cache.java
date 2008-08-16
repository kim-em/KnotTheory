package org.katlas.JavaKh.utils;

public interface Cache<E> {
	
	public E cache(E e);
	public void flush();
	
}
