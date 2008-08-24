package org.katlas.JavaKh.utils;

public class TrivialCache<E> implements Cache<E> {

	private int count = 0;
	
	@Override
	public E cache(E e) {
		// System.out.println((++count) + e.hashCode());
		return e;
	}

	@Override
	public void flush() {
	}

	@Override
	public int size() {
		return 0;
	}

	
}
