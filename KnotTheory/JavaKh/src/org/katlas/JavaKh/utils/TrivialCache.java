package org.katlas.JavaKh.utils;

public class TrivialCache<E> implements Cache<E> {

	private transient int checks = 0;
	
	@Override
	public E cache(E e) {
		++checks;
		return e;
	}

	@Override
	public void flush() {
	}

	@Override
	public int size() {
		return 0;
	}

	public int getNumberOfHits() {
		return 0;
	}

	public int getNumberOfChecks() {
		return checks;
	}

	
}
