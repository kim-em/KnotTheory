package org.katlas.JavaKh.utils;

public class TrivialCache<E> implements Cache<E> {

	private transient int checks = 0;
	
	public E cache(E e) {
		++checks;
		return e;
	}

	public void flush() {
	}

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
