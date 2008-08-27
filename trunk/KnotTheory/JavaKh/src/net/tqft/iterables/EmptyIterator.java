/*
 * Created on Nov 5, 2005
 */
package net.tqft.iterables;

import java.io.Serializable;
import java.util.Iterator;
import java.util.NoSuchElementException;

final class EmptyIterator implements Iterator<Object>, Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4447162822814554884L;

	@SuppressWarnings("unchecked")
	static <T> Iterator<T> getInstance() {
		return (Iterator<T>) EMPTY_ITERATOR;
	}
	
	private static final Iterator<Object> EMPTY_ITERATOR = new EmptyIterator();
	
	// don't let other people create instances
	private EmptyIterator() { }
	
	public boolean hasNext() {
		return false;
	}

	public Object next() {
		throw new NoSuchElementException();
	}

	public void remove() {
		throw new IllegalStateException();
	}

    // Preserves singleton property
    private Object readResolve() {
        return EMPTY_ITERATOR;
    }
}
