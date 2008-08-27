/*
 * Created on Oct 26, 2005
 */
package net.tqft.iterables;

import java.io.Serializable;
import java.util.Iterator;

final class EmptyIterable implements Iterable<Object>, Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -4424019291474175172L;

	@SuppressWarnings("unchecked")
	static <T> Iterable<T> getInstance() {
		return (Iterable<T>) EMPTY_ITERABLE;
	}
	
	private static final Iterable<Object> EMPTY_ITERABLE = new EmptyIterable();
	
	// don't let other people create instances
	private EmptyIterable() { }
	
	public Iterator<Object> iterator() {
		return Iterables.emptyIterator();
	}

    // Preserves singleton property
    private Object readResolve() {
        return EMPTY_ITERABLE;
    }
	
}
