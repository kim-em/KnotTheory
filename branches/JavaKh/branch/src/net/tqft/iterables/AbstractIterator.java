/*
 * Created on Jan 5, 2006
 */
package net.tqft.iterables;

import java.util.Iterator;
import java.util.NoSuchElementException;

public abstract class AbstractIterator<T> implements Iterator<T> {

	public abstract boolean hasNext();
	protected abstract T returnNext();
	
	public T next() {
		if(!hasNext()) throw new NoSuchElementException();
		return returnNext();
	}

	public void remove() {
		throw new UnsupportedOperationException();
	}

}
