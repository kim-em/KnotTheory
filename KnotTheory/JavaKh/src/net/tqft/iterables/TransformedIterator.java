/*
 * Created on Oct 31, 2005
 */
package net.tqft.iterables;

import java.util.Iterator;

public abstract class TransformedIterator<S, T> extends AbstractIterator<T>
		implements Iterator<T> {

	private final Iterator<? extends S> iterator;

	public TransformedIterator(Iterator<? extends S> iterator) {
		this.iterator = iterator;
	}

	public abstract T transform(S s);

	public boolean hasNext() {
		return iterator.hasNext();
	}

	public T returnNext() {
		return transform(iterator.next());
	}

}
