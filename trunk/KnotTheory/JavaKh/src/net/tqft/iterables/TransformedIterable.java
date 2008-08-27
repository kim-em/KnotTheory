/*
 * Created on Oct 31, 2005
 */
package net.tqft.iterables;

import java.util.Iterator;

import net.tqft.iterables.interfaces.Transformer;

public abstract class TransformedIterable<S, T> implements Iterable<T>, Transformer<S, T> {

	private final Iterable<? extends S> iterable;
	
	public TransformedIterable(Iterable<? extends S> iterable) {
		this.iterable = iterable;
	}

	public abstract T evaluate(S s);
	
	public Iterator<T> iterator() {
		return Iterables.transform(iterable.iterator(), this);
	}
	
}
