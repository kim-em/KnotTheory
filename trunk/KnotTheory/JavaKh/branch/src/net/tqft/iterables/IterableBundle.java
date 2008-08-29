/*
 * Created on Oct 30, 2005
 */
package net.tqft.iterables;

import java.util.Iterator;

public abstract class IterableBundle<S, T> implements Iterable<T> {

	private final Iterable<S> baseIterable;

	public IterableBundle(Iterable<S> baseIterable) {
		this.baseIterable = baseIterable;
	}

	protected abstract Iterable<T> buildNewFibreIterable(S s);

	public Iterator<T> iterator() {

		return new IteratorBundle<S, T>(baseIterable.iterator()) {
			@Override
			protected Iterator<T> buildNewFibreIterator(S s) {
				return buildNewFibreIterable(s).iterator();
			}
		};
	}
}
