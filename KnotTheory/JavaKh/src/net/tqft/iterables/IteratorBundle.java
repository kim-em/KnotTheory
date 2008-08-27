/*
 * Created on Oct 30, 2005
 */
package net.tqft.iterables;

import java.util.Iterator;

public abstract class IteratorBundle<S, T> extends AbstractIterator<T> implements Iterator<T> {

	private final Iterator<S> baseIterator;
	private Iterator<T> fibreIterator;
	
	public IteratorBundle(Iterator<S> baseIterator) {
		this.baseIterator = baseIterator;
	}
	
	protected abstract Iterator<T> buildNewFibreIterator(S s);

	public boolean hasNext() {
		while(fibreIterator == null || !fibreIterator.hasNext()) {
			if(baseIterator.hasNext()) {
				fibreIterator = buildNewFibreIterator(baseIterator.next());
			} else {
				return false;
			}
		}
		return true;
	}

	public T returnNext() {
		return fibreIterator.next();
	}

	public void remove() {
		fibreIterator.remove();
	}
}
