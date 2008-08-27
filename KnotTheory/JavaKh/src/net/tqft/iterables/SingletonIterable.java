/*
 * Created on Oct 25, 2005
 */
package net.tqft.iterables;

import java.util.Iterator;
import java.util.NoSuchElementException;

class SingletonIterable<T> implements Iterable<T> {

	private final T o;
	
	public SingletonIterable(T o) {
		this.o = o;
	}
	
	public Iterator<T> iterator() {
		return new Iterator<T>() {

			private boolean available = true;

			public boolean hasNext() {
				return available;
			}

			public T next() {
				if(!hasNext()) throw new NoSuchElementException();
				available = false;
				return o;
			}

			public void remove() {
				if(available) {
					throw new UnsupportedOperationException();
				} else {
					throw new IllegalStateException();
				}
			}
			
		};
	}
}
