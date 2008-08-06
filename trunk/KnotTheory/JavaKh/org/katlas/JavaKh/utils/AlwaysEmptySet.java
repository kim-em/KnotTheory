package org.katlas.JavaKh.utils;
import java.io.Serializable;
import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;


public class AlwaysEmptySet<V> implements Set<V>, Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1283740797021223218L;

	@Override
	public boolean add(V arg0) {
		return true;
	}

	@Override
	public boolean addAll(Collection<? extends V> arg0) {
		return true;
	}

	@Override
	public void clear() {
		
	}

	@Override
	public boolean contains(Object arg0) {
		return false;
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		return c.isEmpty();
	}

	@Override
	public boolean isEmpty() {
		return true;
	}

	@Override
	public Iterator<V> iterator() {
		return new Iterator<V>() {

			@Override
			public boolean hasNext() {
				return false;
			}

			@Override
			public V next() {
				throw new NoSuchElementException();
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		};
	}

	@Override
	public boolean remove(Object o) {
		return false;
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		return false;
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		return false;
	}

	@Override
	public int size() {
		return 0;
	}

	@Override
	public Object[] toArray() {
		return new Object[] {};
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T> T[] toArray(T[] a) {
		return (T[])(new Object[] {});
	}

}
