package org.katlas.JavaKh.utils;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;

public class SerializableListWrapper<E extends Serializable> implements SerializingList<E> {

	private final List<E> internalList;
	
	public SerializableListWrapper(List<E> internalList) {
		this.internalList = internalList;
	}
	
	@Override
	public Iterator<ObjectInputStream> getSerializedForms() throws IOException {
		final Iterator<E> iterator = internalList.iterator();
		return new Iterator<ObjectInputStream>() {

			@Override
			public boolean hasNext() {
				return iterator.hasNext();
			}

			@Override
			public ObjectInputStream next() {
				if(hasNext()) {
					try {
						PipedInputStream pis = new PipedInputStream();
						PipedOutputStream pos = new PipedOutputStream(pis);
						ObjectOutputStream oos = new ObjectOutputStream(pos);
						oos.writeObject(iterator.next());
						ObjectInputStream ois = new ObjectInputStream(pis);
						return ois;
					} catch (IOException e) {
						e.printStackTrace();
						throw new NoSuchElementException();
					}
				} else {
					throw new NoSuchElementException();
				}
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		};
	}

	@Override
	public boolean add(E e) {
		return internalList.add(e);
	}

	@Override
	public void add(int index, E element) {
		internalList.add(index, element);
	}

	@Override
	public boolean addAll(Collection<? extends E> c) {
		return internalList.addAll(c);
	}

	@Override
	public boolean addAll(int index, Collection<? extends E> c) {
		return internalList.addAll(index, c);
	}

	@Override
	public void clear() {
		internalList.clear();
	}

	@Override
	public boolean contains(Object o) {
		return internalList.contains(o);
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		return internalList.containsAll(c);
	}

	@Override
	public E get(int index) {
		return internalList.get(index);
	}

	@Override
	public int indexOf(Object o) {
		return internalList.indexOf(o);
	}

	@Override
	public boolean isEmpty() {
		return internalList.isEmpty();
	}

	@Override
	public Iterator<E> iterator() {
		return internalList.iterator();
	}

	@Override
	public int lastIndexOf(Object o) {
		return internalList.lastIndexOf(o);
	}

	@Override
	public ListIterator<E> listIterator() {
		return internalList.listIterator();
	}

	@Override
	public ListIterator<E> listIterator(int index) {
		return internalList.listIterator(index);
	}

	@Override
	public boolean remove(Object o) {
		return internalList.remove(o);
	}

	@Override
	public E remove(int index) {
		return internalList.remove(index);
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		return internalList.removeAll(c);
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		return internalList.retainAll(c);
	}

	@Override
	public E set(int index, E element) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int size() {
		return internalList.size();
	}

	@Override
	public List<E> subList(int fromIndex, int toIndex) {
		return internalList.subList(fromIndex, toIndex);
	}

	@Override
	public Object[] toArray() {
		return internalList.toArray();
	}

	@Override
	public <T> T[] toArray(T[] a) {
		return internalList.toArray(a);
	}

	@Override
	public int hashCode() {
		return internalList.hashCode();
	}

	@Override
	public String toString() {
		return internalList.toString();
	}

}
