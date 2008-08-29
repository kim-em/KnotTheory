package org.katlas.JavaKh.rows;

import java.util.Iterator;

import net.tqft.iterables.AbstractIterator;

public class LinkedListRow<F> implements MatrixRow<F> {

	Entry initial;

	class Entry {
		int index;
		F value;
		Entry next;

		Entry(int index, F value) {
			this.index = index;
			this.value = value;
			this.next = null;
		}
	}

	public void compact() {
	}

	public boolean containsKey(int key) {
		Entry entry = initial;
		while (entry != null && entry.index <= key) {
			if (entry.index == key) {
				return true;
			}
			entry = entry.next;
		}
		return false;
	}

	public void decrementIndexesAbove(int j) {
		Entry entry = initial;
		while (entry != null) {
			if (entry.index > j) {
				entry.index--;
			}
			entry = entry.next;
		}
	}

	public F get(int key) {
		Entry entry = initial;
		while (entry != null && entry.index <= key) {
			if (entry.index == key) {
				return entry.value;
			}
			entry = entry.next;
		}
		return null;
	}

	public Iterable<Integer> keys() {
		return new Iterable<Integer>() {

			public Iterator<Integer> iterator() {
				return new AbstractIterator<Integer>() {

					Entry nextEntry = initial;

					@Override
					public boolean hasNext() {
						return nextEntry != null;
					}

					@Override
					protected Integer returnNext() {
						int result = nextEntry.index;
						nextEntry = nextEntry.next;
						return result;
					}

				};
			}

		};
	}
	
	public void put(int key, F f) {
		Entry entry = initial;
		if (entry == null) {
			initial = new Entry(key, f);
			return;
		}
		if(entry.index > key) {
			Entry newEntry = new Entry(key, f);
			newEntry.next = entry;
			initial = newEntry;
			return;
		}
		if(entry.index == key) {
			entry.value = f;
			return;
		}
		while (entry.next != null && entry.next.index < key) {
			entry = entry.next;
		}
		if (entry.next == null) {
			if(entry.index == key) {
				entry.value = f;
			} else {
				entry.next = new Entry(key, f);
			}
		} else {
			if (entry.next.index == key) {
				entry.next.value = f;
			} else {
				Entry newEntry = new Entry(key, f);
				newEntry.next = entry.next;
				entry.next = newEntry;
			}
		}
	}

	public void remove(int j) {
		Entry entry = initial;
		if (entry == null) {
			return;
		}
		if (entry.index == j) {
			initial = entry.next;
		} else {
			while (entry.next != null && entry.next.index < j) {
				entry = entry.next;
			}
			if (entry.next != null && entry.next.index == j) {
				entry.next = entry.next.next;
			}
		}
	}

	public void clear() {
		initial = null;
	}

}
