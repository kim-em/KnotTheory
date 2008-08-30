package org.katlas.JavaKh.rows;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Iterator;

import org.apache.commons.io.IOUtils;
import org.katlas.JavaKh.CobMatrix;
import org.katlas.JavaKh.utils.SerializingList;

public class LinkedListRow<F> implements MatrixRow<F> {

	private Entry firstEntry;

	private transient Entry lastEntry;
	private transient Entry cachedEntry;
	
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
		Entry entry = firstEntry;
		while (entry != null && entry.index <= key) {
			if (entry.index == key) {
				return true;
			}
			entry = entry.next;
		}
		return false;
	}

	public void decrementIndexesAbove(int j) {
		Entry entry = firstEntry;
		while (entry != null) {
			if (entry.index > j) {
				entry.index--;
			}
			entry = entry.next;
		}
	}

	public F get(int key) {
		if(cachedEntry != null && cachedEntry.index == key) {
			return cachedEntry.value;
		}
			
		Entry entry = firstEntry;
		while (entry != null && entry.index <= key) {
			if (entry.index == key) {
				cachedEntry = entry;
				return entry.value;
			}
			entry = entry.next;
		}
		return null;
	}

	public Iterable<Integer> keys() {
		return new Iterable<Integer>() {

			public Iterator<Integer> iterator() {
				return new Iterator<Integer>() {

					Entry nextEntry = firstEntry;

					public boolean hasNext() {
						return nextEntry != null;
					}

					public Integer next() {
						int result = nextEntry.index;
						cachedEntry = nextEntry;
						nextEntry = nextEntry.next;
						return result;
					}

					public void remove() {
						throw new UnsupportedOperationException();
					}

				};
			}

		};
	}
	
	public void put(int key, F f) {
		if(lastEntry != null) {
			if(key > lastEntry.index) {
				lastEntry.next = new Entry(key, f);
				lastEntry = lastEntry.next;
				return;
			} else if (key == lastEntry.index) {
				lastEntry.value = f;
				return;
			}
		}
		
		Entry entry;
		
		if(cachedEntry != null) {
			if(cachedEntry.index == key) {
				cachedEntry.value = f;
				return;
			} else if(cachedEntry.index < key) {
				entry = cachedEntry;
				cachedEntry = null;
			} else {
				entry = firstEntry;
				cachedEntry = null;
			}
		} else {
			entry = firstEntry;
		}
		
		if (entry == null) {
			firstEntry = new Entry(key, f);
			lastEntry = firstEntry;
			return;
		}
		if(entry.index > key) {
			Entry newEntry = new Entry(key, f);
			newEntry.next = entry;
			firstEntry = newEntry;
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
				lastEntry = entry;
				return;
			} else {
				entry.next = new Entry(key, f);
				lastEntry = entry.next;
				return;
			}
		} else {
			if (entry.next.index == key) {
				entry.next.value = f;
				return;
			} else {
				Entry newEntry = new Entry(key, f);
				newEntry.next = entry.next;
				entry.next = newEntry;
				return;
			}
		}
	}

	public void remove(int j) {
		cachedEntry = null;
		if(lastEntry != null && lastEntry.index == j) lastEntry = null;
		
		Entry entry = firstEntry;
		if (entry == null) {
			return;
		}
		if (entry.index == j) {
			firstEntry = entry.next;
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
		firstEntry = null;
		cachedEntry = null;
		lastEntry = null;
	}
	

}
