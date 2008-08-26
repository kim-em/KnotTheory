package org.katlas.JavaKh.utils;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CachingList<Element extends Serializable> extends AbstractList<Element> implements SerializingList<Element> {
//	private static final Log log = LogFactory.getLog(CachingList.class);

	private final SerializingList<Element> innerList;
	private int cacheSize;
	private final Map<Integer, Element> cache;
	private final List<Integer> cacheOrder;
	
	public CachingList(SerializingList<Element> innerList, int cacheSize) {
		this.innerList = innerList;
		this.cacheSize = (cacheSize > 1) ? cacheSize : 1;
		cache = new HashMap<Integer, Element>();
		cacheOrder = new ArrayList<Integer>();
	}
	
	public void resetCacheSize(int newCacheSize) {
		cacheSize = (newCacheSize > 1) ? newCacheSize : 1;
		while(cache.size() > cacheSize) reduceCacheSize();	
	}
	
	private static void invokeGC() {
		for(int i = 0; i < 4; ++i) {
			System.gc();
		}
	}

	private void checkSizes() {
		if(cacheOrder.size() != cache.size()) {
			throw new AssertionError("cache corruption");
		}
	}
	
	private void reduceCacheSize() {
		checkSizes();
		int deleteIndex = cacheOrder.get(0);
		Element e = cache.remove(deleteIndex);
		cacheOrder.remove(0);
		innerList.set(deleteIndex, e);
		invokeGC();
		checkSizes();
	}
	
	@Override
	public synchronized Element get(int index) {
		if(!cache.containsKey(index)) {
			while(cache.size() >= cacheSize) reduceCacheSize();
			Element e = innerList.get(index);
			cache.put(index, e);
			cacheOrder.add(index);
			checkSizes();
		}
	
		return cache.get(index);
	}

	@Override
	public synchronized int size() {
		return innerList.size();
	}
	
	@Override
	public synchronized boolean add(Element element) {
		while(cache.size() >= cacheSize) reduceCacheSize();
		int size = size();
		cache.put(size, element);
		cacheOrder.add(size);
		innerList.add(null);
		
		checkSizes();
		
		return true;
	}

	@Override
	public synchronized void add(int index, Element element) {
		// blegh, I don't want to have to deal with shifting indices.
		throw new UnsupportedOperationException();
	}
	
	@Override
	public synchronized void clear() {
		cache.clear();
		cacheOrder.clear();
		innerList.clear();
	}

	@Override
	public synchronized Element remove(int index) {
		// blegh, I don't want to have to deal with shifting indices.
		throw new UnsupportedOperationException();
	}

	@Override
	public synchronized Element set(int index, Element element) {
//		Element old = get(index);
		if(!cache.containsKey(index)) {
			reduceCacheSize();
			cacheOrder.add(index);
		}
		cache.put(index, element);
//		return old;
		return null;
	}

	public List<File> getSerializedForms() throws IOException {
		while(cache.size() > 0) reduceCacheSize();
		return innerList.getSerializedForms();
	}

	public void setSerializedForm(int index, int hash, InputStream is)
			throws IOException {
		if(cache.containsKey(index)) {
			checkSizes();
			cache.remove(index);
			cacheOrder.remove((Object)index); // ugh, we want to remove the object, not by index. Confusing!
			checkSizes();
		}
		innerList.setSerializedForm(index, hash, is);
	}
	
	
}
