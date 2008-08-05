import java.io.Serializable;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class CachingList<Element extends Serializable> extends AbstractList<Element> implements List<Element> {
	private static final Log log = LogFactory.getLog(CachingList.class);

	private final List<Element> innerList;
	private final int numberToCache;
	private final Map<Integer, Element> cache;
	private final List<Integer> cacheOrder;
	
	public CachingList(List<Element> innerList, int numberToCache) {
		this.innerList = innerList;
		this.numberToCache = (numberToCache >=1) ? numberToCache : 1;
		cache = new HashMap<Integer, Element>();
		cacheOrder = new ArrayList<Integer>();
	}

	private void reduceCacheSize() {
		log.debug("reducing cache size...");
		int deleteIndex = cacheOrder.get(0);
		Element e = cache.remove(deleteIndex);
		cacheOrder.remove(0);
		innerList.set(deleteIndex, e);
	}
	
	@Override
	public Element get(int index) {
		if(!cache.containsKey(index)) {
			while(cache.size() >= numberToCache) reduceCacheSize();
			Element e = innerList.get(index);
			cache.put(index, e);
			cacheOrder.add(index);
		}
		
		return cache.get(index);
	}

	@Override
	public int size() {
		return innerList.size();
	}
	
	@Override
	public boolean add(Element element) {
		while(cache.size() >= numberToCache) reduceCacheSize();
		int size = size();
		cache.put(size, element);
		cacheOrder.add(size);
		innerList.add(null);
		
		return true;
	}

	@Override
	public void add(int index, Element element) {
		// blegh, I don't want to have to deal with shifting indices.
		throw new UnsupportedOperationException();
	}
	
	@Override
	public void clear() {
		cache.clear();
		cacheOrder.clear();
		innerList.clear();
	}

	@Override
	public Element remove(int index) {
		// blegh, I don't want to have to deal with shifting indices.
		throw new UnsupportedOperationException();
	}

	@Override
	public Element set(int index, Element element) {
		Element old = get(index);
		cache.put(index, element);
		return old;
	}
	
	
}
