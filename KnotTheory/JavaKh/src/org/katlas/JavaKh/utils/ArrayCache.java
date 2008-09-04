package org.katlas.JavaKh.utils;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class ArrayCache implements Cache<byte[]> {

	private static final Log log = LogFactory.getLog(ArrayCache.class);
	
	private final Map<Integer, byte[]> hashmap = new HashMap<Integer, byte[]>();
	private transient long hits = 0;
	private transient long checks = 0;
//	private transient long hashCollisions = 0;
	
	public synchronized byte[] cache(byte[] e) {
		++checks;
		int hash = Arrays.hashCode(e);
		if(hashmap.containsKey(hash)) {
			byte[] result = hashmap.get(hash);
			if(result != null) {
				if(e != result && !Arrays.equals(e, result)) {
					if(hash != Arrays.hashCode(result)) {
						log.info("Hashcode has mysteriously changed.");
					}
//					assert false;
//					log.warn("Hash collision #" + (++hashCollisions));
				} else {
					++hits;
					return result;
				}
			} else {
//				log.debug("Weak reference lost object");
				hashmap.remove(hash);
			}
		}
		hashmap.put(hash, e);
		return e;
	}
	
	public synchronized void flush() {
		hashmap.clear();
	}

	public int size() {
		return hashmap.size();
	}

	public long getNumberOfChecks() { return checks; }
	public long getNumberOfHits() { return hits; }
	
}
