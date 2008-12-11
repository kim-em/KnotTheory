package org.katlas.JavaKh.utils;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.SoftReference;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class SoftReferenceCachingList<Element extends Serializable> extends AbstractList<Element> implements SerializingList<Element> {
	private static final Log log = LogFactory.getLog(SoftReferenceCachingList.class);

	private final SerializingList<Element> innerList;
	private final List<Boolean> dirtyList;
	private final List<SoftReference<Element>> softReferenceList;
	private final ReferenceQueue<Element> referenceQueue;
	
	public SoftReferenceCachingList(SerializingList<Element> innerList) {
		this.referenceQueue = new ReferenceQueue<Element>();
		this.innerList = innerList;
		this.dirtyList = new ArrayList<Boolean>(innerList.size());
		this.softReferenceList = new ArrayList<SoftReference<Element>>(innerList.size());
		for(Element e : innerList) {
			this.dirtyList.add(false);
			this.softReferenceList.add(null);
		}
	}
	
	private void processReferenceQueue() {
		Reference<? extends Element> reference;
		while((reference = referenceQueue.poll()) != null) {
			int index = softReferenceList.indexOf(reference);
			if(index != -1) {
				if(dirtyList.get(index)) {
					log.debug("Writing dirty element after soft reference was cleared.");
					Element element = reference.get();
					if(element == null) {
						log.fatal("Dirty reference was cleared too early.");
						System.exit(1);
					}
					innerList.set(index, element);
				}
				softReferenceList.set(index, null);
			} else {
				log.warn("Reference appeared in the queue, but it's not in the list?");
			}
		}
	}
	
	@Override
	public synchronized Element get(int index) {
		processReferenceQueue();
		// try to use a SoftReference
		Reference<Element> reference = softReferenceList.get(index);
		if(reference != null) {
			Element element = reference.get();
			if(element != null) {
				return element;
			}
		}
		
		// otherwise, retrieve from the innerList, and save a SoftReference
		Element element = innerList.get(index);
		dirtyList.set(index, false);
		softReferenceList.set(index, new SoftReference<Element>(element));
		return element;
	}
	
	@Override
	public synchronized int size() {
		return innerList.size();
	}
	
	@Override
	public synchronized boolean add(Element element) {
		processReferenceQueue();
		
		innerList.add(null);
		dirtyList.add(true);
		softReferenceList.add(new SoftReference<Element>(element));	
		return true;
	}

	@Override
	public synchronized void add(int index, Element element) {
		// blegh, I don't want to have to deal with this for now.
		throw new UnsupportedOperationException();
	}
	
	@Override
	public synchronized void clear() {
		dirtyList.clear();
		softReferenceList.clear();
		innerList.clear();
		
		processReferenceQueue();
	}

	@Override
	public synchronized Element remove(int index) {
		processReferenceQueue();
		
		dirtyList.remove(index);
		softReferenceList.remove(index);
		innerList.remove(index);
		
		return null;
	}

	@Override
	public synchronized Element set(int index, Element element) {
		processReferenceQueue();
		
		dirtyList.set(index, true);
		softReferenceList.set(index, new SoftReference<Element>(element));
		return null;
	}

	private synchronized void writeDirtyElements() {
		for(int i = 0; i < innerList.size(); ++i) {
			if(dirtyList.get(i)) {
				Reference<Element> reference = softReferenceList.get(i);
				assert reference != null;
				Element element = reference.get();
				assert element != null;
				innerList.set(i, element);
				dirtyList.set(i, false);
			}
		}
	}
	
	public List<File> getSerializedForms() throws IOException {
		writeDirtyElements();
		return innerList.getSerializedForms();
	}

	public synchronized void setSerializedForm(int index, int hash, InputStream is)
			throws IOException {
		dirtyList.set(index, false);
		softReferenceList.set(index, null);
		innerList.setSerializedForm(index, hash, is);
	}
	
}
