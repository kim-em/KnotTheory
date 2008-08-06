import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;


public class DiskBackedList<Element extends Serializable> extends AbstractList<Element> implements SerializingList<Element> {

	static private int counter = 0;
	
	private final File storePath;
	private final List<Integer> hashlist = new ArrayList<Integer>();
	
	public DiskBackedList(File basePath) {
		storePath = new File(basePath, "DiskBackedList" + (++counter));
		storePath.delete();
		storePath.mkdirs();
		storePath.deleteOnExit();
	}
	
	private File file(Element element) {
		return new File(storePath, new Integer(element.hashCode()).toString());
	}
	
	private File file(int hashCode) {
		return new File(storePath, new Integer(hashCode).toString());		
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Element get(int index) {
		if(hashlist.get(index) == null) return null;
		ObjectInputStream ois = null;
		try {
			ois = new ObjectInputStream(new FileInputStream(file(hashlist.get(index))));
			Element r = (Element)(ois.readObject());
			return r;
		} catch (FileNotFoundException e) {
			return null;
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
			return null;
		} finally {
			if(ois != null) {
				try {
					ois.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
	}

	@Override
	public int size() {
		return hashlist.size();
	}

	private boolean store(Element element) {
		if((element != null) && !(element instanceof Serializable)) {
			throw new ClassCastException("Only Serializable objects can be added to a DiskBackedList.");
		}
	
		if (element != null) {
			File f = file(element);
			ObjectOutputStream oos = null;
			try {
				oos = new ObjectOutputStream(
						new FileOutputStream(f));
				oos.writeObject(element);
			} catch (FileNotFoundException e) {
				e.printStackTrace();
				return false;
			} catch (IOException e) {
				e.printStackTrace();
				return false;
			} finally {
				if(oos != null) {
					try {
						oos.close();
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
			}
			return true;
		} else{
			return true;
		}

	}

	private boolean erase(int hashcode) {
		File f = file(hashcode);
		return f.delete();
	}
	
	@Override
	public void add(int index, Element element) {
		store(element);
		hashlist.add(index, (element == null) ? null : element.hashCode());
	}

	@Override
	public Element remove(int index) {
		Element e = get(index);
		int hashCode = hashlist.get(index);
		hashlist.remove(index);
		if(!hashlist.contains(hashCode)) {
			erase(hashCode);
		}
		return e;
	}

	@Override
	public Element set(int index, Element element) {
		Element e = get(index);
		if(element == null) {
			hashlist.set(index, null);
		} else {
			if(!(hashlist.contains(element.hashCode()))) {
				store(element);
			}
			hashlist.set(index, element.hashCode());
		}
		return e;
	}


	@Override
	public int indexOf(Object o) {
		return hashlist.indexOf(o.hashCode());
	}

	@Override
	public int lastIndexOf(Object o) {
		return hashlist.lastIndexOf(o.hashCode());
	}

	@Override
	public void clear() {
		for(Integer hashCode : hashlist) {
			erase(hashCode);
		}
		hashlist.clear();
	}

	@Override
	public int hashCode() {
		return hashlist.hashCode() + storePath.hashCode();
	}

	/*@Override
	public Iterator<ObjectInputStream> getSerializedForms() throws IOException {
		List<ObjectInputStream> result = new ArrayList<ObjectInputStream>();
		for(int hash : hashlist) {
			result.add(new ObjectInputStream(new FileInputStream(file(hash))));
		}
		return result;
	}*/
	
	public Iterator<ObjectInputStream> getSerializedForms() throws IOException {
		final Iterator<Integer> iterator = hashlist.iterator();
		return new Iterator<ObjectInputStream>() {

			@Override
			public boolean hasNext() {
				return iterator.hasNext();
			}

			@Override
			public ObjectInputStream next() {
				if(hasNext()) {
					try {
						return new ObjectInputStream(new FileInputStream(file(iterator.next())));
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
	
}
