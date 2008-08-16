package org.katlas.JavaKh.utils;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;


public class DiskBackedCache<Element extends Serializable> implements Cache<Element> {
	private static final Log log = LogFactory.getLog(DiskBackedCache.class);
	
	static private int counter = 0;
	
	private final File storePath;
	
	public DiskBackedCache(File basePath) {
		storePath = new File(basePath, "DiskBackedCache" + (++counter));
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
	
	private boolean store(Element element) {
		if((element != null) && !(element instanceof Serializable)) {
			throw new ClassCastException("Only Serializable objects can be added to a DiskBackedCache.");
		}
	
		if (element != null) {
			File f = file(element);
			ObjectOutputStream oos = null;
			try {
				oos = new ObjectOutputStream(
						new FileOutputStream(f));
				oos.writeObject(element);
			} catch (FileNotFoundException ex) {
				log.warn(ex);
				return false;
			} catch (IOException ex) {
				log.warn(ex);
				return false;
			} finally {
				if(oos != null) {
					try {
						oos.close();
					} catch (IOException ex) {
						log.warn(ex);
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

	@SuppressWarnings("unchecked")
	@Override
	public Element cache(Element e) {
		File f = file(e);
		if(f.exists()) {
			ObjectInputStream ois = null;
			try {
				ois = new ObjectInputStream(new FileInputStream(f));
				return (Element)(ois.readObject());
			} catch (IOException ex) {
				log.warn(ex);
			} catch (ClassNotFoundException ex) {
				log.warn(ex);
			} finally {
				if(ois != null) {
					try {
						ois.close();
					} catch (IOException ex) {
						log.warn(ex);
					}
				}
			}
		}
		store(e);
		return e;
	}

	@Override
	public void flush() {
		
	}
}
