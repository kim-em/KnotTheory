package org.katlas.JavaKh.algebra;

import java.lang.reflect.Constructor;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class Rings {
	private static final Log log = LogFactory.getLog(Rings.class);
	
	static Class<? extends Ring<?>> ring;
	
	@SuppressWarnings("unchecked")
	public static Ring createInstance(int i) {
		try {
			Constructor<? extends Ring<?>> c = ring.getConstructor(Integer.class);
			return (c.newInstance(i));
		} catch (Exception e) {
			log.warn(e);
			e.printStackTrace();
		} 
		return null;
	}
}