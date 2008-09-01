package org.katlas.JavaKh.algebra;

import java.lang.reflect.Constructor;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class Rings {
	private static final Log log = LogFactory.getLog(Rings.class);
	
	static Constructor<?> constructor;
	public static String ring;
	
	public static void setRing(String ringName) {
		ring = ringName;
		try {
		    Class<?> params[] = {Integer.TYPE};
		    Class<?> ringClass = Class.forName("org.katlas.JavaKh." + ring);
		    constructor = ringClass.getConstructor(params);
		} catch (Exception e) {
		    System.err.println("Error setting BaseRing");
		    System.exit(1);
		}
	    }
	
	@SuppressWarnings("unchecked")
	public static <R> R createInstance(int i) {
		assert constructor != null;
		try {
			return (R)(constructor.newInstance(i));
		} catch (Exception e) {
			log.warn(e);
			e.printStackTrace();
		} 
		return null;
	}
}