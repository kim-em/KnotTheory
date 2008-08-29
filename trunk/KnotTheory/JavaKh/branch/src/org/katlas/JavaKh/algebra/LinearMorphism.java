package org.katlas.JavaKh.algebra;

public interface LinearMorphism<R extends Ring<R>, Obj, Mor extends LinearMorphism<R, Obj, Mor>> extends Morphism<Obj, Mor> {

	Mor add(Mor m);
	Mor multiply(R r);
	
}
