package org.katlas.JavaKh.algebra;

public abstract class AbstractLinearMorphism<R extends Ring<R>, Obj, Mor extends LinearMorphism<R, Obj, Mor>> implements LinearMorphism<R, Obj, Mor> {

	@SuppressWarnings("unchecked")
	public Mor multiply(int r) {
		return multiply((R)Rings.createInstance(r));
	}

}
