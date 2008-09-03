package org.katlas.JavaKh.algebra.implementations;

import org.katlas.JavaKh.algebra.LinearMorphism;
import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.algebra.rings.Rings;

public abstract class AbstractLinearMorphism<R extends Ring<R>, Obj, Mor extends LinearMorphism<R, Obj, Mor>> implements LinearMorphism<R, Obj, Mor> {

	public Mor multiply(int r) {
		Rings<R> ring = Rings.current();
		return multiply(ring.createInstance(r));
	}

}
