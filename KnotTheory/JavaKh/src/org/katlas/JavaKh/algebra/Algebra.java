package org.katlas.JavaKh.algebra;

public interface Algebra<R, X extends Algebra<R, X>> {

	X add(X m);
	X multiply(R r);
	X compose(X m);
	
}
