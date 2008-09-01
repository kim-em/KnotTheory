package org.katlas.JavaKh.algebra;


public interface LinearCombo<R extends Ring<R>, O, Mor extends Morphism<O, Mor>, LinearMor extends LinearMorphism<R, O, LinearMor>> extends LinearMorphism<R, O, LinearMor> {
	// ooof, generics get confusing. See the interface LCCC for an example of how to use this.
	
	public abstract int numberOfTerms();
	
	public abstract R firstCoefficient();
	public abstract Mor firstTerm();

	public abstract void add(Mor m, int num);
	public abstract void add(Mor m, R num);
	
	public abstract LinearMor zeroLinearCombo(O source, O target);

}