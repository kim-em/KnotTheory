/*
 * Created on Nov 30, 2005
 */
package net.tqft.iterables;

import java.util.Arrays;
import java.util.Iterator;

import net.tqft.iterables.interfaces.Transformer;

/**
 * @author Scott Morrison
 */
public final class Iterables {

	// don't allow instances -- this is a purely static class
	private Iterables() {
	}

	private Object readResolve() {
		return null;
	}

	public static <S, T extends S, U, V extends U> Iterator<U> transform(
			Iterator<? extends T> iterator, final Transformer<S, V> transformer) {
		return new TransformedIterator<T, U>(iterator) {
			@Override
			public U transform(T t) {
				return transformer.evaluate(t);
			}
		};
	}

	public static <S, T extends S, U, V extends U> Iterable<U> transform(
			Iterable<? extends T> iterable, final Transformer<S, V> transformer) {
		return new TransformedIterable<T, U>(iterable) {
			@Override
			public U evaluate(T t) {
				return transformer.evaluate(t);
			}
		};
	}

	/**
	 * @param <T>
	 * @return an empty Iterator object.
	 */
	public static <T> Iterator<T> emptyIterator() {
		return EmptyIterator.getInstance();
	}

	/**
	 * @param <T>
	 * @return an empty Iterable object.
	 */
	public static <T> Iterable<T> emptyIterable() {
		return EmptyIterable.getInstance();
	}

	/**
	 * @param <T>
	 * @param t
	 * @return an iterable containing the single element t.
	 */
	public static <S, T extends S> Iterable<S> singleton(T t) {
		return new SingletonIterable<S>(t);
	}
	
	/**
	 * @param <T>
	 * @param iterables
	 * @return an iterable iterating over all the values of each iterable in
	 *         turn
	 */
	public static <T> Iterable<T> concatenate(Iterable<Iterable<T>> iterables) {
		return new IterableBundle<Iterable<T>, T>(iterables) {
			@Override
			public Iterable<T> buildNewFibreIterable(Iterable<T> iterable) {
				return iterable;
			}
		};
	}

	/**
	 * @param <T>
	 * @param iterables
	 * @return an iterable iterating over all the values of each iterable in
	 *         turn
	 */
	public static <T> Iterable<T> concatenate(Iterable<T>... iterables) {
		return concatenate(Arrays.asList(iterables));
	}

}
