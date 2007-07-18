/*
 * Created on Jul 18, 2007
 */
package org.katlas.rdf;

import java.util.Iterator;

import net.tqft.iterables.Iterables;
import net.tqft.iterables.Predicates;
import net.tqft.iterables.Triples;
import net.tqft.iterables.interfaces.Pair;
import net.tqft.iterables.interfaces.Predicate;
import net.tqft.iterables.interfaces.Transformer;
import net.tqft.iterables.interfaces.Triple;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class KnotAtlasDataExtractor implements Iterable<Triple<String, String, String>> {
	private static Log log = LogFactory.getLog(KnotAtlasDataExtractor.class);

	private final Iterable<Triple<String, String, String>> triples;

	private static final Predicate<String> isDataPage = new Predicate<String>() {
		public Boolean evaluate(String s) {
			return s.startsWith("Data:");
		}
	};
	private static final Transformer<Pair<String, String>, String> first = new Transformer<Pair<String, String>, String>() {
		public String evaluate(Pair<String, String> s) {
			return s.getFirst();
		}
	};
	private static final Transformer<Pair<String, String>, Triple<String, String, String>> tripleBuilder = new Transformer<Pair<String,String>, Triple<String, String, String>>() {
		public Triple<String, String, String> evaluate(Pair<String, String> pair) {
			String title = pair.getFirst();
			String text = pair.getSecond();
			int index = title.indexOf("/");
			if(index < 0) {
				log.warn("Bad title: " + title);
				return null;
			}
			return Triples.asTriple(
					title.substring(5, title.indexOf("/")).replace(' ', '_'),
					title.substring(title.indexOf("/") + 1).replace(' ', '_'),
					text
					);
		}
		
	};	
	
	private final Iterable<Triple<String, String, String>> createIterable(Iterable<Pair<String, String>> pages) {
		Iterable<Pair<String, String>> dataPages = Iterables.filter(
				pages,
				Predicates.compose(first, isDataPage)
			);
		

		return Iterables.filter(
					Iterables.transform(dataPages, tripleBuilder),
					Predicates.nonnull()
				);
		
	}
	
	public KnotAtlasDataExtractor(String xmlFilename) {
		triples = createIterable(new MediawikiTextExtractor(xmlFilename));
	}
	
	public KnotAtlasDataExtractor(Iterable<Pair<String, String>> pages) {
		triples = createIterable(pages);
	}
	
	public Iterator<Triple<String, String, String>> iterator() {
		return triples.iterator();
	}
	
}
