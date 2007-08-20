/*
 * Created on Jul 18, 2007
 */
package org.katlas.rdf;

import java.util.Iterator;

import net.tqft.iterables.Iterables;
import net.tqft.iterables.interfaces.Transformer;
import net.tqft.iterables.interfaces.Triple;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;

public class KnotAtlasRDFStatements implements Iterable<Statement> {
	private static Log log = LogFactory.getLog(KnotAtlasRDFStatements.class);

	private final ValueFactory vf;
	private final RepositoryConnection connection;
	private final Iterable<Statement> iterable;

	private final Transformer<Triple<String, String, String>, Statement> statementBuilder = new Transformer<Triple<String, String, String>, Statement>() {
		public Statement evaluate(Triple<String, String, String> triple) {
			return vf.createStatement(
					vf.createURI("knot:" + triple.getFirst()),
					vf.createURI("invariant:" + triple.getSecond()),
					vf.createLiteral(triple.getThird()));
		}
	};

	private final Iterable<Statement> createIterable(Iterable<Triple<String, String, String>> triples) {
		return Iterables.transform(triples, statementBuilder);
	}
	
	private final void init() throws RepositoryException {
		connection.setNamespace("knot", "http://katlas.org/wiki/");
		connection.setNamespace("invariant", "http://katlas.org/wiki/Invariants/");
	}

	public KnotAtlasRDFStatements(String xmlFilename, Repository repository) throws RepositoryException {
		vf = repository.getValueFactory();
		connection =  repository.getConnection();
		log.debug("Initialising RDF repository.");
		init();
		
		iterable = createIterable(new KnotAtlasDataExtractor(xmlFilename));
	}
	
	public KnotAtlasRDFStatements(Iterable<Triple<String, String, String>> triples, Repository repository) throws RepositoryException {
		vf = repository.getValueFactory();
		connection =  repository.getConnection();
		log.debug("Initialising RDF repository.");
		init();
		
		iterable = createIterable(triples);
	}

	public Iterator<Statement> iterator() {
		return iterable.iterator();
	}
	

	
}
