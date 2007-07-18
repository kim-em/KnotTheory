package org.katlas.rdf;


import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import net.tqft.iterables.AbstractIterator;
import net.tqft.iterables.Iterables;
import net.tqft.iterables.Pairs;
import net.tqft.iterables.Predicates;
import net.tqft.iterables.Triples;
import net.tqft.iterables.interfaces.Pair;
import net.tqft.iterables.interfaces.Predicate;
import net.tqft.iterables.interfaces.Transformer;
import net.tqft.iterables.interfaces.Triple;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.http.HTTPRepository;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.ntriples.NTriplesWriter;
import org.openrdf.sail.nativerdf.NativeStore;

public class MediawikiTextExtractor implements Iterable<Pair<String, String>> {
	private static Log log = LogFactory.getLog(MediawikiTextExtractor.class);

	private final File xmlFile;

	public MediawikiTextExtractor(File xmlFile) {
		this.xmlFile = xmlFile;
	}

	public MediawikiTextExtractor(String xmlFileName) {
		this.xmlFile = new File(xmlFileName);
	}

	public Iterator<Pair<String, String>> iterator() {
		XMLInputFactory inputFactory = XMLInputFactory.newInstance();
		InputStream input;
		try {
			input = new FileInputStream(xmlFile);
		} catch (FileNotFoundException e1) {
			log.warn(e1);
			return Iterables.emptyIterator();
		}

		XMLStreamReader _reader;
		try {
			_reader = inputFactory.createXMLStreamReader(input);
		} catch (XMLStreamException e1) {
			log.warn(e1);
			return Iterables.emptyIterator();
		}
		final XMLStreamReader reader = _reader;

		return new AbstractIterator<Pair<String, String>>() {
			boolean readingTitle = false, readingText = false;

			String title = null, text = null;

			StringBuffer buffer;

			@Override
			public boolean hasNext() {
				try {
					while ((title == null || text == null) && reader.hasNext()) {
						int event = reader.next();
						if (event == XMLStreamConstants.START_ELEMENT) {
							if (reader.getLocalName() == "title") {
								readingTitle = true;
								buffer = new StringBuffer();
							}
							if (reader.getLocalName() == "text") {
								readingTitle = true;
								buffer = new StringBuffer();
							}
						}
						if (event == XMLStreamConstants.CHARACTERS) {
							if(readingText || readingTitle)
								buffer.append(reader.getText());
						}
						if (event == XMLStreamConstants.END_ELEMENT) {
							if (reader.getLocalName() == "title") {
								readingTitle = false;
								title = buffer.toString();
							}
							if (reader.getLocalName() == "text") {
								readingText = false;
								text = buffer.toString();
							}
						}
					}
				} catch (XMLStreamException e) {
					log.warn(e);
				}
				return title != null && text != null;
			}

			@Override
			protected Pair<String, String> returnNext() {
				Pair<String, String> result = Pairs.asPair(title, text);
				title = null;
				text = null;
				return result;
			}
		};
	}
	
}
