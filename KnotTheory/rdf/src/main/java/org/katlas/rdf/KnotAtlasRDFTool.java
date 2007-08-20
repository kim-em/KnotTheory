/*
 * Created on Jul 18, 2007
 */
package org.katlas.rdf;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.PosixParser;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.ntriples.NTriplesWriter;
import org.openrdf.sail.nativerdf.NativeStore;

public class KnotAtlasRDFTool {
	private static Log log = LogFactory.getLog(KnotAtlasRDFTool.class);

	public static void main(String... args) throws IOException,
			RepositoryException, RDFHandlerException {

		String inputFile = null;
		String outputFile = null;
		boolean preserve = true;

		File dataDir = new File("/www/sesame/openrdf-sesame/repositories/katlas/");

		try {
			CommandLineParser parser = new PosixParser();

			Options options = new Options();
			options.addOption("h", "help", false, "show this help screen");
			options.addOption("p", "preserve", false,
					"preserve pre-existing RDF statements in the repository.");
			options.addOption("i", "input <filename>", true,
					"process mediawiki XML export in file <filename>");
			options.addOption("o", "output <filename>", true,
					"write RDF statements to <filename>.");
			options
					.addOption(
							"r",
							"repository <directory>",
							true,
							"use <directory> as the RDF repository. (default /www/sesame/openrdf-sesame/repositories/katlas/)");
			options.addOption("info", false,
					"turn on lower level debugging statements [INFO]");
			options.addOption("debug", false,
					"turn on lowest level debugging statements [DEBUG]");

			CommandLine line = parser.parse(options, args);
			// String[] clean_args = line.getArgs();

			Logger rootLogger = Logger.getRootLogger();
			if (line.hasOption("debug"))
				rootLogger.setLevel(Level.DEBUG);
			else if (line.hasOption("info"))
				rootLogger.setLevel(Level.INFO);
			else
				rootLogger.setLevel(Level.WARN);

			preserve = line.hasOption("p");

			if (line.hasOption("i")) {
				inputFile = line.getOptionValue("i");
			}

			if (line.hasOption("o")) {
				outputFile = line.getOptionValue("o");
			}

			if (line.hasOption("r")) {
				dataDir = new File(line.getOptionValue("r"));
			}

			if (line.hasOption("h")
					|| (inputFile == null && outputFile == null)) {
				HelpFormatter formatter = new HelpFormatter();
				formatter.printHelp("katlas-rdf", options);
				System.exit(1);
			}

		} catch (Exception e) {
			log.fatal("Error found initializing: ", e);
			e.printStackTrace();
			System.exit(1);
		}

		/*
		 * String sesameServer = "http://katlas.org:8083/openrdf-sesame"; String
		 * repositoryName = "katlas"; Repository httpRepository = new
		 * HTTPRepository(sesameServer, repositoryName);
		 */

		Repository localRepository = new SailRepository(
				new NativeStore(dataDir));

		Repository repository = localRepository;
		repository.initialize();
		ValueFactory vf = repository.getValueFactory();

		String contextURI = "http://katlas.org/rdf/latest/";

		Resource context;
		if (contextURI == null) {
			context = null;
		} else {
			log.info("Creating context URI");
			context = vf.createURI(contextURI);
		}

		if (inputFile != null) {
			RepositoryConnection connection = null;
			try {
				connection = repository.getConnection();

				if (!preserve) {
					connection.remove((Resource) null, (URI) null,
							(Value) null, context);
				}

				Iterable<Statement> statements = new KnotAtlasRDFStatements(inputFile, repository);

				connection.add(statements, context);
			} finally {
				if (connection != null)
					connection.close();
			}
		}

		if (outputFile != null) {
			RepositoryConnection connection = null;
			try {
			connection = repository.getConnection();
			RDFHandler writer = new NTriplesWriter(new FileWriter(outputFile));
			writer.handleNamespace("knot", "http://katlas.org/wiki/");
			writer.handleNamespace("invariant", "http://katlas.org/wiki/Invariants/");
			connection.export(writer, context);
			connection.close(); 
			} finally {
				if(connection != null) {
					connection.close();
				}
			}
		}

	}

}
