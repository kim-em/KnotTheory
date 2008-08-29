package org.katlas.JavaKh;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.math.BigInteger;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.StringTokenizer;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadFactory;

import org.apache.commons.io.IOUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.algebra.Rings;
import org.katlas.JavaKh.utils.CachingList;
import org.katlas.JavaKh.utils.DiskBackedList;
import org.katlas.JavaKh.utils.LimitedSizeInputStream;
import org.katlas.JavaKh.utils.SerializingList;

public class Komplex<R extends Ring<R>> implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -6669296477790589829L;

	private static final Log log = LogFactory.getLog(Komplex.class);

	static final int MAXDEPTH = 3;

	private static int mostReductions = 0;

	private static int largestMatrix = 0;
	int ncolumns, nfixed;
	final SmoothingColumn columns[];
	final boolean inMemory;
	private transient List<CobMatrix<R>> matrices;
	int startnum;

	transient static boolean parallel;
	transient static boolean intenseGarbage;

	private CobMatrix<R> getMatrix(int i) {
		return matrices.get(i);
	}

	// This is intentionally void return type, instead of the more usual 'CobMatrix',
	// so DiskBackedList doesn't have to read before it writes.
	private void setMatrix(int i, CobMatrix<R> m) {
		matrices.set(i, m);
	}

	static int pascalTriangle[][];

	static void fillPascal(int n) {
		if (pascalTriangle != null && pascalTriangle.length > n)
			return;
		pascalTriangle = new int[n + 1][];
		for (int i = 0; i <= n; i++) {
			pascalTriangle[i] = new int[i + 1];
			for (int j = 0; j <= i; j++)
				if (j == 0 || j == i)
					pascalTriangle[i][j] = 1;
				else
					pascalTriangle[i][j] = pascalTriangle[i - 1][j - 1]
							+ pascalTriangle[i - 1][j];
		}
	}

	public Komplex(int n, boolean inMemory) {
		ncolumns = n;
		columns = new SmoothingColumn[ncolumns];
		this.inMemory = inMemory;
		createMatrixList();
	}

	@SuppressWarnings("unchecked")
	public boolean equals(Object o) { // doesn't fully check for equivalence
		if (!(o instanceof Komplex))
			return false;
		Komplex<R> k = (Komplex<R>) o;
		
		// try to find a match, allowing for empty columns, etc.
		int i = 0, j = 0;
		if (startnum < k.startnum) {
			for (; i < k.startnum - startnum; i++)
				if (columns[i].n != 0)
					return false;
		} else if (startnum > k.startnum) {
			for (; j < startnum - k.startnum; j++)
				if (k.columns[j].n != 0)
					return false;
		}
		if (i < ncolumns && j < k.ncolumns)
			if (!columns[i++].equals(k.columns[j++]))
				return false;
		for (; i < ncolumns && j < k.ncolumns; i++, j++) {
			if (!columns[i].equals(k.columns[j]))
				return false;
			if (!getMatrix(i - 1).equals(k.getMatrix(j - 1)))
				return false;
		}
		for (; i < ncolumns; i++)
			if (columns[i].n != 0)
				return false;
		for (; j < k.ncolumns; j++)
			if (k.columns[j].n != 0)
				return false;
		return true;
	}

	public static <R extends Ring<R>> void checkReidemeister() {
		int R1aPD[][] = { { 2, 0, 1, 2 } }, R1aS[] = { -1 };
		Komplex<R> R1a = new Komplex<R>(R1aPD, R1aS, 2, true);
		R1a.deLoop();
		R1a.reductionLemma();
		int R1bPD[][] = { { 0, 1, 2, 2 } }, R1bS[] = { 1 };
		Komplex<R> R1b = new Komplex<R>(R1bPD, R1bS, 2, true);
		R1b.deLoop();
		R1b.reductionLemma();
		Komplex<R> R1c = new Komplex<R>(1, true);
		R1c.startnum = 0;
		R1c.columns[0] = new SmoothingColumn(1);
		R1c.columns[0].smoothings[0] = new Cap(2, 0);
		R1c.columns[0].smoothings[0].pairings[0] = 1;
		R1c.columns[0].smoothings[0].pairings[1] = 0;
		if (R1a.equals(R1b) && R1b.equals(R1c) && R1c.equals(R1a))
			System.out.println("R1 checks OK");
		else
			System.out.println("Error checking R1");

		int R2aPD[][] = { { 0, 1, 5, 4 }, { 5, 2, 3, 4 } }, R2aS[] = { -1, 1 };
		Komplex<R> R2a = new Komplex<R>(R2aPD, R2aS, 4, true);
		R2a.deLoop();
		R2a.reductionLemma();
		int R2bPD[][] = { { 1, 5, 4, 0 }, { 4, 5, 2, 3 } }, R2bS[] = { 1, -1 };
		Komplex<R> R2b = new Komplex<R>(R2bPD, R2bS, 4, true);
		R2b.deLoop();
		R2b.reductionLemma();
		Komplex<R> R2c = new Komplex<R>(1, true);
		R2c.startnum = 0;
		R2c.columns[0] = new SmoothingColumn(1);
		R2c.columns[0].smoothings[0] = new Cap(4, 0);
		R2c.columns[0].smoothings[0].pairings[0] = 3;
		R2c.columns[0].smoothings[0].pairings[1] = 2;
		R2c.columns[0].smoothings[0].pairings[2] = 1;
		R2c.columns[0].smoothings[0].pairings[3] = 0;
		if (R2a.equals(R2b) && R2b.equals(R2c) && R2c.equals(R2a))
			System.out.println("R2 checks OK");
		else
			System.out.println("Error checking R2");

		int R3aPD[][] = { { 0, 6, 8, 5 }, { 2, 7, 6, 1 }, { 8, 7, 3, 4 } }, R3aS[] = {
				-1, 1, -1 };
		Komplex<R> R3a = new Komplex<R>(R3aPD, R3aS, 6, true);
		R3a.deLoop();
		R3a.reductionLemma();
		int R3bPD[][] = { { 0, 1, 7, 6 }, { 7, 2, 3, 8 }, { 8, 4, 5, 6 } }, R3bS[] = {
				-1, -1, 1 };
		Komplex<R> R3b = new Komplex<R>(R3bPD, R3bS, 6, true);
		R3b.deLoop();
		R3b.reductionLemma();
		if (R3a.equals(R3b))
			System.out.println("R3 checks OK");
		else
			System.out.println("Error checking R3");
	}

	public static int[][] getPD(BufferedReader br) throws java.io.IOException {
		// System.out.print("Enter PD: ");
		String str = br.readLine();
		if (str == null)
			return null;
		StringTokenizer st = new StringTokenizer(str, "X");
		int pd[][] = new int[st.countTokens()][4];
		int a = 0;
		while (st.hasMoreTokens()) {
			String tok = st.nextToken().trim();
			if (tok.length() < 9)
				continue;
			int i = tok.indexOf('[');
			int j = tok.indexOf(']', i);
			tok = tok.substring(i + 1, j);
			StringTokenizer st2 = new StringTokenizer(tok, ",");
			for (int k = 0; k < 4; k++)
				pd[a][k] = Integer.parseInt(st2.nextToken().trim()) - 1;
			a++;
		}
		int ret[][] = new int[a][4];
		System.arraycopy(pd, 0, ret, 0, a);
		return ret;
	}

	public String Kh() {
		if (JavaKh.using_h)
			return KhWithH();
		else if (BaseRing.ring.equals("Int"))
			return KhForZ();
		String ret = "";
		for (int i = 0; i < ncolumns; i++) {
			if (columns[i].n == 0)
				continue;
			int nums[] = new int[columns[i].n];
			System.arraycopy(columns[i].numbers, 0, nums, 0, columns[i].n);
			Arrays.sort(nums);
			for (int j = 0; j < nums.length;) {
				int n, k;
				for (k = j; k < nums.length && nums[k] == nums[j]; k++)
					;
				n = k - j;
				if (!ret.equals(""))
					ret += "+ ";
				if (n != 1)
					ret = ret + n + "*";
				ret += "q^" + nums[j] + "*t^" + (i + startnum) + " ";
				j = k;
			}
		}
		return ret;
	}

	public String KhWithH() {
		StringBuffer ret = new StringBuffer();
		for (int i = 0; i < ncolumns; i++) {
			int last = 0x80000000;
			while (true) {
				int min = 0x7fffffff, count = 0;
				for (int j = 0; j < columns[i].n; j++)
					if (columns[i].numbers[j] > last
							&& columns[i].numbers[j] < min) {
						min = columns[i].numbers[j];
						count = 1;
					} else if (columns[i].numbers[j] == min)
						count++;
				if (min == 0x7fffffff)
					break;
				last = min;
				if (i == ncolumns - 1) {
					if (ret.length() != 0)
						ret.append(" + ");
					ret.append("q^" + min + "*t^" + (i + startnum) + "*h^0*M["
							+ 0 + ", " + count + "]");
					continue;
				}
				int last2 = min - 1;
				while (true) {
					int min2 = 0x7fffffff, count2 = 0;
					;
					if (last2 == min - 1) {
						min2 = min;
						for (int j = 0; j < columns[i + 1].n; j++)
							if (columns[i + 1].numbers[j] == min2)
								count2++;
					} else
						for (int j = 0; j < columns[i + 1].n; j++)
							if (columns[i + 1].numbers[j] > last2
									&& columns[i + 1].numbers[j] < min2) {
								min2 = columns[i + 1].numbers[j];
								count2 = 1;
							} else if (columns[i + 1].numbers[j] == min2)
								count2++;
					if (min2 == 0x7fffffff)
						break;
					last2 = min2;
					assert (min2 - min) % 2 == 0;
					if (ret.length() != 0)
						ret.append(" + ");
					ret.append("q^" + min + "*t^" + (i + startnum) + "*h^"
							+ (min2 - min) / 2 + "*M[" + count2 + ", " + count);
					// boolean first = true;
					for (int j = 0; j < columns[i + 1].n; j++)
						if (columns[i + 1].numbers[j] == min2) {
							/*
							 * if (first) first = false; else
							 */
							// ret += ", ";
							// ret += "{";
							// boolean first2 = true;
							LCCC<R> row[] = getMatrix(i).unpackRow(j);
							for (int k = 0; k < columns[i].n; k++)
								if (columns[i].numbers[k] == min) {
									/*
									 * if (first2) first2 = false; else
									 */
									ret.append(", ");
									if (row[k] == null || row[k].size() == 0)
										ret.append("0");
									else {
										assert row[k].size() == 1;
										ret.append(row[k].firstCoefficient());
									}
								}
							// ret += "}";
						}
					ret.append("]");
				}
			}
		}
		return ret.toString();
	}

	@SuppressWarnings("unchecked")
	public String KhForZ() {
		IntMatrix mats[] = new IntMatrix[matrices.size()];
		int colcopy[][] = new int[ncolumns][];
		for (int i = 0; i < ncolumns; i++) {
			colcopy[i] = new int[columns[i].n];
			System
					.arraycopy(columns[i].numbers, 0, colcopy[i], 0,
							columns[i].n);
		}
		for (int i = 0; i < matrices.size(); i++) {
			mats[i] = new IntMatrix(getMatrix(i));
			mats[i].source = colcopy[i];
			mats[i].target = colcopy[i + 1];
			if (i > 0) {
				// mats[i].prev = mats[i - 1];
				mats[i - 1].next = mats[i];
			}
		}
		String ret = "";
		for (int i = 0; i < ncolumns; i++) {
			int degrees[] = new int[columns[i].n], ndeg = 0;
			int nentries[] = new int[columns[i].n];
			ArrayList<Integer> retvals[] = new ArrayList[columns[i].n];
			int last = 0x80000000;
			int n = 0;
			if (i != 0)
				n = Math.min(mats[i - 1].rows, mats[i - 1].columns);
			while (true) {
				int min = 0x7fffffff;
				for (int j = 0; j < columns[i].n; j++)
					if (colcopy[i][j] > last && colcopy[i][j] < min)
						min = colcopy[i][j];
				if (min == 0x7fffffff)
					break;
				degrees[ndeg] = min;
				retvals[ndeg] = new ArrayList<Integer>();
				BigInteger nums[] = new BigInteger[n];
				int nnum = 0;
				for (int j = 0; j < n; j++)
					if (colcopy[i][j] == min
							&& !mats[i - 1].matrix[j][j]
									.equals(BigInteger.ZERO))
						nums[nnum++] = mats[i - 1].matrix[j][j];
				nentries[ndeg] = nnum;
				if (nnum != 0) {
					boolean done = false;
					int pk = 1;
					while (!done) {
						// find next p^k
						int p = 0, k;
						pk++;
						for (;; pk++) {
							for (int j = 2; j <= pk; j++)
								if (pk % j == 0) {
									p = j;
									break;
								}
							int tmp = pk;
							k = 0;
							while (tmp > 1)
								if (tmp % p == 0) {
									tmp /= p;
									k++;
								} else
									break;
							if (tmp == 1)
								break;
						}
						for (int j = 0; j < nnum; j++) {
							BigInteger divmod[] = nums[j]
									.divideAndRemainder(BigInteger.valueOf(pk));
							if (divmod[1].equals(BigInteger.ZERO)
									&& !nums[j].mod(BigInteger.valueOf(pk * p))
											.equals(BigInteger.ZERO)) {
								retvals[ndeg].add(new Integer(pk));
								nums[j] = divmod[0];
							}
						}
						done = true;
						for (int j = 0; j < nnum; j++)
							if (!nums[j].equals(BigInteger.ONE)) {
								done = false;
								break;
							}
					}
				}
				last = min;
				ndeg++;
			}
			if (i != ncolumns - 1) {
				mats[i].toSmithForm();
				// mats[i].print();
				if (!mats[i].isDiagonal())
					throw new AssertionError("Matrix is not diagonal");
			}
			for (int j = 0; j < ndeg; j++) {
				int nzeros = 0;
				if (i != ncolumns - 1) {
					for (int k = 0; k < columns[i].n; k++)
						if (colcopy[i][k] == degrees[j]
								&& (k >= mats[i].rows || k >= mats[i].columns || mats[i].matrix[k][k]
										.equals(BigInteger.ZERO)))
							nzeros++;
				} else
					for (int k = 0; k < columns[i].n; k++)
						if (colcopy[i][k] == degrees[j])
							nzeros++;
				nzeros -= nentries[j];
				for (int k = 0; k < nzeros; k++)
					retvals[j].add(new Integer(0));
				if (retvals[j].size() > 0) {
					Object retvalarr[] = retvals[j].toArray();
					Arrays.sort(retvalarr);
					if (!ret.equals(""))
						ret += " + ";
					ret += "q^" + degrees[j] + "*t^" + (i + startnum) + "*Z[";
					for (int k = 0; k < retvalarr.length; k++) {
						ret += retvalarr[k];
						if (k == retvalarr.length - 1)
							ret += "]";
						else
							ret += ",";
					}
				}
			}
		}
		return ret;
	}

	public void debugPrint() {
		for (int i = 0; i < ncolumns; i++) {
			columns[i].printNumbers();
			if (i != ncolumns - 1)
				getMatrix(i).printZeros();
		}
	}

	public void finalizeH() {
		for (int i = 0; i < ncolumns - 1; i++) {
			CobMatrix<R> m = getMatrix(i);
			for(CobMatrixRow<R> rowEntries : m.entries) {
				for(int j : rowEntries.keys()) {
					rowEntries.put(j, rowEntries.get(j).finalizeH());
				}
			}
//			for (int j = 0; j < m.target.n; j++)
//				for (int k = 0; k < m.rowsizes[j]; k++) {
//					LCCC lc = m.values[j][k];
//					if (lc != null && lc.size() != 0) {
//						m.values[j][k] = lc.finalizeH();
//						assert m.values[j][k] == null || m.values[j][k].size() < 2;
//					}
//				}
			setMatrix(i, m);
		}
	}

	private static void invokeGC() {
		if(intenseGarbage) {
		for(int i = 0; i < 4; ++i) {
			System.gc();
		}
		}
	}
	
	public void reduce() {
		/*
		 * Can this be safely parallelised?
		 */
		if (parallel) {
			parallelReduce();
		} else {
			for (int i = 0; i < ncolumns; i++) {
				debug("delooping " + (i + 1) + "/" + ncolumns);
				deLoop(i);
				invokeGC();
				if (i > 0) {
					debug("applying reduce " + (i + 1) + "/" + ncolumns);
					CobMatrix<R> m = getMatrix(i - 1);
					m.reduce();
					setMatrix(i - 1, m);
					invokeGC();
					debug("applying reduction lemma " + (i + 1) + "/" + ncolumns);
					reductionLemma(i - 1);
					invokeGC();
				}
			}
		}
	}

	private void parallelReduce() {
		ExecutorService executor = Executors.newCachedThreadPool(new ThreadFactory() {
			ThreadFactory tf = Executors.defaultThreadFactory();
			
			public Thread newThread(Runnable arg0) {
				Thread t = tf.newThread(arg0);
				t.setDaemon(true);
				return t;
			}});
			
//		ExecutorService executor =	new ThreadPoolExecutor(1, 10, 10,
//				TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>());
		int p = 4;
		for (int j = 0; j < p; ++j) {
			// prepare the tasks.
			List<Callable<Boolean>> tasks = new ArrayList<Callable<Boolean>>();
			for (int i = j; i < ncolumns; i += p) {
				tasks.add(new reduceTask(i));
			}
			// submit them.
			List<Future<Boolean>> futures = null;
			try {
				futures = executor.invokeAll(tasks);
			} catch (InterruptedException e) {
				log.warn("Maybe parallel reduction wasn't such a good idea.", e);
				System.exit(1);
			}
			// block until they're all done.
			if (futures != null) {
				for (Future<Boolean> future : futures) {
					try {
						future.get();
					} catch (InterruptedException e) {
						log.warn("Maybe parallel reduction wasn't such a good idea.", e);
						System.exit(1);
					} catch (ExecutionException e) {
						log.warn("Maybe parallel reduction wasn't such a good idea.", e);
						System.exit(1);
					}
				}
			}
		}
		executor.shutdownNow();
	}

	private class reduceTask implements Callable<Boolean> {

		private final int i;

		reduceTask(int i) {
			this.i = i;
		}

		public Boolean call() throws Exception {
			debug("delooping " + (i + 1) + "/" + ncolumns);
			deLoop(i);
			if (i > 0) {
				deLoop(i - 1);
				debug("applying reduce " + (i + 1) + "/" + ncolumns);
				CobMatrix<R> m = getMatrix(i - 1);
				m.reduce();
				setMatrix(i - 1, m);
				debug("applying reduction lemma " + (i + 1) + "/" + ncolumns);
				reductionLemma(i - 1);
				debug("done " + (i + 1) + "/" + ncolumns);
			}
			return true;
		}

	}

	public void reduceLocal() {
		for (int i = 0; i < matrices.size(); i++) {
			CobMatrix<R> m = getMatrix(i);
			m.reduce();
			setMatrix(i, m);
		}
	}

	/*
	 * This function looks horrible inefficient; however it doesn't actually get
	 * used. meh. ---Scott
	 */
	public void deLoop() { // deloops all columns
		for (int i = 0; i < ncolumns; i++) {
			deLoop(i);
			reduceLocal();
		}
	}

	public void deLoop(int colnum) { // deloops one column
		if (JavaKh.using_h) {
			deLoopWithH(colnum);
			return;
		}
		int size = 0;
		for (int i = 0; i < columns[colnum].n; i++)
			size += 1 << columns[colnum].smoothings[i].ncycles;
		SmoothingColumn newsc = new SmoothingColumn(size);
		CobMatrix<R> prev = null, next = null;
		if (colnum != 0)
			prev = new CobMatrix<R>(columns[colnum - 1], newsc);
		if (colnum != ncolumns - 1)
			next = new CobMatrix<R>(newsc, columns[colnum + 1]);
		int newn = 0;
		for (int i = 0; i < columns[colnum].n; i++) {
			Cap oldsm = columns[colnum].smoothings[i];
			Cap newsm = new Cap(oldsm.n, 0);
			for (int j = 0; j < oldsm.n; j++)
				newsm.pairings[j] = oldsm.pairings[j];
			newsm = Cap.capCache.cache(newsm);
			CannedCobordism prevcc = new CannedCobordism(oldsm, newsm);
			prevcc.ncc = prevcc.nbc;
			prevcc.connectedComponent = CannedCobordism.counting[prevcc.nbc];
			prevcc.dots = new byte[prevcc.ncc];
			prevcc.genus = CannedCobordism.zeros[prevcc.ncc];
			CannedCobordism nextcc = new CannedCobordism(newsm, oldsm);
			nextcc.ncc = nextcc.nbc;
			nextcc.connectedComponent = CannedCobordism.counting[nextcc.nbc];
			nextcc.dots = new byte[nextcc.ncc];
			nextcc.genus = CannedCobordism.zeros[nextcc.ncc];
			// the dots array for prevcc and nextcc is reused here
			// this is safe because dots are not stored in the CC cache
			for (int j = 0; j < (1 << oldsm.ncycles); j++) {
				int nmod = 0;
				for (int k = 0; k < oldsm.ncycles; k++) {
					if ((j & (1 << k)) == 0) {
						nmod++;
						// these appear to be reversed
						// but it works this way
						prevcc.dots[prevcc.offtop + k] = 1;
						nextcc.dots[nextcc.offbot + k] = 0;
					} else {
						nmod--;
						prevcc.dots[prevcc.offtop + k] = 0;
						nextcc.dots[nextcc.offbot + k] = 1;
					}
				}
				newsc.smoothings[newn] = newsm;
				newsc.numbers[newn] = columns[colnum].numbers[i] + nmod;
				if (prev != null) {
					CobMatrix<R> prevMatrix = getMatrix(colnum - 1);
//					prev.rowsizes[newn] = prevMatrix.rowsizes[i];
//					prev.indices[newn] = prevMatrix.indices[i];
					if (oldsm.ncycles != 0) {
						LCCC<R> lc = new LCCC<R>(oldsm, newsm);
						lc.add(prevcc, 1);
//						prev.values[newn] = new LCCC[prev.rowsizes[newn]];
						CobMatrixRow<R> prevMatrixEntriesI = prevMatrix.entries.get(i);
						CobMatrixRow<R> prevEntriesNewN = prev.entries.get(newn);
						for(int k : prevMatrixEntriesI.keys()) {
							prevEntriesNewN.put(k, lc.compose(prevMatrixEntriesI.get(k)));
						}
//						for (int k = 0; k < prev.rowsizes[newn]; k++) {
//							prev.values[newn][k] = lc.compose(prevMatrix.values[i][k]);
//						}
					} else {
						prev.entries.set(newn, prevMatrix.entries.get(i));
//						prev.values[newn] = prevMatrix.values[i];
					}
				}
				if (next != null) {
					CobMatrix<R> nextMatrix = getMatrix(colnum);
					if (oldsm.ncycles != 0) {
						LCCC<R> lc = new LCCC<R>(newsm, oldsm);
						lc.add(nextcc, 1);
						
						for(int k = 0; k < nextMatrix.entries.size(); ++k) {
							CobMatrixRow<R> rowEntries = nextMatrix.entries.get(k);
							if(rowEntries.containsKey(i)) {
								LCCC<R> nmv = rowEntries.get(i);
								next.append(k, newn, nmv.compose(lc));
							}
						}
//						for (int k = 0; k < nextMatrix.values.length; k++) {
//							for (int l = 0; l < nextMatrix.rowsizes[k]; l++) {
//								if (nextMatrix.indices[k][l] == i) {
//									LCCC nmv = nextMatrix.values[k][l];
//									next.append(k, newn, nmv.compose(lc));
//								}
//							}
//						}
					} else {
						for(int k = 0; k < nextMatrix.entries.size(); ++k) {
							CobMatrixRow<R> rowEntries = nextMatrix.entries.get(k);
							if(rowEntries.containsKey(i)) {
								next.append(k, newn, rowEntries.get(i)); 
							}
						}
//						for (int k = 0; k < nextMatrix.values.length; k++) {
//							for (int l = 0; l < nextMatrix.rowsizes[k]; l++) {
//								if (nextMatrix.indices[k][l] == i) {
//									next.append(k, newn, nextMatrix.values[k][l]);
//								}
//							}
//						}
					}
//					next.trim();
				}

				newn++;
			}
		}

		columns[colnum] = newsc;
		if (prev != null)
			setMatrix(colnum - 1, prev);
		if (next != null)
			setMatrix(colnum, next);
	}

	public void deLoopWithH(int colnum) { // deloops one column
		int size = 0;
		for (int i = 0; i < columns[colnum].n; i++)
			size += 1 << columns[colnum].smoothings[i].ncycles;
		SmoothingColumn newsc = new SmoothingColumn(size);
		CobMatrix<R> prev = null, next = null;
		if (colnum != 0)
			prev = new CobMatrix<R>(columns[colnum - 1], newsc);
		if (colnum != ncolumns - 1)
			next = new CobMatrix<R>(newsc, columns[colnum + 1]);
		int newn = 0;
		for (int i = 0; i < columns[colnum].n; i++) {
			Cap oldsm = columns[colnum].smoothings[i];
			Cap newsm = new Cap(oldsm.n, 0);
			for (int j = 0; j < oldsm.n; j++)
				newsm.pairings[j] = oldsm.pairings[j];
			newsm = Cap.capCache.cache(newsm);
			CannedCobordism prevcc = new CannedCobordism(oldsm, newsm);
			prevcc.ncc = prevcc.nbc;
			prevcc.connectedComponent = CannedCobordism.counting[prevcc.nbc];
			prevcc.dots = new byte[prevcc.ncc];
			prevcc.genus = CannedCobordism.zeros[prevcc.ncc];
			CannedCobordism nextcc = new CannedCobordism(newsm, oldsm);
			nextcc.ncc = nextcc.nbc;
			nextcc.connectedComponent = CannedCobordism.counting[nextcc.nbc];
			nextcc.dots = new byte[nextcc.ncc];
			nextcc.genus = CannedCobordism.zeros[nextcc.ncc];
			for (int j = 0; j < (1 << oldsm.ncycles); j++) {
				int nmod = 0;
				int morechoices[] = new int[oldsm.ncycles], nmore = 0;
				for (int k = 0; k < oldsm.ncycles; k++) {
					if ((j & (1 << k)) == 0) {
						nmod++;
						// these appear to be reversed
						// but it works this way
						// prevcc.dots[prevcc.offtop + k] = 1;
						morechoices[nmore++] = k;
						nextcc.dots[nextcc.offbot + k] = 0;
					} else {
						nmod--;
						prevcc.dots[prevcc.offtop + k] = 0;
						nextcc.dots[nextcc.offbot + k] = 1;
					}
				}
				LCCC<R> prevlc = new LCCC<R>(oldsm, newsm);
				if (prev != null) {
					for (int k = 0; k < (1 << nmore); k++) {
						byte prevdots[] = new byte[prevcc.ncc], prevhpow = 0;
						System.arraycopy(prevcc.dots, 0, prevdots, 0,
								prevcc.ncc);
						for (int l = 0; l < nmore; l++) {
							if ((k & (1 << l)) == 0)
								prevdots[prevcc.offtop + morechoices[l]] = 1;
							else {
								prevdots[prevcc.offtop + morechoices[l]] = 0;
								prevhpow++;
							}
						}
						int coeff;
						if (prevhpow % 2 == 0)
							coeff = 1;
						else
							coeff = -1;
						CannedCobordism newprev = new CannedCobordism(oldsm,
								newsm);
						newprev.ncc = newprev.nbc;
						newprev.connectedComponent = CannedCobordism.counting[newprev.nbc];
						newprev.genus = CannedCobordism.zeros[newprev.ncc];
						newprev.dots = prevdots;
						newprev.hpower = prevhpow;
						prevlc.add(newprev, coeff);
					}
				}
				newsc.smoothings[newn] = newsm;
				newsc.numbers[newn] = columns[colnum].numbers[i] + nmod;
				if (prev != null) {
					CobMatrix<R> prevMatrix = getMatrix(colnum - 1);
//					prev.rowsizes[newn] = prevMatrix.rowsizes[i];
//					prev.indices[newn] = prevMatrix.indices[i];
					if (oldsm.ncycles != 0) {
//						prev.values[newn] = new LCCC[prev.rowsizes[newn]];
						CobMatrixRow<R> prevMatrixEntriesI = prevMatrix.entries.get(i);
						CobMatrixRow<R> prevEntriesNewN = prev.entries.get(newn);
						for(int k : prevMatrixEntriesI.keys()) {
							prevEntriesNewN.put(k, prevlc.compose(prevMatrixEntriesI.get(k)));
						}
//						for (int k = 0; k < prev.rowsizes[newn]; k++) {
//							prev.values[newn][k] = prevlc
//									.compose(prevMatrix.values[i][k]);
//						}
					} else {
						prev.entries.set(newn, prevMatrix.entries.get(i));
//						prev.values[newn] = prevMatrix.values[i];
					}
				}
				if (next != null) {
					CobMatrix<R> nextMatrix = getMatrix(colnum);
					if (oldsm.ncycles != 0) {
						LCCC<R> lc = new LCCC<R>(newsm, oldsm);
						lc.add(nextcc, 1);

						for(int k = 0; k < nextMatrix.entries.size(); ++k) {
							CobMatrixRow<R> rowEntries = nextMatrix.entries.get(k);
							if(rowEntries.containsKey(i)) {
								LCCC<R> nmv = rowEntries.get(i);
								next.append(k, newn, nmv.compose(lc));
							}
						}						
//						for (int k = 0; k < nextMatrix.values.length; k++) {
//							for (int l = 0; l < nextMatrix.rowsizes[k]; l++) {
//								if (nextMatrix.indices[k][l] == i) {
//									next.append(k, newn,
//													nextMatrix.values[k][l]
//															.compose(lc));
//								}
//							}
//						}
					} else {
						for(int k = 0; k < nextMatrix.entries.size(); ++k) {
							CobMatrixRow<R> rowEntries = nextMatrix.entries.get(k);
							if(rowEntries.containsKey(i)) {
								next.append(k, newn, rowEntries.get(i));
							}
						}						
//						for (int k = 0; k < nextMatrix.values.length; k++) {
//							for (int l = 0; l < nextMatrix.rowsizes[k]; l++) {
//								if (nextMatrix.indices[k][l] == i) {
//									next.append(k, newn,
//											nextMatrix.values[k][l]);
//								}
//							}
//						}
					}
//					next.trim();
				}

				newn++;
			}
		}

		columns[colnum] = newsc;
		if (prev != null)
			setMatrix(colnum - 1, prev);
		if (next != null)
			setMatrix(colnum, next);
	}

	public void reductionLemma() {
		for (int i = 0; i < ncolumns - 1; i++)
			reductionLemma(i);
	}

	public boolean reductionLemma(int i) { // does one matrix
		// this assumes delooping has taken place
		boolean found, found2 = false, ret = false;
		CobMatrix<R> m = getMatrix(i);
		if(m.target.n > largestMatrix) {
			largestMatrix = m.target.n;
			log.info("Largest matrix: " + largestMatrix + " rows.");
		}
		int count = 0;
		do {
			found = false;
			rlfor: for (int j = 0; j < m.entries.size(); j++) {
				for(int k : m.entries.get(j).keys()) {
					LCCC<R> lc = m.entries.get(j).get(k);
					if (lc != null && lc.size() == 1) {
						if (!columns[i].smoothings[k]
								.equals(columns[i + 1].smoothings[j])) {
							continue;
						}
						CannedCobordism cc = lc.coefficients.firstKey();
						R n = lc.coefficients.get(cc);
						if (!n.isInvertible()) {
							continue;
						}
						if (!cc.isIsomorphism()) {
							continue;
						}
						found2 = found = true;
						++count;
						reductionLemma(i, j, k, n, false);
						m = getMatrix(i);
						break rlfor;
					}
				}
			}
			if(count > mostReductions) {
				mostReductions = count;
				log.info("Most reductions: " + mostReductions);
			}
			
			if (found)
				ret = true;
			if (!found) {
				if (found2) {
					// System.out.println("Reduce Local:" + i);
					m = getMatrix(i);
					m.reduce();
					setMatrix(i, m);
					found = true;
					found2 = false;
				}
			}
		} while (found);
		return ret;
	}

	public void reductionLemma(int i, int j, int k, R n,
			boolean zeros) {
		// matrices[i].matrix[j][k] is the isomorphism, with coefficient n
		// zeros is true if row j or column k is zero
		SmoothingColumn scb1 = new SmoothingColumn(1);
		scb1.smoothings[0] = columns[i].smoothings[k];
		scb1.numbers[0] = columns[i].numbers[k];
		SmoothingColumn scD = new SmoothingColumn(columns[i].n - 1);
		System.arraycopy(columns[i].smoothings, 0, scD.smoothings, 0, k);
		System.arraycopy(columns[i].smoothings, k + 1, scD.smoothings, k, scD.n
				- k);
		System.arraycopy(columns[i].numbers, 0, scD.numbers, 0, k);
		System.arraycopy(columns[i].numbers, k + 1, scD.numbers, k, scD.n - k);
		SmoothingColumn scb2 = new SmoothingColumn(1);
		scb2.smoothings[0] = columns[i + 1].smoothings[j];
		scb2.numbers[0] = columns[i + 1].numbers[j];
		SmoothingColumn scE = new SmoothingColumn(columns[i + 1].n - 1);
		System.arraycopy(columns[i + 1].smoothings, 0, scE.smoothings, 0, j);
		System.arraycopy(columns[i + 1].smoothings, j + 1, scE.smoothings, j,
				scE.n - j);
		System.arraycopy(columns[i + 1].numbers, 0, scE.numbers, 0, j);
		System.arraycopy(columns[i + 1].numbers, j + 1, scE.numbers, j, scE.n
				- j);

		CobMatrix<R> m = getMatrix(i);

		CobMatrix<R> delta = null, gamma = null;
		if (!zeros) {
			delta = new CobMatrix<R>(scD, scb2);
			CobMatrixRow<R> mRowEntriesJ = m.entries.get(j);
			CobMatrixRow<R> deltaRowEntries0 = delta.entries.get(0);
			for(int c : mRowEntriesJ.keys()) {
				if(c < k) {
					deltaRowEntries0.put(c, mRowEntriesJ.get(c));
				} else if(c > k) {
					deltaRowEntries0.put(c - 1, mRowEntriesJ.get(c));					
				}
 			}
			deltaRowEntries0.compact();
			
//			delta.values[0] = new LCCC[m.rowsizes[j]];
//			delta.indices[0] = new int[m.rowsizes[j]];
//			for (int a = 0; a < m.rowsizes[j]; a++) {
//				int idx = m.indices[j][a];
//				if (idx < k) {
//					delta.values[0][delta.rowsizes[0]] = m.values[j][a];
//					delta.indices[0][delta.rowsizes[0]++] = idx;
//				} else if (idx > k) {
//					delta.values[0][delta.rowsizes[0]] = m.values[j][a];
//					delta.indices[0][delta.rowsizes[0]++] = idx - 1;
//				}
//			}
			// fill this at the same time as epsilon
			gamma = new CobMatrix<R>(scb1, scE);
		}
		CobMatrix<R> epsilon = new CobMatrix<R>(scD, scE);
		
		for (int a = 0, b = 0; a < m.entries.size(); a++) {
			if (a != j) {
				CobMatrixRow<R> mRowEntriesA = m.entries.get(a);
				CobMatrixRow<R> epsilonRowEntriesB = epsilon.entries.get(b);
				for (int c : mRowEntriesA.keys()) {
					if (c < k) {
						epsilonRowEntriesB.put(c, mRowEntriesA.get(c));
					} else if (c > k) {
						epsilonRowEntriesB.put(c - 1, mRowEntriesA.get(c));
					} else if (!zeros) {
						gamma.append(b, 0, mRowEntriesA.get(c));
					}
				}
				epsilonRowEntriesB.compact();
				b++;
			}
		}
		
//		for (int a = 0, b = 0; a < m.values.length; a++) {
//			if (a != j) {
//				epsilon.values[b] = new LCCC[m.rowsizes[a]];
//				epsilon.indices[b] = new int[m.rowsizes[a]];
//				for (int c = 0; c < m.rowsizes[a]; c++) {
//					int idx = m.indices[a][c];
//					if (idx < k) {
//						epsilon.values[b][epsilon.rowsizes[b]] = m.values[a][c];
//						epsilon.indices[b][epsilon.rowsizes[b]++] = idx;
//					} else if (idx > k) {
//						epsilon.values[b][epsilon.rowsizes[b]] = m.values[a][c];
//						epsilon.indices[b][epsilon.rowsizes[b]++] = idx - 1;
//					} else if (!zeros) {
//						gamma.append(b, 0, m.values[a][c]);
//					}
//				}
//				b++;
//			}
//		}
		if (zeros) {
			setMatrix(i, epsilon);
		} else {
			CobMatrix<R> phiinv = new CobMatrix<R>(scb2, scb1);
			CannedCobordism phicc = new CannedCobordism(
					columns[i + 1].smoothings[j], columns[i].smoothings[k]);
			// assume delooping has been done
			// make phicc an isomorphism
			for (byte a = 0; a < phicc.nbc; a++)
				phicc.connectedComponent[a] = a;
			phicc.ncc = phicc.nbc;
			phicc.dots = new byte[phicc.ncc];
			phicc.genus = new byte[phicc.ncc];
			LCCC<R> philc = new LCCC<R>(columns[i + 1].smoothings[j],
					columns[i].smoothings[k]);
			philc.add(phicc, n.inverse().multiply(-1));
			phiinv.append(0, 0, philc);
			CobMatrix<R> gpd = gamma.multiply(phiinv).multiply(delta);
			gpd.add(epsilon);
			setMatrix(i, gpd);
		}
		columns[i] = scD;
		columns[i + 1] = scE;
		if (i != 0) {
			CobMatrix<R> previousMatrix = getMatrix(i - 1);

			previousMatrix.target = columns[i];
			previousMatrix.entries.remove(k);
			setMatrix(i - 1, previousMatrix);

//			CobMatrix beta = new CobMatrix(columns[i - 1], columns[i]);			
//			beta.entries.clear();
//			beta.entries.addAll(previousMatrix.entries.subList(0, k));
//			beta.entries.addAll(previousMatrix.entries.subList(k + 1, previousMatrix.entries.size()));
//			setMatrix(i - 1, beta);
		}
		if (i != ncolumns - 2) {
			CobMatrix<R> nextMatrix = getMatrix(i + 1);

//			nextMatrix.source = columns[i + 1];
//			for(TIntObjectHashMap<LCCC> rowEntry : nextMatrix.entries) {
//				for(int c : rowEntry.keys()) {
//					if(c > j) {
//						rowEntry.put(c - 1, rowEntry.get(c));
//					}
//					if(c >= j) {
//						rowEntry.remove(c);
//					}
//				}
//			}
//			setMatrix(i + 1, nextMatrix);
			
			CobMatrix<R> nu = new CobMatrix<R>(columns[i + 1], columns[i + 2]);
			for(int a = 0; a < nextMatrix.entries.size(); ++a) {
				CobMatrixRow<R> nextMatrixRowEntriesA = nextMatrix.entries.get(a);
				CobMatrixRow<R> nuRowEntriesA = nu.entries.get(a);
				for(int c : nextMatrixRowEntriesA.keys()) {
					if(c < j) {
						nuRowEntriesA.put(c, nextMatrixRowEntriesA.get(c));
					} else if (c > j) {
						nuRowEntriesA.put(c - 1, nextMatrixRowEntriesA.get(c));						
					}
				}
				nuRowEntriesA.compact();
			}
			setMatrix(i + 1, nu);
		}
	}

	private static int takeNextCrossing(int edges[], int pd[][], boolean in[],
			boolean done[], int depth, int retmax[]) {
		for (int i = 0; i < pd.length; ++i) {
			if (!done[i])
				return i;
		}
		assert (false);
		return 0;
	}

	private static int chooseXingRecursive(int edges[], int pd[][],
			boolean in[], boolean done[], int depth, int retmax[]) {
		int nedges = edges.length;
		int best = -1;
//		int nbest = 1000000;
		int nconbest = -1;
		int rbest[] = new int[depth];
		for (int i = 0; i < pd.length; i++) {
			if (!done[i]) {
				int ncon = 0;
				for (int j = 0; j < 4; j++)
					if (in[pd[i][j]])
						ncon++;
				if (ncon == 0 && nedges != 0) {
					continue;
				} else if (ncon < nconbest)
					continue;
				// must be adjacent edges
				int start;
				for (start = 0; start < nedges; start++) {
					boolean found = false;
					for (int k = 0; k < 4; k++)
						if (pd[i][k] == edges[start]) {
							found = true;
							break;
						}
					if (!found)
						break;
				}
				if (start == nedges)
					start = 0;
				for (int k = 0; k < nedges; k++) {
					boolean found = false;
					for (int l = 0; l < 4; l++)
						if (pd[i][l] == edges[(start + k) % nedges]) {
							found = true;
							start = (start + k) % nedges;
							break;
						}
					if (found)
						break;
				}
				int kstart;
				if (nedges == 0)
					kstart = 0;
				else
					for (kstart = 0; kstart < 4; kstart++)
						if (pd[i][kstart] == edges[start])
							break;

				assert kstart != 4;
				boolean good = true;
				for (int k = 0; k < ncon; k++)
					if (pd[i][(kstart + 4 - k) % 4] != edges[(start + k)
							% nedges]) {
						good = false;
						break;
					}
				if (!good)
					continue;
//				int n;
				int getn[] = new int[depth];
				if (depth == 0) {
//					n = nedges + 4 - 2 * ncon;
				} else {
					kstart += 4 - ncon + 1;
					kstart %= 4;
					int newedges[] = new int[nedges + 4 - 2 * ncon];
					int j;
					for (j = 0; j < nedges - ncon; j++)
						newedges[j] = edges[(start + ncon + j) % nedges];
					for (int k = 0; k < 4 - ncon; k++, j++)
						newedges[j] = pd[i][(kstart + ncon + k) % 4];
					boolean prev = done[i];
					done[i] = true;
					boolean previn[] = new boolean[4];
					for (int k = 0; k < 4; k++)
						previn[k] = in[pd[i][k]];
					for (int k = 0; k < 4; k++)
						in[pd[i][k]] = true;
					// int getn[] = new int[depth];
					chooseXingRecursive(newedges, pd, in, done, depth - 1, getn);
//					n = getn[0];
					done[i] = prev;
					for (int k = 0; k < 4; k++)
						in[pd[i][k]] = previn[k];
				}
				boolean better = false;
				if (ncon > nconbest)
					better = true;
				else if (ncon == nconbest)
					for (int j = 0; j < depth; j++)
						if (getn[j] > rbest[j]) {
							better = true;
							break;
						} else if (getn[j] < rbest[j])
							break;
				if (better) {
					nconbest = ncon;
					System.arraycopy(getn, 0, rbest, 0, depth);
					best = i;
				}
			}
		}
		if (best == -1)
			throw new AssertionError();
		// retmax[0] = nbest;
		retmax[0] = nconbest;
		System.arraycopy(rbest, 0, retmax, 1, depth);
		return best;
	}

	private static long peakMemoryInUse;

//	private static long timeSinceLastLap = System.currentTimeMillis();

//	private static long timeElapsed() {
//		long r = System.currentTimeMillis() - timeSinceLastLap;
//		timeSinceLastLap = System.currentTimeMillis();
//		return r;
//	}
	
	private static long memoryInUse() {
		long m = (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory());
		if(m > peakMemoryInUse) peakMemoryInUse = m;
		return m;
	}	

	private static final DateFormat timeFormatter =
	    DateFormat.getTimeInstance(DateFormat.DEFAULT,
                new Locale("en", "US"));
	private static final DateFormat dateFormatter =
	    DateFormat.getDateInstance(DateFormat.DEFAULT,
                new Locale("en", "US"));
	
	private static final NumberFormat memoryFormatter = new DecimalFormat("###,###,###,###");
	
	private static String prependLoggingStatus(String msg) {
//		return "Time: " + System.currentTimeMillis() + " Memory: " + memoryInUse() + " " + msg;
		return timeFormatter.format(new Date()) + " " + dateFormatter.format(new Date()) + 
			" Memory(peak): " + memoryFormatter.format(memoryInUse()) + 
			"(" + memoryFormatter.format(peakMemoryInUse) + ")" + 
			" Cache size(hits): " + // CannedCobordism.vcacheSize() + "/" + CannedCobordism.hcacheSize() + 
				Cap.capCache.size() + "/" + CannedCobordism.cobordismCache.size() + 
			"(" + CannedCobordism.cobordismCache.getNumberOfHits() + "/" + CannedCobordism.cobordismCache.getNumberOfChecks() +
			") " + msg;

	}
	
//	private static void debug(String msg, Throwable t) {
//		log.debug(prependLoggingStatus(msg), t);
//	}
	
	public static void debug(String msg) {
		log.debug(prependLoggingStatus(msg));
	}
	
//	private static void info(String msg, Throwable t) {
//		log.info(" " + prependLoggingStatus(msg), t);
//	}
	
	public static void info(String msg) {
		log.info(" " + prependLoggingStatus( msg));
	}
	
	
	// adds crossings one by one
	@SuppressWarnings("unchecked")
	public static <R extends Ring<R>> Komplex<R> generateFast(int pd[][], int xsigns[],
			boolean reorderCrossings, boolean caching, boolean inMemory) {
		invokeGC();
		
		if (pd.length == 0) { // assume unknot
			Komplex<R> kom = new Komplex<R>(1, true);
			kom.columns[0] = new SmoothingColumn(1);
			kom.columns[0].smoothings[0] = Cap.capCache.cache(new Cap(0, 1));
			kom.reduce();
			return kom;
		}
		boolean in[] = new boolean[pd.length * 4];
		boolean done[] = new boolean[pd.length];
		int pd1[][] = { { 0, 1, 2, 3 } };
		int xsign1[] = new int[1];
		xsign1[0] = 1;
		Komplex<R> kplus = new Komplex<R>(pd1, xsign1, 4, true);
		xsign1[0] = -1;
		Komplex<R> kminus = new Komplex<R>(pd1, xsign1, 4, true);
		Komplex<R> kom;
		int edges[] = new int[0];
		int firstdepth = (pd.length > MAXDEPTH + 1 ? MAXDEPTH : pd.length - 1);
		int firstdummy[] = new int[firstdepth + 1];
		int first = reorderCrossings ? chooseXingRecursive(edges, pd, in, done,
				firstdepth, firstdummy) : takeNextCrossing(edges, pd, in, done,
				firstdepth, firstdummy);
		if (xsigns[first] == 1)
			kom = kplus;
		else
			kom = kminus;
		edges = pd[first];
		int nedges = 4;
		done[first] = true;
		for (int i = 0; i < 4; i++)
			in[pd[first][i]] = true;
		for (int i = 1; i < pd.length; i++) {

			boolean dryRun = false;

			if (caching) {
				/* skip ahead if we can see more cached files */
				if (new File("cache/" + new Integer(i + 1).toString()).exists()) {
					info("More cache files exist, skipping ahead at crossing: "
									+ i);
					dryRun = true;
				}

				/* just load from the cache, if the file already exists */
				File cache = new File("cache/" + new Integer(i).toString());
				if (!dryRun && cache.exists()) {
					try {
						ObjectInputStream deserializer = new ObjectInputStream(
								new FileInputStream(cache));
						info("Beginning to load cached complex for crossing: " + i);
						kom = (Komplex<R>) (deserializer.readObject());
						dryRun = true;
						
						// uncomment this to upconvert serialization versions...
//						System.out.println("Writing the complex back to disk, in the new serialization format...");
//						writeCache(kom, i);
//						System.exit(0);
//						
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
//						log.warn("Trying to delete broken cache file...");
//						cache.deleteOnExit();
//						cache.delete();
						log.warn("Aborting!");
						System.exit(1);
					} finally {
						info("Successfully loaded cached complex.");
					}
				}
			}

			int depth = pd.length - i - 1;
			if (depth > MAXDEPTH)
				depth = MAXDEPTH;
			int dummy[] = new int[depth + 1];
			int best = reorderCrossings ? chooseXingRecursive(edges, pd, in,
					done, depth, dummy) : takeNextCrossing(edges, pd, in, done,
					depth, dummy);
			int nbest = 0;
			for (int j = 0; j < 4; j++)
				if (in[pd[best][j]])
					nbest++;
			int start;
			for (start = 0; start < nedges; start++) {
				boolean found = false;
				for (int j = 0; j < 4; j++)
					if (pd[best][j] == edges[start]) {
						found = true;
						break;
					}
				if (!found)
					break;
			}
			if (start == nedges)
				start = 0;
			for (int j = 0; j < nedges; j++) {
				boolean found = false;
				for (int k = 0; k < 4; k++)
					if (pd[best][k] == edges[(start + j) % nedges]) {
						found = true;
						start = (start + j) % nedges;
						break;
					}
				if (found)
					break;
			}
			int kstart = -1;
			for (int j = 0; j < 4; j++)
				if (pd[best][j] == edges[start]) {
					kstart = j;
					break;
				}
			assert kstart != -1;
			kstart += 4 - nbest + 1;
			kstart %= 4;
			if (!dryRun) {
				info("Crossing number: " + i + "\tGirth: " + nedges
								+ "\t ");
				if (xsigns[best] == 1)
					kom = kom.compose(start, kplus, kstart, nbest, inMemory);
				else
					kom = kom.compose(start, kminus, kstart, nbest, inMemory);
				invokeGC();
				info("Finished composition.");
			}

			assert kom.check(true);
			
			// flush the cobordism cache again!
			CannedCobordism.flushCache();
//			Cap.flushCache();
			
			if (!dryRun) {
				info("reducing");
				kom.reduce();
				invokeGC();
				info("finished reducing");
			}
			int newedges[] = new int[nedges + 4 - 2 * nbest];
			int n;
			for (n = 0; n < nedges - nbest; n++)
				newedges[n] = edges[(start + nbest + n) % nedges];
			for (int j = 0; j < 4 - nbest; j++, n++)
				newedges[n] = pd[best][(kstart + nbest + j) % 4];
			edges = newedges;
			nedges = newedges.length;
			done[best] = true;
			for (int j = 0; j < 4; j++)
				in[pd[best][j]] = true;

			// flush the cobordism cache
			CannedCobordism.flushCache();
//			Cap.flushCache();
			
			if (caching) {
				if (!dryRun) {
					writeCache(kom, i);
				}
			}

		}

		if (JavaKh.using_h && nedges == 2) {
			kom.finalizeH();
		}
		
		log.info("Peak memory usage: " + memoryFormatter.format(peakMemoryInUse));
		
		return kom;
	}

	private static <R extends Ring<R>> void writeCache(Komplex<R> kom, int i) {
		File output = new File("cache/" + new Integer(i).toString());
		output.getParentFile().mkdirs();
		try {
			info("Caching complex after crossing: " + i);
			ObjectOutputStream serializer = new ObjectOutputStream(
					new FileOutputStream(output));
			serializer.writeObject(kom);
			serializer.close();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			log.warn("Trying to delete failed output file...");
			output.deleteOnExit();
			output.delete();
		} finally {
			info("Completed caching complex.");
		}
	}

	// adds the tangle contained in kom to this tangle
	// uses horizontal composition
	// WARNING: this destroys the original Komplex object in the process, for
	// the sake
	// of speedy(?) garbage collection.
	@SuppressWarnings("unchecked")
	public Komplex<R> compose(int start, Komplex<R> kom, int kstart, int nc,
			boolean inMemory) {
		Komplex<R> ret = new Komplex<R>(ncolumns + kom.ncolumns - 1, inMemory);
		if(ret.matrices instanceof CachingList) {
			((CachingList<CobMatrix<R>>)(ret.matrices)).resetCacheSize(1);
		}
		ret.startnum = startnum + kom.startnum;
		ret.nfixed = nfixed + kom.nfixed - 2 * nc;
		int colsizes[] = new int[ret.ncolumns];
		for (int i = 0; i < ret.ncolumns; i++)
			// want columns a from this and b from kom s.t. a + b = i
			for (int j = 0; j <= i && j < ncolumns; j++)
				if (i - j < kom.ncolumns)
					colsizes[i] += columns[j].n * kom.columns[i - j].n;
		for (int i = 0; i < ret.ncolumns; i++) {
			ret.columns[i] = new SmoothingColumn(colsizes[i]);
			// colsizes[i] = 0;
		}
		// fill the columns
		int startnum[][] = new int[ncolumns][kom.ncolumns];
		for (int i = 0; i < ret.ncolumns; i++)
			for (int j = 0, sn = 0; j <= i && j < ncolumns; j++)
				if (i - j < kom.ncolumns) {
					int k = i - j;
					startnum[j][k] = sn;
					for (int l = 0; l < columns[j].n; l++)
						for (int m = 0; m < kom.columns[k].n; m++) {
							ret.columns[i].smoothings[sn] = columns[j].smoothings[l]
									.compose(start,
											kom.columns[k].smoothings[m],
											kstart, nc);
							ret.columns[i].numbers[sn] = columns[j].numbers[l]
									+ kom.columns[k].numbers[m];
							sn++;
						}
				}
		// fill the matrices
		for (int i = 0; i < ret.ncolumns - 1; i++) {
			CobMatrix<R> newMatrix = new CobMatrix<R>(ret.columns[i],
					ret.columns[i + 1]);
			boolean first = true;
			for (int j = 0; j <= i && j < ncolumns; j++)
				if (i - j < kom.ncolumns) {
					int k = i - j;
					if (j < ncolumns - 1) {
						CobMatrix<R> matrixJ = getMatrix(j);
						// entries derived from matrices[j] and kom.columns[k]
						for (int m = 0; m < kom.columns[k].n; m++) {
							CannedCobordism komcc = CannedCobordism
									.isomorphism(kom.columns[k].smoothings[m]);
							for (int n = 0; n < columns[j + 1].n; n++) {
								for(int l : matrixJ.entries.get(n).keys()) {
									LCCC<R> lc = matrixJ.entries.get(n).get(l);
									if (lc != null && lc.size() != 0) {
										lc = lc.compose(start, komcc, kstart,
												nc, false);
										if (lc != null) {
											if (k % 2 == 0) {
												lc.multiply((R)Rings.createInstance(-1));
											}
											newMatrix.append(startnum[j + 1][k]
													+ n * kom.columns[k].n + m,
													startnum[j][k] + l
															* kom.columns[k].n
															+ m, lc);
										}
									}
								}
							}
						}
						if (first
								&& ((j == 0 && k == kom.ncolumns - 1) || j != 0)
								&& ncolumns > 2)
							// WARNING!!! this assumes "this" is to be thrown
							// away.
							setMatrix(j, null); // to be garbage collected
						first = false;
					}
					if (k < kom.ncolumns - 1) {
						// entries derived from kom.matrix[k] and columns[j]
						CobMatrix<R> komMatrixK = kom.getMatrix(k);
						for (int l = 0; l < columns[j].n; l++) {
							CannedCobordism thiscc = CannedCobordism
									.isomorphism(columns[j].smoothings[l]);
							for (int n = 0; n < kom.columns[k + 1].n; n++) {
								for(int m : komMatrixK.entries.get(n).keys()) {
									LCCC<R> lc = komMatrixK.entries.get(n).get(m);
									if (lc != null && lc.size() != 0) {
										lc = lc.compose(kstart, thiscc, start,
												nc, true);
										if (lc != null)
											newMatrix.append(startnum[j][k + 1]
													+ l * kom.columns[k + 1].n
													+ n, startnum[j][k] + l
													* kom.columns[k].n + m, lc);
									}
								}
							}
						}
					}
				}
//			newMatrix.trim();
			debug("Finished composition step " + (i + 1) + "/" + ret.ncolumns);
			ret.matrices.add(newMatrix);
			invokeGC();
		}
		if(ret.matrices instanceof CachingList) {
			((CachingList<CobMatrix<R>>)(ret.matrices)).resetCacheSize(3);
		}
		return ret;
	}

	public static int[] getSigns(int pd[][]) {
		// assume pd is a knot
		// follow it in order assuming pd[][0] is the incoming lower strand
		int xsigns[] = new int[pd.length];
		for (int i = 0; i < pd.length; i++) {
			// from the KnotTheory package
			if (pd[i][1] - pd[i][3] == 1 || pd[i][3] - pd[i][1] > 1)
				xsigns[i] = 1;
			else if (pd[i][3] - pd[i][1] == 1 || pd[i][1] - pd[i][3] > 1)
				xsigns[i] = -1;
			else
				throw new AssertionError("Error finding crossing signs");
		}
		return xsigns;
	}

	// this is currently limited to 31 or 32 crossings
	// of course, memory will be exhausted much sooner
	public Komplex(int pd[][], int xsigns[], int nfixed, boolean inMemory) {
		// pd defines a tangle in PD form, with nfixed fixed points
		this.nfixed = nfixed;
		ncolumns = pd.length + 1; // pd.length is the number of crossings
		columns = new SmoothingColumn[ncolumns];
		this.inMemory = inMemory;
		createMatrixList();
		startnum = 0;
		for (int i = 0; i < pd.length; i++)
			if (xsigns[i] == -1)
				startnum--;

		fillPascal(pd.length);
		for (int i = 0; i < ncolumns; i++) {
			columns[i] = new SmoothingColumn(pascalTriangle[pd.length][i]);
		}
		for (int i = 0; i < ncolumns - 1; i++) {
			matrices.add(new CobMatrix<R>(columns[i], columns[i + 1]));
		}

		int numsmoothings[] = new int[ncolumns];
		// maps a crossing to the two cycles its smoothing touches
		int crossing2cycles[][][] = new int[1 << pd.length][pd.length][2];
		for (int i = 0; i < (1 << pd.length); i++)
			for (int j = 0; j < pd.length; j++)
				for (int k = 0; k < 2; k++)
					crossing2cycles[i][j][k] = -1;
		Cap smoothings[] = new Cap[1 << pd.length];
		int whichColumn[] = new int[1 << pd.length];
		int whichRow[] = new int[1 << pd.length];
		for (int i = 0; i < (1 << pd.length); i++) {
			// i defines a smoothing number
			int smoothing[][] = new int[pd.length * 2][2]; // P(a,b)....
			int num1 = 0; // number of 1 smoothings
			for (int j = 0; j < pd.length; j++) {
				if (((i >> j) & 1) == 0) { // 0 smoothing
					smoothing[2 * j][0] = pd[j][0];
					smoothing[2 * j][1] = pd[j][1];
					smoothing[2 * j + 1][0] = pd[j][2];
					smoothing[2 * j + 1][1] = pd[j][3];
				} else { // 1 smoothing
					smoothing[2 * j][0] = pd[j][0];
					smoothing[2 * j][1] = pd[j][3];
					smoothing[2 * j + 1][0] = pd[j][1];
					smoothing[2 * j + 1][1] = pd[j][2];
					num1++;
				}
			}
			int rsmoothing[][] = new int[pd.length * 4][2];
			for (int j = 0; j < rsmoothing.length; j++)
				rsmoothing[j][0] = rsmoothing[j][1] = -1;
			// java.util.Arrays.fill(rsmoothing, -1);
			for (int j = 0; j < smoothing.length; j++) {
				if (rsmoothing[smoothing[j][0]][0] == -1)
					rsmoothing[smoothing[j][0]][0] = j;// smoothing[j][1];
				else
					rsmoothing[smoothing[j][0]][1] = j;
				if (rsmoothing[smoothing[j][1]][0] == -1)
					rsmoothing[smoothing[j][1]][0] = j;// smoothing[j][0];
				else
					rsmoothing[smoothing[j][1]][1] = j;
			}
			boolean done[] = new boolean[4 * pd.length];
			java.util.Arrays.fill(done, false);
			int ncycles = 0;
			int pairings[] = new int[nfixed];
			for (int j = 0; j < rsmoothing.length && rsmoothing[j][0] != -1; j++) {
				if (done[j])
					continue;
				int dst = j;
				do {
					// IF THINGS ARE BROKEN, HERE IS A LIKELY CULPRIT
					int a;
					if (dst < nfixed)
						a = 0;
					else {
						int b0, b1;
						if (smoothing[rsmoothing[dst][0]][0] == dst)
							b0 = 1;
						else
							b0 = 0;
						if (smoothing[rsmoothing[dst][1]][0] == dst)
							b1 = 1;
						else
							b1 = 0;
						if (done[smoothing[rsmoothing[dst][0]][b0]])
							if (done[smoothing[rsmoothing[dst][1]][b1]])
								// go back to the start if we can
								if (smoothing[rsmoothing[dst][0]][b0] == j)
									a = 0;
								else
									a = 1;
							else
								a = 1;
						else
							a = 0;

						// I hope this works like it should
						crossing2cycles[i][rsmoothing[dst][1] / 2][rsmoothing[dst][1] % 2] = j;
					}
					crossing2cycles[i][rsmoothing[dst][0] / 2][rsmoothing[dst][0] % 2] = j;
					done[dst] = true;
					if (smoothing[rsmoothing[dst][a]][0] == dst)
						dst = smoothing[rsmoothing[dst][a]][1];
					else
						dst = smoothing[rsmoothing[dst][a]][0];
				} while (dst >= nfixed && dst != j);
				if (dst == j) {
					ncycles++;
				} else {
					pairings[j] = dst;
					pairings[dst] = j;
				}
				done[dst] = true;
			}

			int remap[] = new int[rsmoothing.length];
			java.util.Arrays.fill(remap, -1);
			for (int j = 0, k = 0; j < pd.length; j++)
				for (int l = 0; l < 2; l++)
					if (crossing2cycles[i][j][l] < nfixed)
						crossing2cycles[i][j][l] = -1
								- crossing2cycles[i][j][l];
					else if (remap[crossing2cycles[i][j][l]] != -1)
						crossing2cycles[i][j][l] = remap[crossing2cycles[i][j][l]];
					else {
						remap[crossing2cycles[i][j][l]] = k;
						crossing2cycles[i][j][l] = k++;
					}

			Cap c = new Cap(nfixed, ncycles);
			System.arraycopy(pairings, 0, c.pairings, 0, nfixed);
			c = Cap.capCache.cache(c);
			whichColumn[i] = num1;
			whichRow[i] = numsmoothings[num1];
			columns[num1].smoothings[numsmoothings[num1]] = c;
			for (int j = 0; j < pd.length; j++)
				if ((i & (1 << j)) == 0)
					if (xsigns[j] == 1)
						columns[num1].numbers[numsmoothings[num1]]++;
					else
						columns[num1].numbers[numsmoothings[num1]] -= 2;
				else if (xsigns[j] == 1)
					columns[num1].numbers[numsmoothings[num1]] += 2;
				else
					columns[num1].numbers[numsmoothings[num1]]--;
			numsmoothings[num1]++;
			smoothings[i] = c;

			// fill in the cobordisms
			if (i != 0) { // fill in the edges which lead to i
				for (int j = 0; j < pd.length; j++)
					if ((i & (1 << j)) != 0) {
						int k = i ^ (1 << j);
						CannedCobordism cc = new CannedCobordism(smoothings[k],
								c);
						// now, join the components on either side
						// of each smoothed crossing
						// and join them together at crossing j
						java.util.Arrays.fill(cc.connectedComponent, 0, cc.connectedComponent.length, (byte)(-1));
						for (int l = 0; l < pd.length; l++)
							if (l != j) {
								for (int m = 0; m < 2; m++) {
									int x = crossing2cycles[i][l][m];
									if (x < 0)
										x = cc.component[-1 - x];
									else
										x += cc.offbot;
									// this may be unnecessary
									byte cci;
									int y = crossing2cycles[k][l][m];
									if (y < 0)
										y = cc.component[-1 - y];
									else
										y += cc.offtop;
									if (cc.connectedComponent[y] != -1)
										cci = cc.connectedComponent[y];
									else if (cc.connectedComponent[x] != -1)
										cci = cc.connectedComponent[x];
									else
										cci = cc.ncc++;
									// IMPORTANT!!!!!
									// make sure the last component in
									// crossing2cycles works in a consistent
									// manner!!!
									cc.connectedComponent[y] = cci;
									cc.connectedComponent[x] = cci;
									// this might not be enough
								}
							} else { // l == j
								// join both sides at top and bottom
								int num[] = new int[4];
								num[0] = crossing2cycles[i][l][0];
								num[1] = crossing2cycles[i][l][1];
								num[2] = crossing2cycles[k][l][0];
								num[3] = crossing2cycles[k][l][1];
								for (int x = 0; x < 4; x++)
									if (num[x] >= 0)
										if (x < 2)
											num[x] += cc.offbot;
										else
											num[x] += cc.offtop;
									else
										num[x] = cc.component[-1 - num[x]];
								byte cci = -1;
								for (int x = 0; x < 4; x++)
									if (cc.connectedComponent[num[x]] != -1) {
										cci = cc.connectedComponent[num[x]];
										break;
									}
								if (cci == -1)
									cci = cc.ncc++;
								for (int x = 0; x < 4; x++)
									if (cc.connectedComponent[num[x]] != cci) {
										int y = cc.connectedComponent[num[x]];
										if (y != -1) {
											for (int z = 0; z < cc.nbc; z++)
												if (cc.connectedComponent[z] == y)
													cc.connectedComponent[z] = cci;
										}
										cc.connectedComponent[num[x]] = cci;
									}
							}
						// relabel cc's connectedComponents in ascending order
						cc.ncc = 0;
						for (int l = 0; l < cc.nbc; l++)
							if (cc.connectedComponent[l] > cc.ncc) { // swap
								byte x = cc.connectedComponent[l];
								for (int m = l; m < cc.nbc; m++)
									if (cc.connectedComponent[m] == x)
										cc.connectedComponent[m] = cc.ncc;
									else if (cc.connectedComponent[m] == cc.ncc)
										cc.connectedComponent[m] = x;
								cc.ncc++;
							} else if (cc.connectedComponent[l] == cc.ncc)
								cc.ncc++;
						cc.dots = cc.genus = CannedCobordism.zeros[cc.ncc];
						LCCC<R> lc = new LCCC<R>(cc.top, cc.bottom);
						int num = 1;
						for (int l = j + 1; l < pd.length; l++) {
							if ((i & (1 << l)) != 0) {
								num = -num;
							}
						}
						lc.add(cc, num);

						CobMatrix<R> m = getMatrix(num1 - 1);
						m.append(whichRow[i], whichRow[k], lc);
						setMatrix(num1 - 1, m);
					}
			}
		}
	}

	public boolean check(boolean reduce) { // checks that d^2 = 0
		for (int i = 1; i < matrices.size(); i++) {
			CobMatrix<R> cm = getMatrix(i).multiply(getMatrix(i - 1));
			if (!cm.isZero()) {
				if (reduce) {
					cm.reduce();
					if (!cm.isZero())
						return false;
				} else
					return false;
			}
		}
		return true;
	}

	private void createMatrixList() {
		if (inMemory) {
			matrices = new ArrayList<CobMatrix<R>>(ncolumns - 1);
		} else {
			matrices = new CachingList<CobMatrix<R>>(
					new DiskBackedList<CobMatrix<R>>(new File(System
							.getProperty("java.io.tmpdir"))), 3);
		}
	}

	private void writeObject(ObjectOutputStream s) throws IOException {
		s.defaultWriteObject();
		s.writeInt(1); // Serialization version
		s.writeInt(matrices.size());
		int i = 0;
		if (matrices instanceof SerializingList) {
			s.writeBoolean(true);
			for (File file : ((SerializingList<CobMatrix<R>>) (matrices))
					.getSerializedForms()) {
				debug("Writing height " + (++i) + "/" + matrices.size());
				s.writeLong(file.length());
				s.writeInt(Integer.parseInt(file.getName()));
				IOUtils.copy(new FileInputStream(file), s);
			}
		} else {
			s.writeBoolean(false);
			for (CobMatrix<R> m : matrices) {
				debug("Writing height " + (++i) + "/" + matrices.size());
				s.writeObject(m);
				invokeGC();
			}
		}
	}

	@SuppressWarnings("unchecked")
	private void readObject(ObjectInputStream s) throws IOException,
			ClassNotFoundException {
		s.defaultReadObject();
    	int serializationVersion = s.readInt();
    	if(serializationVersion < 1 || serializationVersion > 1) {
    		log.warn("Serialization version looks wrong...");
    	}
		int size = s.readInt();
		createMatrixList();
		if(s.readBoolean()) {
			for(int i = 0; i < size; ++i) {
				debug("Reading height " + (i + 1) + "/" + size);
				long fileLength = s.readLong();
				int hash = s.readInt();
				InputStream lsis = new LimitedSizeInputStream(s, fileLength);
				if(matrices instanceof SerializingList) {
					matrices.add(null);
					((SerializingList<CobMatrix<R>>) matrices).setSerializedForm(i, hash, lsis);
				} else {
					ObjectInputStream p = new ObjectInputStream(lsis);
					matrices.add((CobMatrix<R>) (p.readObject()));
					invokeGC();
				}
			}
		} else {
			for (int i = 0; i < size; ++i) {
				debug("Reading height " + (i + 1) + "/" + size);
				matrices.add((CobMatrix<R>) (s.readObject()));
				invokeGC();
			}
		}
	}
	
}
