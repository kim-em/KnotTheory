package org.katlas.JavaKh;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.katlas.JavaKh.algebra.Ring;
import org.katlas.JavaKh.algebra.implementations.SingleTermLinearCombo;
import org.katlas.JavaKh.algebra.rings.Rings;
import org.katlas.JavaKh.interfaces.CannedCobordism;
import org.katlas.JavaKh.interfaces.LCCC;

public class SingleTermLCCC<R extends Ring<R>> extends
		SingleTermLinearCombo<R, Cap, CannedCobordism, LCCC<R>> implements
		LCCC<R>, Serializable {

	/**
	 * 
	 */
	private static final long serialVersionUID = -28865833270013691L;

	public SingleTermLCCC(CannedCobordism cc, R coefficient) {
		super(cc, coefficient);
	}

	@Override
	public LCCC<R> fixedZeroLinearCombo(Cap source, Cap target) {
		return null;
	}

	public LCCC<R> singleTermLinearCombo(CannedCobordism cc, R r) {
		return new SingleTermLCCC<R>(cc, r);
	}
	
	@Override
	public LCCC<R> flexibleZeroLinearCombo(Cap source, Cap target) {
		return new LCCCMap<R>(source, target);
	}

	public LCCC<R> compose(int start, CannedCobordism cc, int cstart, int nc,
			boolean reverse) {
		CannedCobordism result;
		if(reverse) {
			result = cc.compose(cstart, mor, start, nc);
		} else {
			result = mor.compose(start, cc, cstart, nc);
		}
		return singleTermLinearCombo(result, coefficient);
	}

	public LCCC<R> finalizeH() {
		// TODO Auto-generated method stub
		return null;
	}

	public LCCC<R> reduce() {
		LCCC<R> ret = new LCCCMap<R>(source(), target());
			
			if(!(mor instanceof CannedCobordismImpl)) {
				throw new UnsupportedOperationException();
			}
			CannedCobordismImpl cc = (CannedCobordismImpl)mor;
			
			R num = coefficient;
			cc.reverseMaps();
			byte dots[] = new byte[cc.nbc];
			byte genus[] = CannedCobordismImpl.zeros[cc.nbc];
			int moreWork[] = new int[cc.ncc];
			int nmoreWork = 0;
			boolean kill = false;
			for (int i = 0; i < cc.ncc; i++)
				if (cc.genus[i] + cc.dots[i] > 1) {
					kill = true;
				} else if (cc.boundaryComponents[i].length == 0) {
					if (cc.genus[i] == 1)
						num = num.multiply(2);
					else if (cc.dots[i] == 0)
						kill = true;
				} else if (cc.boundaryComponents[i].length == 1) {
					dots[cc.boundaryComponents[i][0]] = (byte) (cc.dots[i] + cc.genus[i]);
					if (cc.genus[i] == 1)
						num = num.multiply(2);
				} else {
					// neck cutting relation
					if (cc.genus[i] + cc.dots[i] == 1) { // only one choice
						if (cc.genus[i] == 1) {
							// there is a 2 from the torus
							num = num.multiply(2);
						}
						// use dots to cancel out the other factors
						for (int j = 0; j < cc.boundaryComponents[i].length; j++)
							dots[cc.boundaryComponents[i][j]] = 1;
					} else {
						// cc.bC[i].length choices
						// use dots to cancel out all the factors, except
						// one which is cancelled by the torus
						// do these last
						moreWork[nmoreWork++] = i;
					}
				}
			if (kill)
				return null;
			byte neckCutting[][] = new byte[1][];
			neckCutting[0] = dots;
			for (int i = 0; i < nmoreWork; i++) {
				int concomp = moreWork[i];
				int nbc = cc.boundaryComponents[concomp].length;
				byte newarr[][] = new byte[neckCutting.length * nbc][cc.nbc];
				for (int j = 0; j < neckCutting.length; j++) {
					System.arraycopy(neckCutting[j], 0, newarr[j * nbc], 0,
							cc.nbc);
					for (int k = 0; k < nbc; k++)
						newarr[j * nbc][cc.boundaryComponents[concomp][k]] = 1;
					for (int k = 1; k < nbc; k++)
						System.arraycopy(newarr[j * nbc], 0,
								newarr[j * nbc + k], 0, cc.nbc);
					for (int k = 0; k < nbc; k++)
						newarr[j * nbc + k][cc.boundaryComponents[concomp][k]] = 0;
				}
				neckCutting = newarr;
			}
			byte connectedComponent[] = CannedCobordismImpl.counting[cc.nbc];
			for (int i = 0; i < neckCutting.length; i++) {
				CannedCobordismImpl newcc = new CannedCobordismImpl(source(), target());
				// IMPORTANT!!! in order for them to safely share arrays
				// CannedCobordisms must be treated as immutable
				newcc.connectedComponent = connectedComponent;
				newcc.ncc = newcc.nbc;
				newcc.genus = genus;
				newcc.dots = neckCutting[i];
				ret = ret.add(newcc, num);
			}
		
		if (ret.numberOfTerms() == 0) {
			return null;
		} else if (ret.numberOfTerms() == 1) {
			return singleTermLinearCombo(ret.firstTerm(), ret.firstCoefficient());
		} else {
			if(ret instanceof LCCCMap) {
				((LCCCMap<R>)ret).alreadyReduced = true;
			} else {
				assert false;
			}
			return ret;
		}
	}

	public LCCC<R> reduceWithH() {
		Rings<R> ring = Rings.current();
		
		LCCC<R> ret = new LCCCMap<R>(source(), target());
			
			if(!(mor instanceof CannedCobordismImpl)) {
				throw new UnsupportedOperationException();
			}
			CannedCobordismImpl cc = (CannedCobordismImpl)mor;
			
			R num = coefficient;
			cc.reverseMaps();
			byte dots[] = new byte[cc.nbc];
			int hpow = cc.hpower;
			int moreWork[] = new int[cc.ncc];
			int nmoreWork = 0;
			boolean kill = false;
			for (int i = 0; i < cc.ncc; i++)
				if (cc.boundaryComponents[i].length == 0) {
					if (cc.dots[i] > 0)
						hpow += cc.dots[i] + cc.genus[i] - 1;
					else if (cc.genus[i] % 2 == 0)
						kill = true;
					else {
						num = num.multiply(2);
						hpow += cc.genus[i] - 1;
					}
				} else if (cc.boundaryComponents[i].length == 1) {
					if (cc.dots[i] > 0) {
						hpow += cc.dots[i] + cc.genus[i] - 1;
						dots[cc.boundaryComponents[i][0]] = 1;
					} else if (cc.genus[i] % 2 == 0)
						hpow += cc.genus[i];
					else
						moreWork[nmoreWork++] = i;
				} else {
					if (cc.dots[i] > 0) {
						// the dot and the -h terms cancel since dot == h
						for (int j = 0; j < cc.boundaryComponents[i].length; j++)
							dots[cc.boundaryComponents[i][j]] = 1;
						hpow += cc.dots[i] + cc.genus[i] - 1;
					} else
						moreWork[nmoreWork++] = i;
				}
			if (kill)
				return null;
			byte nCdots[][] = new byte[1][];
			int nChpow[] = new int[1];
			List<R> nCnum = new ArrayList<R>(1);
			nCdots[0] = dots;
			nChpow[0] = hpow;
			nCnum.set(0, num);
			for (int i = 0; i < nmoreWork; i++) {
				int concomp = moreWork[i];
				int nbc = cc.boundaryComponents[concomp].length;
				assert cc.dots[concomp] == 0;
				byte newdots[][] = new byte[nCdots.length << nbc][cc.nbc];
				int newhpow[] = new int[nChpow.length << nbc];
				List<R> newnum = new ArrayList<R>(nCnum.size() << nbc);
				for (int j = 0; j < nCdots.length; j++) {
					for (int k = 0; k < (1 << nbc); k++) {
						int idx = (j << nbc) + k;
						System.arraycopy(nCdots[j], 0, newdots[idx], 0, cc.nbc);
						newhpow[idx] = nChpow[j];
						newnum.set(idx, nCnum.get(j));
						int nzeros = 0;
						for (int l = 0; l < nbc; l++)
							if ((k & (1 << l)) == 0) {
								newdots[idx][cc.boundaryComponents[concomp][l]] = 0;
								nzeros++;
							} else
								newdots[idx][cc.boundaryComponents[concomp][l]] = 1;
						R nmul = ring.ZERO;
						int hmod = 0;
						boolean hset = false;
						for (int l = 0; l < (1 << nzeros); l++) {
							int ndots = 0;
							for (int m = 0; m < nzeros; m++)
								if ((l & (1 << m)) == 0)
									ndots++;
							if (ndots > 0) {
								if (hset) {
									if (hmod != nzeros + cc.genus[concomp] - 1)
										throw new AssertionError();
								} else {
									hmod = nzeros + cc.genus[concomp] - 1;
									hset = true;
								}
								int n = 1;
								for (int o = 0; o < nzeros - ndots; o++)
									n = -n;
								nmul = nmul.add(ring.createInstance(n));
							} else if (cc.genus[concomp] % 2 == 0)
								continue; // coefficient of zero
							else {
								if (hset) {
									if (hmod != nzeros + cc.genus[concomp] - 1)
										throw new AssertionError();
								} else {
									hmod = nzeros + cc.genus[concomp] - 1;
									hset = true;
								}
								int n = 2;
								for (int o = 0; o < nzeros; o++)
									n = -n;
								nmul = nmul.add(ring.createInstance(n));
							}
						}
						newhpow[idx] += hmod;
						newnum.set(idx, newnum.get(idx).multiply(nmul));
					}
				}
				nCdots = newdots;
				nChpow = newhpow;
				nCnum = newnum;
			}
			for (int i = 0; i < nCdots.length; i++) {
				CannedCobordismImpl newcc = new CannedCobordismImpl(source(), target());
				newcc.connectedComponent = CannedCobordismImpl.counting[newcc.nbc];
				newcc.ncc = newcc.nbc;
				newcc.genus = CannedCobordismImpl.zeros[cc.nbc];
				newcc.dots = nCdots[i];
				newcc.hpower = nChpow[i];
				ret = ret.add(newcc, nCnum.get(i));
			}
		if (ret.numberOfTerms() == 0) {
			return null;
		} else {
			if(ret instanceof LCCCMap) {
				((LCCCMap<R>)ret).alreadyReduced = true;
			} else {
				assert false;
			}
			return ret;
		}
	}

	public LCCC<R> compact() {
		if(coefficient.isZero()) {
			return fixedZeroLinearCombo(source(), target());
		} else {
			return this;
		}
	}

}
