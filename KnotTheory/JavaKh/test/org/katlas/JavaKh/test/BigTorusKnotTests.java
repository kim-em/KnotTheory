package org.katlas.JavaKh.test;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;

import org.junit.BeforeClass;
import org.junit.Test;
import org.katlas.JavaKh.Komplex;
import org.katlas.JavaKh.algebra.rings.Int;
import org.katlas.JavaKh.algebra.rings.Rational;
import org.katlas.JavaKh.algebra.rings.Rings;


public class BigTorusKnotTests {

	private static final String pdT76 = "PD[X[13, 25, 14, 24], X[3, 15, 4, 14], X[2, 26, 3, 25], X[12, 36, 13, 35], X[1, 37, 2, 36], X[23, 35, 24, 34], X[70, 48, 1, 47], X[11, 47, 12, 46], X[22, 46, 23, 45], X[33, 45, 34, 44], X[10, 58, 11, 57], X[21, 57, 22, 56], X[32, 56, 33, 55], X[43, 55, 44, 54], X[69, 59, 70, 58], X[59, 49, 60, 48], X[60, 38, 61, 37], X[61, 27, 62, 26], X[49, 39, 50, 38], X[50, 28, 51, 27], X[62, 16, 63, 15], X[63, 5, 64, 4], X[51, 17, 52, 16], X[52, 6, 53, 5], X[53, 65, 54, 64], X[39, 29, 40, 28], X[40, 18, 41, 17], X[42, 66, 43, 65], X[41, 7, 42, 6], X[9, 69, 10, 68], X[20, 68, 21, 67], X[31, 67, 32, 66], X[29, 19, 30, 18], X[19, 9, 20, 8], X[30, 8, 31, 7]]";
	private static final String T76_polynomial = "q^29*t^0 + q^31*t^0 + q^33*t^2 + q^37*t^3 + q^35*t^4 + q^37*t^4 + q^39*t^5 + q^41*t^5 + q^37*t^6 + q^39*t^6 + q^41*t^7 + q^43*t^7 + q^39*t^8 + 2*q^41*t^8 + q^43*t^9 + 2*q^45*t^9 + q^41*t^10 + 2*q^43*t^10 + q^45*t^11 + 3*q^47*t^11 + 2*q^45*t^12 + q^47*t^12 + q^51*t^12 + 3*q^49*t^13 + q^51*t^13 + q^47*t^14 + q^49*t^14 + q^53*t^14 + 2*q^51*t^15 + 2*q^53*t^15 + q^49*t^16 + q^51*t^16 + q^55*t^16 + q^57*t^16 + q^53*t^17 + q^55*t^17 + q^53*t^18 + q^57*t^19 ";
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		Rings.setRing("Rational");
	}
	
	private int[][] parseKnot(String pd) throws IOException {
		BufferedReader br = new BufferedReader(new StringReader(pd));
		return Komplex.getPD(br);
	}
	
	private Komplex<Rational> createComplex(int[][] knot) {
		return Komplex.generateFast(knot, Komplex.getSigns(knot), false, false, true);
	}
	
	private Komplex<Rational> createComplex(String pd) throws IOException {
		return createComplex(parseKnot(pd));
	}
	
	@Test
	public void testT76() throws IOException {
		assertEquals(T76_polynomial, createComplex(pdT76).Kh());
	}

}