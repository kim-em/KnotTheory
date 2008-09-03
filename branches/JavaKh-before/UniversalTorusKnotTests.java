

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;

import org.junit.BeforeClass;
import org.junit.Test;


public class UniversalTorusKnotTests {

	private static final String pdT43 = "PD[X[4,16,5,15],X[5,11,6,10],X[16,12,1,11],X[1,7,2,6],X[12,8,13,7],X[9,15,10,14],X[13,3,14,2],X[8,4,9,3]]";
	private static final String T43_umodule = "q^5*t^0*Z[0] + q^7*t^0*Z[0]";
	
	private static final String pdT65 = "PD[X[1, 31, 2, 30], X[48, 40, 1, 39], X[40, 32, 41, 31], X[10, 30, 11, 29], X[9, 39, 10, 38], X[8, 48, 9, 47], X[11, 21, 12, 20], X[19, 29, 20, 28], X[2, 22, 3, 21], X[3, 13, 4, 12], X[18, 38, 19, 37], X[17, 47, 18, 46], X[41, 23, 42, 22], X[16, 8, 17, 7], X[32, 24, 33, 23], X[42, 14, 43, 13], X[33, 15, 34, 14], X[24, 16, 25, 15], X[43, 5, 44, 4], X[34, 6, 35, 5], X[25, 7, 26, 6], X[35, 45, 36, 44], X[26, 46, 27, 45], X[27, 37, 28, 36]]";
	private static final String T65_umodule = "q^19*t^0*Z[0] + q^21*t^0*Z[0] + q^39*t^11*Z[0]";
	
	private static final String pdT76 = "PD[X[13, 25, 14, 24], X[3, 15, 4, 14], X[2, 26, 3, 25], X[12, 36, 13, 35], X[1, 37, 2, 36], X[23, 35, 24, 34], X[70, 48, 1, 47], X[11, 47, 12, 46], X[22, 46, 23, 45], X[33, 45, 34, 44], X[10, 58, 11, 57], X[21, 57, 22, 56], X[32, 56, 33, 55], X[43, 55, 44, 54], X[69, 59, 70, 58], X[59, 49, 60, 48], X[60, 38, 61, 37], X[61, 27, 62, 26], X[49, 39, 50, 38], X[50, 28, 51, 27], X[62, 16, 63, 15], X[63, 5, 64, 4], X[51, 17, 52, 16], X[52, 6, 53, 5], X[53, 65, 54, 64], X[39, 29, 40, 28], X[40, 18, 41, 17], X[42, 66, 43, 65], X[41, 7, 42, 6], X[9, 69, 10, 68], X[20, 68, 21, 67], X[31, 67, 32, 66], X[29, 19, 30, 18], X[19, 9, 20, 8], X[30, 8, 31, 7]]";
	private static final String T76_umodule = "q^29*t^0*Z[0] + q^31*t^0*Z[0] + q^53*t^15*Z[0,0] + q^53*t^16*Z[0] + q^55*t^17*Z[0] + q^55*t^19*Z[0,0]";
	
	
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		BaseRing.setRing("Int");
		JavaKh.using_h = true;
	}
	
	private int[][] parseKnot(String pd) throws IOException {
		BufferedReader br = new BufferedReader(new StringReader(pd));
		return Komplex.getPD(br);
	}
	
	private Komplex createComplex(int[][] knot) {
		return Komplex.generateFast(knot, Komplex.getSigns(knot));
	}
	
	private Komplex createComplex(String pd) throws IOException {
		return createComplex(parseKnot(pd));
	}
	
	@Test
	public void testT43() throws IOException {
		assertEquals(T43_umodule, createComplex(pdT43).KhForZ());
	}

	@Test
	public void testT65() throws IOException {
		assertEquals(T65_umodule, createComplex(pdT65).KhForZ());
	}

	@Test
	public void testT76() throws IOException {
		assertEquals(T76_umodule, createComplex(pdT76).KhForZ());
	}

}
