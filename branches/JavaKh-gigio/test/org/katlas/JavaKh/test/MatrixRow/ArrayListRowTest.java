package org.katlas.JavaKh.test.MatrixRow;


import org.junit.Before;
import org.katlas.JavaKh.rows.ArrayListRow;

public class ArrayListRowTest extends MatrixRowTest<ArrayListRow<String>> {

	@Before
	public void setUp() throws Exception {
		row = new ArrayListRow<String>();
	}

}
