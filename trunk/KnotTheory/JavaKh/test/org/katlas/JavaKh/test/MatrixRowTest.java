package org.katlas.JavaKh.test;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.junit.Test;
import org.katlas.JavaKh.rows.MatrixRow;

public abstract class MatrixRowTest<Row extends MatrixRow<String>> {

	protected Row row;

	@Test
	public void testPut1() {
		row.put(2, "1");
		assertEquals("1", row.get(2));
		row.put(2, "2");
		assertEquals("2", row.get(2));
		row.put(3, "3");
		assertEquals("3", row.get(3));
		row.put(3, "4");
		assertEquals("4", row.get(3));
		row.put(2, "5");
		assertEquals("5", row.get(2));
		row.put(2, "6");
		assertEquals("6", row.get(2));
	}

	@Test
	public void testPut2() {
		Iterator<Integer> iterator;

		row.put(3, "3");
		row.put(2, "2");
		row.put(1, "1");

		iterator = row.keys().iterator();
		assertEquals(1, iterator.next());
		assertEquals(2, iterator.next());
		assertEquals(3, iterator.next());
	}

	@Test
	public void testContainsKey() {
		row.put(3, "3");
		assertEquals("3", row.get(3));
	}

	@Test
	public void testDecrementIndexesAbove() {
		row.put(5, "5");
		row.put(6, "6");
		row.put(8, "8");
		row.decrementIndexesAbove(7);
		assertEquals("8", row.get(7));
	}

	@Test
	public void testKeys() {
		List<Integer> ints = new ArrayList<Integer>();
		ints.add(5);
		ints.add(6);
		ints.add(8);
		for (int i : ints) {
			row.put(i, Integer.toString(i));
		}
		for (int k : row.keys()) {
			assertEquals(k, ints.get(0));
			ints.remove(0);
		}
	}

	@Test
	public void testRemove1() {
		row.put(1, "1");
		row.put(2, "2");
		row.remove(1);
		for (int i : row.keys()) {
			assertEquals("2", row.get(i));
		}
	}

	@Test
	public void testRemove2() {
		row.put(1, "1");
		row.put(2, "2");
		row.remove(2);
		for (int i : row.keys()) {
			assertEquals("1", row.get(i));
		}
	}

	@Test
	public void testRemove3() {
		Iterator<Integer> iterator;

		row.put(3, "3");
		row.put(2, "2");
		row.put(1, "1");

		row.remove(2);
		
		iterator = row.keys().iterator();
		assertEquals(1, iterator.next());
		assertEquals(3, iterator.next());

	}

	@Test
	public void testDecrementRemove() {
		row.put(1, "1");
		row.put(2, "2");
		row.put(3, "3");
		row.put(4, "4");
		row.remove(2);
		assertEquals(null, row.get(2));
		row.decrementIndexesAbove(2);
		assertEquals("3", row.get(2));
		row.remove(2);
		row.decrementIndexesAbove(2);
		assertEquals("4", row.get(2));
	}

}
