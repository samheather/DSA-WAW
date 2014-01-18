package unitTests;

import static org.junit.Assert.*;
import logicClasses.*;

import org.junit.Test;
import org.junit.Before;

public class Point_Tests {

	private Point testpointZero, testpointPositive, testpointNegative;
	
	@Before
	public void setUp(){
		testpointZero = new Point(0,1);
		testpointPositive = new Point(100,101);
		testpointNegative = new Point (-100, -101);
	// No constraints applied directly to point constructor; no irregular behaviour should occur
		// from any non-numeric values for x and y.
		
	}
	
	//Testing Mutators & Accessors
	
	// Note for getX_test(), getY_test(), setX_test, setY_test();
	// 0.1 at the end of each test's argument list outlines the tolerance for the 
	// doubles being compared: difference can be as great as 0.1 before test fails. 
	
	@Test
	public void getXTest(){
		assertEquals("getX() test zero value", testpointZero.getX(), 0, 0.1);
		assertEquals("getX() test positive value", testpointPositive.getX(), 100, 0.1);
		assertEquals("getX() test negative value", testpointNegative.getX(), -100, 0.1);
	}
	
	@Test
	public void getYTest(){
		assertEquals("getY() test zero value", testpointZero.getY(), 1, 0.1);
		assertEquals("getY() test positive value", testpointPositive.getY(), 101, 0.1);
		assertEquals("getY() test negative", testpointNegative.getY(), -101, 0.1);
	}
	
	@Test
	public void setXTest(){
		testpointPositive.setX(1000);
		testpointNegative.setX(-1000);
		assertEquals("setX(double) test, positive double argument", testpointPositive.getX(), 1000, 0.1);
		assertEquals("setX(double) test, negative double argument", testpointNegative.getX(), -1000, 0.1);
	}
	
	@Test
	public void setYTest(){
		testpointPositive.setY(1001);
		testpointNegative.setY(-1001);
		assertEquals("setY(double) test, positive double argument", testpointPositive.getY(), 1001, 0.1);
		assertEquals("setY(double) test, negative double argument", testpointNegative.getY(), -1001, 0.1);
	}
	
	@Test
	public void getPointRefTest(){
		testpointZero.setPointRef("success");
		assertEquals("getPointRef(string) test", testpointZero.getPointRef(), "success");
	}
	
	@Test
	public void setPointRefTest(){
		testpointZero.setPointRef("success");
		assertEquals("setPointRef(string) test", testpointZero.getPointRef(), "success");
	}
	

}


