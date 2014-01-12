package unitTests;

import static org.junit.Assert.*;
import logicClasses.*;

import org.junit.Test;
import org.junit.Before;
import org.junit.AfterClass;

//The testable behaviour of Entry, Exit and Way Points, at this point in time, 
//does not differ (graphics) from the testing conducted under Point_Tests. This 
//file is here for updating should future behaviour be given that distinguishes 
//this class from the Point superclass.

public class ExitPoint_Tests {
	
	private ExitPoint testexitpointZero, testexitpointPositive, testexitpointNegative;
	
	@Before
	public void setUp(){
		testexitpointZero = new ExitPoint(0,1);
		testexitpointPositive = new ExitPoint(100,101);
		testexitpointNegative = new ExitPoint (-100, -101);
	// No constraints applied directly to point constructor; no irregular behaviour should occur
		// from any non-numeric values for x and y.
		
	}
	
	//Testing Mutators & Accessors
	
	// Note for getX_test(), getY_test(), setX_test, setY_test();
	// 0.1 at the end of each test's argument list outlines the tolerance for the 
	// doubles being compared: difference can be as great as 0.1 before test fails. 
	
	@Test
	public void getXTest(){
		assertEquals("getX() test zero value", testexitpointZero.getX(), 0, 0.1);
		assertEquals("getX() test positive value", testexitpointPositive.getX(), 100, 0.1);
		assertEquals("getX() test negative value", testexitpointNegative.getX(), -100, 0.1);
	}
	
	@Test
	public void getYTest(){
		assertEquals("getY() test zero value", testexitpointZero.getY(), 1, 0.1);
		assertEquals("getY() test positive value", testexitpointPositive.getY(), 101, 0.1);
		assertEquals("getY() test negative", testexitpointNegative.getY(), -101, 0.1);
	}
	
	@Test
	public void setXTest(){
		testexitpointPositive.setX(1000);
		testexitpointNegative.setX(-1000);
		assertEquals("setX(double) test, positive double argument", testexitpointPositive.getX(), 1000, 0.1);
		assertEquals("setX(double) test, negative double argument", testexitpointNegative.getX(), -1000, 0.1);
	}
	
	@Test
	public void setYTest(){
		testexitpointPositive.setY(1001);
		testexitpointNegative.setY(-1001);
		assertEquals("setY(double) test, positive double argument", testexitpointPositive.getY(), 1001, 0.1);
		assertEquals("setY(double) test, negative double argument", testexitpointNegative.getY(), -1001, 0.1);
	}
	
	@Test
	public void getPointRefTest(){
		testexitpointZero.setPointRef("success");
		assertEquals("getPointRef(string) test", testexitpointZero.getPointRef(), "success");
	}
	
	@Test
	public void setPointRefTest(){
		testexitpointZero.setPointRef("success");
		assertEquals("setPointRef(string) test", testexitpointZero.getPointRef(), "success");
	}
	
	@Test
	public void getPointCountTest(){
		assertEquals("getPointCount() test; three created", testexitpointZero.getPointCount(), 3);
	}
}
