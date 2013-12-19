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

public class EntryPoint_Tests {
	
	private EntryPoint testentrypointZero, testentrypointPositive, testentrypointNegative;
	
	@Before
	public void setUp(){
		testentrypointZero = new EntryPoint(0,1);
		testentrypointPositive = new EntryPoint(100,101);
		testentrypointNegative = new EntryPoint (-100, -101);
	// No constraints applied directly to point constructor; no irregular behaviour should occur
		// from any non-numeric values for x and y.
		
	}
	
	//Testing Mutators & Accessors
	
	// Note for getX_test(), getY_test(), setX_test, setY_test();
	// 0.1 at the end of each test's argument list outlines the tolerance for the 
	// doubles being compared: difference can be as great as 0.1 before test fails. 
	
	@Test
	public void getX_test(){
		assertEquals("getX() test zero value", testentrypointZero.getX(), 0, 0.1);
		assertEquals("getX() test positive value", testentrypointPositive.getX(), 100, 0.1);
		assertEquals("getX() test negative value", testentrypointNegative.getX(), -100, 0.1);
	}
	
	@Test
	public void getY_test(){
		assertEquals("getY() test zero value", testentrypointZero.getY(), 1, 0.1);
		assertEquals("getY() test positive value", testentrypointPositive.getY(), 101, 0.1);
		assertEquals("getY() test negative", testentrypointNegative.getY(), -101, 0.1);
	}
	
	@Test
	public void setX_test(){
		testentrypointPositive.setX(1000);
		testentrypointNegative.setX(-1000);
		assertEquals("setX(double) test, positive double argument", testentrypointPositive.getX(), 1000, 0.1);
		assertEquals("setX(double) test, negative double argument", testentrypointNegative.getX(), -1000, 0.1);
	}
	
	@Test
	public void setY_test(){
		testentrypointPositive.setY(1001);
		testentrypointNegative.setX(-1001);
		assertEquals("setY(double) test, positive double argument", testentrypointPositive.getY(), 1001, 0.1);
		assertEquals("setY(double) test, negative double argument", testentrypointNegative.getY(), -1001, 0.1);
	}
	
	@Test
	public void getPointRef_test(){
		testentrypointZero.setPointRef("success");
		assertEquals("getPointRef(string) test", testentrypointZero.getPointRef(), "success");
	}
	
	@Test
	public void setPointRef_test(){
		testentrypointZero.setPointRef("success");
		assertEquals("setPointRef(string) test", testentrypointZero.getPointRef(), "success");
	}
	
	@Test
	public void getPointCount_test(){
		assertEquals("getPointCount() test; three created", testentrypointZero.getPointCount(), 3);
	}
}
