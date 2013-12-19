package unitTests;

import static org.junit.Assert.*;
import logicClasses.*;

import org.junit.Test;
import org.junit.Before;
import org.junit.AfterClass;


// The testable behaviour of Entry, Exit and Way Points, at this point in time, 
// does not differ (graphics) from the testing conducted under Point_Tests. This 
// file is here for updating should future behaviour be given that distinguishes 
// this class from the Point superclass.

public class Waypoint_Tests {
	
	private Waypoint testwaypointZero, testwaypointPositive, testwaypointNegative;
	
	@Before
	public void setUp(){
		testwaypointZero = new Waypoint(0,1);
		testwaypointPositive = new Waypoint(100,101);
		testwaypointNegative = new Waypoint (-100, -101);
	// No constraints applied directly to point constructor; no irregular behaviour should occur
		// from any non-numeric values for x and y.
		
	}
	
	//Testing Mutators & Accessors
	
	// Note for getX_test(), getY_test(), setX_test, setY_test();
	// 0.1 at the end of each test's argument list outlines the tolerance for the 
	// doubles being compared: difference can be as great as 0.1 before test fails. 
	
	@Test
	public void getX_test(){
		assertEquals("getX() test zero value", testwaypointZero.getX(), 0, 0.1);
		assertEquals("getX() test positive value", testwaypointPositive.getX(), 100, 0.1);
		assertEquals("getX() test negative value", testwaypointNegative.getX(), -100, 0.1);
	}
	
	@Test
	public void getY_test(){
		assertEquals("getY() test zero value", testwaypointZero.getY(), 1, 0.1);
		assertEquals("getY() test positive value", testwaypointPositive.getY(), 101, 0.1);
		assertEquals("getY() test negative", testwaypointNegative.getY(), -101, 0.1);
	}
	
	@Test
	public void setX_test(){
		testwaypointPositive.setX(1000);
		testwaypointNegative.setX(-1000);
		assertEquals("setX(double) test, positive double argument", testwaypointPositive.getX(), 1000, 0.1);
		assertEquals("setX(double) test, negative double argument", testwaypointNegative.getX(), -1000, 0.1);
	}
	
	@Test
	public void setY_test(){
		testwaypointPositive.setY(1001);
		testwaypointNegative.setX(-1001);
		assertEquals("setY(double) test, positive double argument", testwaypointPositive.getY(), 1001, 0.1);
		assertEquals("setY(double) test, negative double argument", testwaypointNegative.getY(), -1001, 0.1);
	}
	
	@Test
	public void getPointRef_test(){
		testwaypointZero.setPointRef("success");
		assertEquals("getPointRef(string) test", testwaypointZero.getPointRef(), "success");
	}
	
	@Test
	public void setPointRef_test(){
		testwaypointZero.setPointRef("success");
		assertEquals("setPointRef(string) test", testwaypointZero.getPointRef(), "success");
	}
	
	@Test
	public void getPointCount_test(){
		assertEquals("getPointCount() test; three created", testwaypointZero.getPointCount(), 3);
	}
	

}
