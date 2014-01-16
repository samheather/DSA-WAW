package unitTests;

import static org.junit.Assert.*;
import logicClasses.*;

import org.junit.Test;
import org.junit.Before;


// The testable behaviour of Entry, Exit and Way Points, at this point in time, 
// does not differ (graphics) from the testing conducted under Point_Tests. This 
// file is here for updating should future behaviour be given that distinguishes 
// this class from the Point superclass.

public class Waypoint_Tests {
	
	private Waypoint testWaypointZero, testWaypointPositive, testWaypointNegative;
	
	@Before
	public void setUp(){
		testWaypointZero = new Waypoint(0,1);
		testWaypointPositive = new Waypoint(100,101);
		testWaypointNegative = new Waypoint (-100, -101);
	// No constraints applied directly to point constructor; no irregular behaviour should occur
		// from any non-numeric values for x and y.
		
	}
	
	//Testing Mutators & Accessors
	
	// Note for getX_test(), getY_test(), setX_test, setY_test();
	// 0.1 at the end of each test's argument list outlines the tolerance for the 
	// doubles being compared: difference can be as great as 0.1 before test fails. 
	
	@Test
	public void getXTest(){
		assertEquals("getX() test zero value", testWaypointZero.getX(), 0, 0.1);
		assertEquals("getX() test positive value", testWaypointPositive.getX(), 100, 0.1);
		assertEquals("getX() test negative value", testWaypointNegative.getX(), -100, 0.1);
	}
	
	@Test
	public void getYTest(){
		assertEquals("getY() test zero value", testWaypointZero.getY(), 1, 0.1);
		assertEquals("getY() test positive value", testWaypointPositive.getY(), 101, 0.1);
		assertEquals("getY() test negative", testWaypointNegative.getY(), -101, 0.1);
	}
	
	@Test
	public void setXTest(){
		testWaypointPositive.setX(1000);
		testWaypointNegative.setX(-1000);
		assertEquals("setX(double) test, positive double argument", testWaypointPositive.getX(), 1000, 0.1);
		assertEquals("setX(double) test, negative double argument", testWaypointNegative.getX(), -1000, 0.1);
	}
	
	@Test
	public void setYTest(){
		testWaypointPositive.setY(1001);
		testWaypointNegative.setY(-1001);
		assertEquals("setY(double) test, positive double argument", testWaypointPositive.getY(), 1001, 0.1);
		assertEquals("setY(double) test, negative double argument", testWaypointNegative.getY(), -1001, 0.1);
	}
	
	@Test
	public void getPointRefTest(){
		testWaypointZero.setPointRef("success");
		assertEquals("getPointRef(string) test", testWaypointZero.getPointRef(), "success");
	}
	
	@Test
	public void setPointRefTest(){
		testWaypointZero.setPointRef("success");
		assertEquals("setPointRef(string) test", testWaypointZero.getPointRef(), "success");
	}
	
	@Test
	public void getPointCountTest(){
		assertEquals("getPointCount() test; three created", testWaypointZero.getPointCount(), 3);
	}
	

}
