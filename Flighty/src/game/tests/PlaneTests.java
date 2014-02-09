
package game.tests;


import org.junit.Test;
import org.junit.Before;
import static org.junit.Assert.*;

import java.util.ArrayList;

import game.struct.Game;
import game.struct.Plane;
import game.struct.Waypoint;


public class PlaneTests {
	
	Game game;
	Plane plane1;
	
	@Before
	public void beforeTests() {
		game = new Game(50, 100);
		plane1 = new Plane("TEST", 500, 3000, 50, game);
		
	}
	
	// Testing Check if flight at waypoint
	
	@Test 
	public void checkIfFlightAtWaypointTest1(){
		assertFalse(plane1.checkIfFlightAtWaypoint(game.getAirport(), game));
	}
	
	@Test 
	public void checkIfFlightAtWaypointTest2(){
		
		plane1.setX(plane1.getFlightPlan().getCurrentRoute().get(0).getX() +15);
		plane1.setY(plane1.getFlightPlan().getCurrentRoute().get(0).getY() +15);
		assertTrue(plane1.checkIfFlightAtWaypoint(plane1.getFlightPlan().getCurrentRoute().get(0), game));
	}
	
	@Test 
	public void checkIfFlightAtWaypointTest3(){
		
		plane1.setX(plane1.getFlightPlan().getCurrentRoute().get(0).getX() +10);
		plane1.setY(plane1.getFlightPlan().getCurrentRoute().get(0).getY() +10);
		assertTrue(plane1.checkIfFlightAtWaypoint(plane1.getFlightPlan().getCurrentRoute().get(0), game));
	}
	
	@Test 
	public void checkIfFlightAtWaypointTest4(){
		
		plane1.setX(plane1.getFlightPlan().getCurrentRoute().get(0).getX() +20);
		plane1.setY(plane1.getFlightPlan().getCurrentRoute().get(0).getY() +20);
		assertFalse(plane1.checkIfFlightAtWaypoint(plane1.getFlightPlan().getCurrentRoute().get(0), game));
	}
	
	
	// Testing increment bearing
	
	@Test 
	public void incrementBearingTest1(){
		plane1.setBearing(0);
		plane1.setTargetBearing(0);
		plane1.incrementBearing();
		assertEquals(1, plane1.getBearing(), 0.1);
		assertEquals (plane1.getBearing(), plane1.getTargetBearing(), 0.1);
		assertTrue(plane1.isTurningRight());
		assertFalse(plane1.isTurningLeft());
		
	}
	
	@Test 
	public void incrementBearingTest2(){
		plane1.setBearing(359);
		plane1.setTargetBearing(359);
		plane1.incrementBearing();
		assertEquals(0, plane1.getBearing(), 0.1);
		assertEquals (plane1.getBearing(), plane1.getTargetBearing(), 0.1);
		assertTrue(plane1.isTurningRight());
		assertFalse(plane1.isTurningLeft());
		
	}
	
	// Testing decrement bearing 
	
	@Test 
	public void decrementBearingTest1(){
		plane1.setBearing(180);
		plane1.setTargetBearing(180);
		plane1.decrementBearing();
		assertEquals(179, plane1.getBearing(), 0.1);
		assertEquals (plane1.getBearing(), plane1.getTargetBearing(), 0.1);
		assertFalse(plane1.isTurningRight());
		assertTrue(plane1.isTurningLeft());
		
	}
	
	@Test 
	public void decrementBearingTest2(){
		plane1.setBearing(0);
		plane1.setTargetBearing(0);
		plane1.decrementBearing();
		assertEquals(359, plane1.getBearing(), 0.1);
		assertEquals (plane1.getBearing(), plane1.getTargetBearing(), 0.1);
		assertFalse(plane1.isTurningRight());
		assertTrue(plane1.isTurningLeft());
		
	}
	
	// Testing Increment Altitude
	
	@Test 
	public void incrementAltitudeTest1(){
		plane1.setAltitude(0);
		plane1.incrementAltitude();
		assertEquals(5, plane1.getAltitude(), 0.1);
	}
	
	// Testing Decrement Altitude
	
	@Test 
	public void decrementAltitudeTest1(){
		plane1.setAltitude(100);
		plane1.decrementAltitude();
		assertEquals(95, plane1.getAltitude(), 0.1);
	}
		
	// Testing Increment Target Altitude
	
	@Test 
	public void incrementTargetAltitudeTest1(){
		plane1.setTargetAltitude(3000);
		plane1.incrementTargetAltitude();
		assertEquals(4000, plane1.getTargetAltitude(), 0.1);
		
	}
	
	
	@Test 
	public void incrementTargetAltitudeTest2(){
		plane1.setTargetAltitude(7000);
		plane1.incrementTargetAltitude();
		assertEquals(7000, plane1.getTargetAltitude(), 0.1);
		
	}
	
	// Testing Decrement Target Altitude
	
	@Test 
	public void decrementTargetAltitudeTest1(){
		plane1.setTargetAltitude(3000);
		plane1.decrementTargetAltitude();
		assertEquals(2000, plane1.getTargetAltitude(), 0.1);
		
	}
	
	@Test 
	public void decrementTargetAltitudeTest2(){
		plane1.setTargetAltitude(2000);
		plane1.decrementTargetAltitude();
		assertEquals(2000, plane1.getTargetAltitude(), 0.1);
		
	}
	
	// Testing Calculating the heading to next Waypoint
	
	@Test
	public void calculateBearingToFirstWaypointTest1(){
		//Testing it calculates the heading to the first waypoint.
		plane1.setX(150);
		plane1.setY(400);
		plane1.setTarget(game.getListOfWaypoints().get(0));
		plane1.calculateBearingToNextWaypoint();
		assertEquals(111.8, plane1.getTargetBearing(), 0.01);
		assertFalse(plane1.isTurningLeft());
		assertFalse(plane1.isTurningRight());
		
	}
	
	
	// Testing Update Current Bearing
	
	@Test
	public void updateCurrentBearingTest1(){
		// Test that the current heading moves towards the target heading
		plane1.setBearing(288);
		plane1.setTargetBearing(0);
		plane1.updateCurrentBearing();
		assertTrue(plane1.isTurningRight());
		assertFalse(plane1.isTurningLeft());
		assertEquals(288.9, plane1.getBearing(), 0.1);
		
	}
	
	@Test
	public void updateCurrentBearingTest2(){
		// Test that the current heading moves towards the target heading
		plane1.setBearing(288);
		plane1.setTargetBearing(270);
		plane1.updateCurrentBearing();
		assertTrue(plane1.isTurningLeft());
		assertFalse(plane1.isTurningRight());
		assertEquals(287.1, plane1.getBearing(), 0.1);
		
	}
	
	@Test
	public void updateCurrentBearingTest3(){
		// Test that the current heading moves towards the target heading
		plane1.setBearing(270);
		plane1.setTargetBearing(90);
		plane1.updateCurrentBearing();
		assertTrue(plane1.isTurningRight());
		assertFalse(plane1.isTurningLeft());
		assertEquals(270.9, plane1.getBearing(), 0.1);
		
	}
	
	@Test
	public void updateCurrentBearingTest4(){
		// Test that the current heading moves towards the target heading
		plane1.setBearing(288);
		plane1.setTargetBearing(300);
		plane1.updateCurrentBearing();
		assertTrue(plane1.isTurningRight());
		assertFalse(plane1.isTurningLeft());
		assertEquals(288.9, plane1.getBearing(), 0.1);
		
	}
	
	@Test
	public void updateCurrentBearingTest5(){
		// Test that the current heading moves towards the target heading
		plane1.setBearing(150);
		plane1.setTargetBearing(200);
		plane1.updateCurrentBearing();
		assertTrue(plane1.isTurningRight());
		assertFalse(plane1.isTurningLeft());
		assertEquals(150.9, plane1.getBearing(), 0.1);
		
	}
	
	@Test
	public void updateCurrentBearingTest6(){
		// Test that the current heading moves towards the target heading
		plane1.setBearing(20);
		plane1.setTargetBearing(290);
		plane1.updateCurrentBearing();
		assertTrue(plane1.isTurningLeft());
		assertFalse(plane1.isTurningRight());
		assertEquals(19.1, plane1.getBearing(), 0.1);
		
	}
	
	@Test
	public void updateCurrentBearingTest7(){
		// Test that the current heading moves towards the target heading
		plane1.setBearing(359.5);
		plane1.setTargetBearing(10);
		plane1.updateCurrentBearing();
		assertTrue(plane1.isTurningRight());
		assertFalse(plane1.isTurningLeft());
		assertEquals(0, plane1.getBearing(), 0.1);
		
	}
	
	@Test
	public void updateCurrentBearingTest8(){
		// Test that the current heading moves towards the target heading
		plane1.setBearing(0.5);
		plane1.setTargetBearing(290);
		plane1.updateCurrentBearing();
		assertTrue(plane1.isTurningLeft());
		assertFalse(plane1.isTurningRight());
		assertEquals(360, plane1.getBearing(), 0.1);
		
	}
	
	// Testing findLandingDescentRate
	
	@Test
	public void findLandingDescentRateTest1(){
		plane1.setX(695);
		plane1.setY(465);
		plane1.setVelocity(8000);
		game.setSpeedDifficulty(0.5);
		plane1.setAltitude(2000);
		assertEquals(45.71, plane1.findLandingDescentRate(), 0.01);
		
	}
	
	// Testing Landing Plane
	
	@Test
	public void landPlane(){
		plane1.setX(695);
		plane1.setY(465);
		plane1.setAltitude(2000);
		plane1.setBearing(170);
		plane1.landPlane();
		assertTrue(game.getAirport().isPlaneLanding());
		assertFalse(plane1.isNeedsToLand());
		assertTrue(plane1.isLanding());
		assertEquals(plane1.getTarget(), plane1.getFlightPlan().getCurrentRoute().get(0));
		assertFalse(game.getManualPlanes().contains(plane1));
		assertEquals(null, game.getCurrentPlane());
		
	}
	
	

	
}


