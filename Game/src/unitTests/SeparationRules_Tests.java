package unitTests;

import static org.junit.Assert.*;
import logicClasses.*;

import org.junit.Test;
import org.junit.Before;

public class SeparationRules_Tests {

	private Airspace airspace;
	private SeparationRules separationRules;
	private Flight flight1;
	private Flight flight2;
	
	@Before
	public void setUp(){
		
		separationRules = new SeparationRules(1);
		
		airspace = new Airspace();
    	airspace.newWaypoint(350, 150, "A");
    	airspace.newWaypoint(400, 470, "B");
    	airspace.newWaypoint(700, 60,  "C");
    	airspace.newWaypoint(800, 320, "D");
    	airspace.newWaypoint(600, 418, "E");
    	airspace.newWaypoint(500, 220, "F");
    	airspace.newWaypoint(950, 188, "G");
    	airspace.newWaypoint(1050, 272,"H");
    	airspace.newWaypoint(900, 420, "I");
    	airspace.newWaypoint(240, 250, "J");
    	airspace.newEntryPoint(150, 400);
    	airspace.newEntryPoint(1200, 200);
    	airspace.newEntryPoint(600, 0);
    	airspace.newExitPoint(800, 0, "1");
    	airspace.newExitPoint(150, 200, "2");
    	airspace.newExitPoint(1200, 300, "3");
    	
		flight1 = new Flight(airspace);
		flight2 = new Flight(airspace);
		
		airspace.addFlight(flight1);
		airspace.addFlight(flight2);
		
		flight1.setX(0);
		flight2.setX(25);
		
		flight1.setY(0);
		flight2.setY(25);
		
		flight1.setAltitude(27000);
		flight2.setAltitude(27000);
		
	}
	
	
	//Test: lateralDistanceBetweenFlights
	@Test
	public void lateralDistanceBetweenFLightsTest() {
		assertTrue(separationRules.lateralDistanceBetweenFlights(flight1, flight2) >= 0);
	}
	
	//Test: verticalDistanceBetweenFlights
	
	@Test
	public void verticalDistanceBetweenFlightsTest(){
		assertTrue(separationRules.verticalDistanceBetweenFlights(flight1, flight2) >= 0);
	}
	
	//Test: checkViolation
	
	@Test
	public void checkViolationTrueTest(){
		
		// Tests that game over is achieved when two flights are too close.
		flight1.setX(1);
		flight2.setX(1);
		flight1.setY(1);
		flight2.setY(1);
		
		separationRules.checkViolation(airspace);
		assertTrue(separationRules.getGameOverViolation());
	}
	
	@Test
	public void checkViolationFalseVerticalTest(){
		// Tests that game over is not achieved when two flights aren't too close.
		flight1.setX(1);
		flight2.setX(1);
		flight1.setY(1000);
		flight2.setY(5000);
		
		separationRules.checkViolation(airspace);
		assertFalse(separationRules.getGameOverViolation());
	}
	
	@Test
	public void checkViolationFalseLateralTest(){
		// Tests that game over is not achieved when two flights aren't too close.
		flight1.setX(1000);
		flight2.setX(1);
		flight1.setY(1000);
		flight2.setY(1000);
		
		separationRules.checkViolation(airspace);
		assertFalse(separationRules.getGameOverViolation());
	}
}
