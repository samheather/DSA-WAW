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
		airspace.newWaypoint(350, 150);
    	airspace.newWaypoint(400, 470);
    	airspace.newWaypoint(700, 60);
    	airspace.newWaypoint(800, 320);
    	airspace.newWaypoint(600, 418);
    	airspace.newWaypoint(500, 220);
    	airspace.newWaypoint(950, 188);
    	airspace.newWaypoint(1050, 272);
    	airspace.newWaypoint(900, 420);
    	airspace.newWaypoint(240, 250);
    	airspace.newEntryPoint(150, 400);
    	airspace.newEntryPoint(1200, 200);
    	airspace.newEntryPoint(600, 0);
    	airspace.newExitPoint(800, 0);
    	airspace.newExitPoint(150, 200);
    	airspace.newExitPoint(1200, 300);
    	
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
		flight1.setX(1);
		flight2.setX(1);
		flight1.setY(1);
		flight2.setY(1);
		
		separationRules.checkViolation(airspace);
		assertTrue(separationRules.getGameOverViolation());
	}
	
	@Test
	public void checkViolationFalseVerticalTest(){
		flight1.setX(1);
		flight2.setX(1);
		flight1.setY(1000);
		flight2.setY(5000);
		
		separationRules.checkViolation(airspace);
		assertFalse(separationRules.getGameOverViolation());
	}
	
	@Test
	public void checkViolationFalseLateralTest(){
		flight1.setX(1000);
		flight2.setX(1);
		flight1.setY(1000);
		flight2.setY(1000);
		
		separationRules.checkViolation(airspace);
		assertFalse(separationRules.getGameOverViolation());
	}
	
	
	
	
	//Test: Mutators and Accessors
	
	@Test
	public void getGameOverLateralSeparationTest(){
		separationRules.setGameOverLateralSeparation(0);
		assertEquals(0, separationRules.getGameOverLateralSeparation());
	}
	
	@Test
	public void setGameOverLateralSeparationTest(){
		separationRules.setGameOverLateralSeparation(0);
		assertEquals(0, separationRules.getGameOverLateralSeparation());
	}
	
	@Test
	public void getGameOverVerticalSeparationTest(){
		separationRules.setGameOverVerticalSeparation(0);
		assertEquals(0, separationRules.getGameOverVerticalSeparation());
	}
	
	@Test
	public void setGameOverVerticalSeparationTest(){
		separationRules.setGameOverVerticalSeparation(0);
		assertEquals(0, separationRules.getGameOverVerticalSeparation());
	}
	
	@Test
	public void getGameOverViolationTest(){
		separationRules.setGameOverViolation(true);
		assertTrue(separationRules.getGameOverViolation());
	}
	
	@Test
	public void setGameOverViolationTest(){
		separationRules.setGameOverViolation(true);
		assertTrue(separationRules.getGameOverViolation());
	}

	@Test
	public void getWarningLateralSeparationTest() {
		assertEquals(100, separationRules.getWarningLateralSeparation());
	}
	
	@Test
	public void getWarningVerticalSeparationTest(){
		assertEquals(1000, separationRules.getWarningVerticalSeparation());
	}
}
