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
		airspace.new_waypoint(350, 150);
    	airspace.new_waypoint(400, 470);
    	airspace.new_waypoint(700, 60);
    	airspace.new_waypoint(800, 320);
    	airspace.new_waypoint(600, 418);
    	airspace.new_waypoint(500, 220);
    	airspace.new_waypoint(950, 188);
    	airspace.new_waypoint(1050, 272);
    	airspace.new_waypoint(900, 420);
    	airspace.new_waypoint(240, 250);
    	airspace.new_entry_point(150, 400);
    	airspace.new_entry_point(1200, 200);
    	airspace.new_entry_point(600, 0);
    	airspace.new_exit_point(800, 0);
    	airspace.new_exit_point(150, 200);
    	airspace.new_exit_point(1200, 300);
    	
		flight1 = new Flight(airspace);
		flight2 = new Flight(airspace);
		
		airspace.add_flight(flight1);
		airspace.add_flight(flight2);
		
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
	public void checkViolationTest(){
		flight1.setX(1);
		flight2.setX(1);
		flight1.setY(1);
		flight2.setY(1);
		
		separationRules.checkViolation(airspace);
		assertTrue(separationRules.getGameOverViolation());
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
