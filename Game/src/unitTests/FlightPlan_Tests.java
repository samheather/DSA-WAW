package unitTests;

import static org.junit.Assert.*;

import java.util.ArrayList;

import logicClasses.*;

import org.junit.Test;
import org.junit.Before;

public class FlightPlan_Tests {
	
	private Airspace airspace;
	private Flight flight1;
	private FlightPlan flightplan;

	@Before
	public void setUp(){
    	airspace = new Airspace();
    	//Waypoints
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
    	//EntryPoints
    	airspace.newEntryPoint(150, 400);
    	airspace.newEntryPoint(1200, 200);
    	airspace.newEntryPoint(600, 0);
    	// Exit Points
    	airspace.newExitPoint(800, 0);
    	airspace.newExitPoint(150, 200);
    	airspace.newExitPoint(1200, 300);
    	flight1 = new Flight(airspace);
    	flightplan = new FlightPlan(airspace, flight1);
    	
		
	}
	
	@Test
	public void generateVelocityTest(){
		for (int i = 0; i < 100; i++){
			double velocity = flightplan.generateVelocity();
			assertTrue(velocity < 400 && velocity >= 200);
			
		}
		
	}
	
	@Test
	public void buildRouteTest1(){
		
		// Testing Length of Route
		for (int i = 0; i < 100; i++){
			ArrayList<Point> route = flightplan.buildRoute(airspace, flight1.getEntryPoint());
			assertTrue(route.size() >= 2 && route.size() <= 5);
		}
	}
	
	@Test 
	public void buildRouteTest2(){
		
		//Testing that it doesn't repeat waypoints
		for (int i = 0; i < 100; i++){
			ArrayList<Point> route = flightplan.buildRoute(airspace, flight1.getEntryPoint());
			boolean samePoint = false;
			
			
			for (int j = 0; j < route.size(); j++) {
				  for (int k = j+1; k < route.size(); k++) {
				    if (route.get(j).equals(route.get(k))){
				    	samePoint = true;
				    }
				  }
			}
			assertFalse(samePoint);
		}
		
		
	}
	
	@Test
	public void buildRouteTest3(){
		
		// Testing that it doesn't build a route if no airspace has no waypoints
		
		Airspace airspaceMissingExitPoints = new Airspace();
		
		//EntryPoints
    	airspaceMissingExitPoints.newEntryPoint(150, 400);
    	airspaceMissingExitPoints.newEntryPoint(1200, 200);
    	airspaceMissingExitPoints.newEntryPoint(600, 0);
    	//Waypoints
    	airspaceMissingExitPoints.newWaypoint(350, 150);
    	airspaceMissingExitPoints.newWaypoint(400, 470);
    	airspaceMissingExitPoints.newWaypoint(700, 60);
    	airspaceMissingExitPoints.newWaypoint(800, 320);
    	airspaceMissingExitPoints.newWaypoint(600, 418);
    	airspaceMissingExitPoints.newWaypoint(500, 220);
    	airspaceMissingExitPoints.newWaypoint(950, 188);
    	airspaceMissingExitPoints.newWaypoint(1050, 272);
    	airspaceMissingExitPoints.newWaypoint(900, 420);
    	airspaceMissingExitPoints.newWaypoint(240, 250);
    	
    	Flight flight1 = new Flight(airspaceMissingExitPoints);
    	
    	ArrayList<Point> route = flightplan.buildRoute(airspaceMissingExitPoints, flight1.getEntryPoint());
    	assertTrue(route.size() == 0);
		
	}
	
	@Test
	public void buildRouteTest4(){
		
		// Testing that it doesn't build a route if no airspace has no exitpoints
		
		Airspace airspaceMissingWaypoints = new Airspace();
		
		//EntryPoints
    	airspaceMissingWaypoints.newEntryPoint(150, 400);
    	airspaceMissingWaypoints.newEntryPoint(1200, 200);
    	airspaceMissingWaypoints.newEntryPoint(600, 0);
    	// Exit Points
    	airspaceMissingWaypoints.newExitPoint(800, 0);
    	airspaceMissingWaypoints.newExitPoint(150, 200);
    	airspaceMissingWaypoints.newExitPoint(1200, 300);
    	
    	Flight flight1 = new Flight(airspaceMissingWaypoints);
    	
    	ArrayList<Point> route = flightplan.buildRoute(airspaceMissingWaypoints, flight1.getEntryPoint());
    	assertTrue(route.size() == 0);
		
	}
	
	@Test 
	public void setVelocityTest(){
		flightplan.setVelocity(350);
		assertEquals(350, flightplan.getVelocity(),3);
	}
	
	
	
	
	
	
	

	
	

}
