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
    	//EntryPoints
    	airspace.newEntryPoint(150, 400);
    	airspace.newEntryPoint(1200, 200);
    	airspace.newEntryPoint(600, 0);
    	// Exit Points
    	airspace.newExitPoint(800, 0, "1");
    	airspace.newExitPoint(150, 200, "2");
    	airspace.newExitPoint(1200, 300, "3");
    	flight1 = new Flight(airspace);
    	flightplan = new FlightPlan(airspace, flight1);
    	
		
	}
	
	// Testing generate_entry_point()
	
	@Test
	public void generateEntryPointTest1(){
		// Testing on airspace with entrypoints
    	EntryPoint result = flight1.getFlightPlan().generateEntryPoint(airspace);
    	assertTrue(result == airspace.getListOfEntryPoints().get(0) || result == airspace.getListOfEntryPoints().get(1) || result == airspace.getListOfEntryPoints().get(2));
		
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
			ArrayList<Point> route = flightplan.buildRoute(airspace, flight1.getFlightPlan().getEntryPoint());
			assertTrue(route.size() >= 2 && route.size() <= 5);
		}
	}
	
	@Test 
	public void buildRouteTest2(){
		
		//Testing that it doesn't repeat waypoints
		for (int i = 0; i < 100; i++){
			ArrayList<Point> route = flightplan.buildRoute(airspace, flight1.getFlightPlan().getEntryPoint());
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
    	airspaceMissingExitPoints.newWaypoint(350, 150, "A");
    	airspaceMissingExitPoints.newWaypoint(400, 470, "B");
    	airspaceMissingExitPoints.newWaypoint(700, 60,  "C");
    	airspaceMissingExitPoints.newWaypoint(800, 320, "D");
    	airspaceMissingExitPoints.newWaypoint(600, 418, "E");
    	airspaceMissingExitPoints.newWaypoint(500, 220, "F");
    	airspaceMissingExitPoints.newWaypoint(950, 188, "G");
    	airspaceMissingExitPoints.newWaypoint(1050, 272,"H");
    	airspaceMissingExitPoints.newWaypoint(900, 420, "I");
    	airspaceMissingExitPoints.newWaypoint(240, 250, "J");
    	
    	Flight flight1 = new Flight(airspaceMissingExitPoints);
    	
    	ArrayList<Point> route = flightplan.buildRoute(airspaceMissingExitPoints, flight1.getFlightPlan().getEntryPoint());
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
    	airspaceMissingWaypoints.newExitPoint(800, 0, "1");
    	airspaceMissingWaypoints.newExitPoint(150, 200, "2");
    	airspaceMissingWaypoints.newExitPoint(1200, 300, "3");
    	
    	Flight flight1 = new Flight(airspaceMissingWaypoints);
    	
    	ArrayList<Point> route = flightplan.buildRoute(airspaceMissingWaypoints, flight1.getFlightPlan().getEntryPoint());
    	assertTrue(route.size() == 0);
		
	}
	
	@Test
	public void updateFlightPlanTest(){
		int previousSize = flight1.getFlightPlan().getCurrentRoute().size();
		flight1.setX(flight1.getFlightPlan().getCurrentRoute().get(0).getX());
		flight1.setY(flight1.getFlightPlan().getCurrentRoute().get(0).getY());
		flight1.getFlightPlan().updateFlightPlan();
		assertEquals(previousSize - 1, flight1.getFlightPlan().getCurrentRoute().size(), 0 );
		
		
	}

	
	
	
	
	
	
	

	
	

}
