package unitTests;

import static org.junit.Assert.*;

import java.util.ArrayList;

import logicClasses.*;

import org.junit.Test;
import org.junit.Before;
import org.junit.AfterClass;

public class FlightPlan_Tests {
	
	private Airspace airspace;
	private Flight flight1;
	private FlightPlan flightplan;

	@Before
	public void setUp(){
    	airspace = new Airspace();
    	//Waypoints
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
    	//EntryPoints
    	airspace.new_entry_point(150, 400);
    	airspace.new_entry_point(1200, 200);
    	airspace.new_entry_point(600, 0);
    	// Exit Points
    	airspace.new_exit_point(800, 0);
    	airspace.new_exit_point(150, 200);
    	airspace.new_exit_point(1200, 300);
    	flight1 = new Flight(airspace);
    	flightplan = new FlightPlan(airspace, flight1);
    	
		
	}
	
	@Test
	public void generate_velocity_test(){
		for (int i = 0; i < 100; i++){
			double velocity = flightplan.generate_velocity();
			assertTrue(velocity < 400 && velocity >= 200);
			
		}
		
	}
	
	@Test
	public void build_route_test1(){
		
		// Testing Length of Route
		for (int i = 0; i < 100; i++){
			ArrayList<Point> route = flightplan.build_route(airspace, flight1.getEntryPoint());
			assertTrue(route.size() >= 2 && route.size() <= 5);
		}
	}
	
	@Test 
	public void build_route_test2(){
		
		//Testing that it doesn't repeat waypoints
		for (int i = 0; i < 100; i++){
			ArrayList<Point> route = flightplan.build_route(airspace, flight1.getEntryPoint());
			boolean same_point = false;
			
			
			for (int j = 0; j < route.size(); j++) {
				  for (int k = j+1; k < route.size(); k++) {
				    if (route.get(j).equals(route.get(k))){
				    	same_point = true;
				    }
				  }
			}
			assertFalse(same_point);
		}
		
		
	}
	
	@Test
	public void build_route_test3(){
		
		// Testing that it doesn't build a route if no airspace has no waypoints
		
		Airspace airspace_missing_exitpoints = new Airspace();
		
		//EntryPoints
    	airspace_missing_exitpoints.new_entry_point(150, 400);
    	airspace_missing_exitpoints.new_entry_point(1200, 200);
    	airspace_missing_exitpoints.new_entry_point(600, 0);
    	//Waypoints
    	airspace_missing_exitpoints.new_waypoint(350, 150);
    	airspace_missing_exitpoints.new_waypoint(400, 470);
    	airspace_missing_exitpoints.new_waypoint(700, 60);
    	airspace_missing_exitpoints.new_waypoint(800, 320);
    	airspace_missing_exitpoints.new_waypoint(600, 418);
    	airspace_missing_exitpoints.new_waypoint(500, 220);
    	airspace_missing_exitpoints.new_waypoint(950, 188);
    	airspace_missing_exitpoints.new_waypoint(1050, 272);
    	airspace_missing_exitpoints.new_waypoint(900, 420);
    	airspace_missing_exitpoints.new_waypoint(240, 250);
    	
    	Flight flight1 = new Flight(airspace_missing_exitpoints);
    	
    	ArrayList<Point> route = flightplan.build_route(airspace_missing_exitpoints, flight1.getEntryPoint());
    	assertTrue(route.size() == 0);
		
	}
	
	@Test
	public void build_route_test4(){
		
		// Testing that it doesn't build a route if no airspace has no exitpoints
		
		Airspace airspace_missing_waypoints = new Airspace();
		
		//EntryPoints
    	airspace_missing_waypoints.new_entry_point(150, 400);
    	airspace_missing_waypoints.new_entry_point(1200, 200);
    	airspace_missing_waypoints.new_entry_point(600, 0);
    	// Exit Points
    	airspace_missing_waypoints.new_exit_point(800, 0);
    	airspace_missing_waypoints.new_exit_point(150, 200);
    	airspace_missing_waypoints.new_exit_point(1200, 300);
    	
    	Flight flight1 = new Flight(airspace_missing_waypoints);
    	
    	ArrayList<Point> route = flightplan.build_route(airspace_missing_waypoints, flight1.getEntryPoint());
    	assertTrue(route.size() == 0);
		
	}
	
	@Test 
	public void setVelocity_test(){
		flightplan.setVelocity(350);
		assertEquals(350, flightplan.getVelocity(),3);
	}
	
	
	
	
	
	
	

	
	

}
