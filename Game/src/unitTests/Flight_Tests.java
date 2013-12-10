package unitTests;

import static org.junit.Assert.*;
import logicClasses.*;

import org.junit.Test;
import org.junit.Before;
import org.junit.AfterClass;

public class Flight_Tests {
	
	private Airspace airspace;
	private  Flight flight1;
	
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
		
	}

	@Test
	public void generate_altitude_test() {
    	int result = flight1.generate_altitude();
    	assertTrue(result >=28000 && result<= 30000);
    	
	}
	
	@Test
	public void generate_entry_point_test(){
    	EntryPoint result = flight1.generate_entry_point();
    	assertTrue(result == airspace.getList_of_entry_points().get(1) || result == airspace.getList_of_entry_points().get(2) || result == airspace.getList_of_entry_points().get(1));
		
	}
	
	
	
	

}
