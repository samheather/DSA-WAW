package unitTests;

import static org.junit.Assert.*;
import logicClasses.*;

import org.junit.Test;
import org.junit.Before;
import org.junit.AfterClass;


import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.SlickException;
import org.lwjgl.Sys;
import org.lwjgl.input.Mouse;

//NOT COMPLETED ;;
public class Airspace_Tests {
	
	private Airspace airspace;
	private  Flight flight1;
	
	@Before
	public void setUp(){
    	airspace = new Airspace();
    	// Waypoints
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
    	// EntryPoints
    	airspace.new_entry_point(150, 400);
    	airspace.new_entry_point(1200, 200);
    	airspace.new_entry_point(600, 0);
    	// Exit Points
    	airspace.new_exit_point(800, 0);
    	airspace.new_exit_point(150, 200);
    	airspace.new_exit_point(1200, 300);
    	// Get a Flight
    	flight1 = new Flight(airspace);
		
	}
	
	// Testing new_waypoint()
	@Test
	public void new_waypoint_test(){
		assertTrue(airspace.new_waypoint(150, 500));
		assertFalse(airspace.new_waypoint(-10000, 0));
	}
	
	// Testing new_exit_point()
	@Test
	public void new_exit_point_test(){
		assertTrue(airspace.new_exit_point(150, 500));
		assertFalse(airspace.new_exit_point(-10000, 0));
	}

	// Testing new_entry_point()
	@Test
	public void new_entry_point_test(){
		assertTrue(airspace.new_entry_point(150, 500));
		assertFalse(airspace.new_entry_point(-10000, 0));
	}

	
	// Testing new_flight()
	//@Test	
	//public void new_flight_test(){
		//assertTrue(airspace.new_flight(GameContainer gc));
		//assertFalse(airspace.new_exit_point(-10000, 0));
	//}
	
	// Testing generate_flight_name()
	@Test
	public void generate_flight_name_test(){
		String name = airspace.generate_flight_name();
		assertTrue((name.length() == 6));
	}
	
	// Testing check_if_flight_has_left_airspace()
	@Test
	public void check_if_flight_has_left_airspace_test(){
			//check flight at limits
			flight1.setX(100);
			flight1.setY(-50);
			assertFalse(airspace.check_if_flight_has_left_airspace(flight1));
			
			flight1.setX(1250);
			flight1.setY(650);
			assertFalse(airspace.check_if_flight_has_left_airspace(flight1));
			
			//Some other sets of tests
			flight1.setX(1251);
			flight1.setY(5);
			assertTrue(airspace.check_if_flight_has_left_airspace(flight1));

			flight1.setX(101);
			flight1.setY(0);
			assertFalse(airspace.check_if_flight_has_left_airspace(flight1));
			
			flight1.setX(-143401);
			flight1.setY(101010);
			assertTrue(airspace.check_if_flight_has_left_airspace(flight1));
	}
	
	// Testing check_selected()
//		@Test
//		public void check_selected_test(){
//			
//			
//		}
	// Testing changeScore()
	@Test
	public void changeScore_test() {
		//Testing initialization of score
		int score = 10;
		int oldscore;
		airspace.changeScore(score);
		oldscore = airspace.getScore();
		assertTrue(airspace.getScore() == score);
		
		//Testing changing of score
		score += 10;
		airspace.changeScore(score);
		assertTrue(airspace.getScore() == oldscore + score);
	}
}



