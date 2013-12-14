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
	
	// Testing generate_entry_point()
	
	@Test
	public void generate_entry_point_test1(){
    	EntryPoint result = flight1.generate_entry_point();
    	assertTrue(result == airspace.getList_of_entry_points().get(0) || result == airspace.getList_of_entry_points().get(1) || result == airspace.getList_of_entry_points().get(2));
		
	}
	
	// Testing generate_altitude()

	@Test
	public void generate_altitude_test1() {
    	int result = flight1.generate_altitude();
    	assertTrue(result >=27000 && result<= 30000);
    	
	}
	
	// Testing calculate_heading_to_first_waypoint()
	
	@Test
	public void calculate_heading_to_first_waypoint_test1(){
		flight1.setX(150);
		flight1.setY(400);
		assertEquals(38.65, flight1.calculate_heading_to_first_waypoint(350,150), 4);
		
	}
	

	
	
	// Testing turn_flight_left(int degrees_turned_by)
	
	
	@Test
	public void turn_flight_left_test1(){
		flight1.setTarget_heading(10);
		flight1.setCurrent_heading(10);
		flight1.turn_flight_left(20);
		assertEquals(350, flight1.getTarget_heading(), 3);
	}
	
	@Test
	public void turn_flight_left_test2(){
		flight1.setTarget_heading(270);
		flight1.setCurrent_heading(270);
		flight1.turn_flight_left(90);
		assertEquals(180, flight1.getTarget_heading(), 3);
	}
	
	@Test
	public void turn_flight_left_test3(){
		flight1.setTarget_heading(90);
		flight1.setCurrent_heading(90);
		flight1.turn_flight_left(90);
		assertEquals(0, flight1.getTarget_heading(), 3);
	}
	
	
	// Testing turn_flight_right(int degrees_turned_by)
	
	@Test
	public void turn_flight_right_test1(){
		flight1.setTarget_heading(0);
		flight1.setCurrent_heading(0);
		flight1.turn_flight_right(30);
		assertEquals(30, flight1.getTarget_heading(), 3);
	}
	
	@Test
	public void turn_flight_right_test2(){
		flight1.setTarget_heading(270);
		flight1.setCurrent_heading(270);
		flight1.turn_flight_right(100);
		assertEquals(10, flight1.getTarget_heading(), 3);
	}
	
	@Test
	public void turn_flight_right_test3(){
		flight1.setTarget_heading(270);
		flight1.setCurrent_heading(270);
		flight1.turn_flight_right(90);
		assertEquals(0, flight1.getTarget_heading(), 3);
	}
	

	
	// Testing give_heading(int new_heading)
	
	@Test
	public void give_heading_test1(){
		flight1.setTarget_heading(0);
		flight1.setCurrent_heading(0);
		flight1.give_heading(0);
		assertEquals(0, flight1.getTarget_heading(), 3);
	}
	
	@Test
	public void give_heading_test2(){
		flight1.setTarget_heading(0);
		flight1.setCurrent_heading(0);
		flight1.give_heading(360);
		assertEquals(0, flight1.getTarget_heading(), 3);
	}
	
	@Test
	public void give_heading_test3(){
		flight1.setTarget_heading(0);
		flight1.setCurrent_heading(0);
		flight1.give_heading(90);
		assertEquals(90, flight1.getTarget_heading(), 3);
	}
	
	@Test
	public void give_heading_test4(){
		flight1.setTarget_heading(0);
		flight1.setCurrent_heading(0);
		flight1.give_heading(3610);
		assertEquals(10, flight1.getTarget_heading(), 3);
	}
	
	
	// Testing check_if_flight_at_waypoint()
	
	
	@Test
	public void check_if_flight_at_waypoint_test1(){
		// Exactly 15 away
		Waypoint waypoint = new Waypoint(350, 150);
		flight1.setX(335);
		flight1.setY(135);
		assertTrue(flight1.check_if_flight_at_waypoint(waypoint));
	}
	
	@Test
	public void check_if_flight_at_waypoint_test2(){
		//Within 15
		Waypoint waypoint = new Waypoint(350, 150);
		flight1.setX(350);
		flight1.setY(150);
		assertTrue(flight1.check_if_flight_at_waypoint(waypoint));
	}
	
	@Test
	public void check_if_flight_at_waypoint_test3(){
		// Both outside 15
		Waypoint waypoint = new Waypoint(350, 150);
		flight1.setX(1000);
		flight1.setY(1000);
		assertFalse(flight1.check_if_flight_at_waypoint(waypoint));
	}
	
	@Test
	public void check_if_flight_at_waypoint_test4(){
		// X too far away but Y close enough
		Waypoint waypoint = new Waypoint(350, 150);
		flight1.setX(1000);
		flight1.setY(150);
		assertFalse(flight1.check_if_flight_at_waypoint(waypoint));
	}
	
	@Test
	public void check_if_flight_at_waypoint_test5(){
		// Y too far away but X close enough
		Waypoint waypoint = new Waypoint(350, 150);
		flight1.setX(350);
		flight1.setY(1000);
		assertFalse(flight1.check_if_flight_at_waypoint(waypoint));
	}
	
	
	
	// Testing update_x_y_coordinates()
	
	
	
	//Testing update_altitude()
	
	@Test
	public void update_altitude_test1(){
		flight1.setCurrent_altitude(27000);
		flight1.setTarget_altitude(28000);
		flight1.update_altitude();
		assertEquals(27001, flight1.getCurrent_altitude(), 3);
		
	}
	
	@Test
	public void update_altitude_test2(){
		flight1.setCurrent_altitude(27000);
		flight1.setTarget_altitude(27000);
		flight1.update_altitude();
		assertEquals(27000, flight1.getCurrent_altitude(), 3);
		
	}
	
	@Test
	public void update_altitude_test3(){
		flight1.setCurrent_altitude(26999);
		flight1.setTarget_altitude(27000);
		flight1.update_altitude();
		assertEquals(27000, flight1.getCurrent_altitude(), 3);
		
	}
	
	@Test
	public void update_altitude_test4(){
		flight1.setCurrent_altitude(28000);
		flight1.setTarget_altitude(27000);
		flight1.update_altitude();
		assertEquals(27999, flight1.getCurrent_altitude(), 3);
		
	}
	
	@Test
	public void update_altitude_test5(){
		flight1.setCurrent_altitude(27001);
		flight1.setTarget_altitude(27000);
		flight1.update_altitude();
		assertEquals(27000, flight1.getCurrent_altitude(), 3);
		
	}
	
	
	//Testing update_current_heading()
	
	@Test
	public void update_current_heading_test1(){
		flight1.setCurrent_heading(288);
		flight1.setTarget_heading(0);
		flight1.update_current_heading();
		assertEquals(288.4, flight1.getCurrent_heading(), 5);
		
	}
	
	@Test
	public void update_current_heading_test2(){
		flight1.setCurrent_heading(288);
		flight1.setTarget_heading(270);
		flight1.update_current_heading();
		assertEquals(287.6, flight1.getCurrent_heading(), 5);
		
	}
	
	@Test
	public void update_current_heading_test3(){
		flight1.setCurrent_heading(270);
		flight1.setTarget_heading(90);
		flight1.update_current_heading();
		assertEquals(270.4, flight1.getCurrent_heading(), 5);
		
	}
	
	
	
	
	//Testing update_flight_plan()
	
	
	
	
	// Testing Mutators and Accessors
	
	
	@Test
	public void getX_test1(){
		flight1.setX(0);
		assertEquals(0, flight1.getX(), 3);
		
	}
	
	@Test
	public void setX_test1(){
		flight1.setX(0);
		assertEquals(0, flight1.getX(), 3);
		
	}
	
	@Test
	public void getY_test1(){
		flight1.setY(0);
		assertEquals(0, flight1.getY(), 3);
		
	}
	
	@Test
	public void setY_test1(){
		flight1.setY(0);
		assertEquals(0, flight1.getY(), 3);
		
	}
	
	@Test
	public void getCurrent_heading_test1(){
		flight1.setCurrent_heading(0);
		assertEquals(0, flight1.getCurrent_heading(), 3);
		
	}
	
	@Test
	public void setCurrent_heading_test1(){
		flight1.setCurrent_heading(0);
		assertEquals(0, flight1.getCurrent_heading(), 3);
		
	}
	
	@Test
	public void getTarget_heading_test1(){
		flight1.setTarget_heading(0);
		assertEquals(0, flight1.getTarget_heading(), 3);
		
	}
	
	@Test
	public void setTarget_heading_test1(){
		flight1.setTarget_heading(0);
		assertEquals(0, flight1.getTarget_heading(), 3);
		
	}
	
	@Test
	public void getTarget_altitude_test1(){
		flight1.setTarget_altitude(0);
		assertEquals(0, flight1.getTarget_altitude(), 3);
		
	}
	
	@Test
	public void setTarget_altitude_test1(){
		flight1.setTarget_altitude(0);
		assertEquals(0, flight1.getTarget_altitude(), 3);
		
	}
	
	@Test
	public void getAltitude_test1(){
		flight1.setAltitude(0);
		assertEquals(0, flight1.getAltitude(), 3);
		
	}
	
	@Test
	public void setAltitude_test1(){
		flight1.setAltitude(0);
		assertEquals(0, flight1.getAltitude(), 3);
		
	}
	
	@Test
	public void isTurning_right_test1(){
		assertEquals(false, flight1.isTurning_right());
	}
	
	@Test
	public void setTurning_right_test1(){
		flight1.setTurning_right(true);
		assertEquals(true, flight1.isTurning_right());
	}
	
	@Test
	public void isTurning_left_test1(){
		assertEquals(false, flight1.isTurning_left());
	}
	
	@Test
	public void setTurning_left_test1(){
		flight1.setTurning_left(true);
		assertEquals(true, flight1.isTurning_left());
	}
	
	@Test
	public void getFlight_num_test1(){
		flight1.setFlight_num(1);
		assertEquals(1, flight1.getFlight_num());
	}
	
	@Test
	public void setFlight_num_test1(){
		flight1.setFlight_num(1);
		assertEquals(1, flight1.getFlight_num());
	}
	
	
	
	@Test
	public void getFlight_name_test1(){
		flight1.setFlight_name("test");
		assertEquals("test", flight1.getFlight_name());
	}
	
	@Test
	public void setFlight_name_test1(){
		flight1.setFlight_name("test");
		assertEquals("test", flight1.getFlight_name());
	}
	
	@Test
	public void isSelected_test1(){
		assertEquals(false, flight1.isSelected());
	}
	
	@Test
	public void setSelected_test1(){
		flight1.setSelected(true);
		assertEquals(true, flight1.isSelected());
	}
	
	@Test
	public void getCurrent_altitude_test1(){
		flight1.setCurrent_altitude(0);
		assertEquals(0, flight1.getCurrent_altitude());
	}
	
	@Test
	public void setCurrent_altitude_test1(){
		flight1.setCurrent_altitude(0);
		assertEquals(0, flight1.getCurrent_altitude());
	}
	
	
	@Test
	public void getFlight_Plan_test1(){
		assertTrue(flight1.getFlight_plan() instanceof FlightPlan);
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

}
