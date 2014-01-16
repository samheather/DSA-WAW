package unitTests;

import static org.junit.Assert.*;
import logicClasses.*;
import org.junit.Test;
import org.junit.Before;

public class Flight_Tests {
	
	private Airspace airspace;
	private  Flight flight1;
	
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
		
	}
	
	// Testing generate_entry_point()
	
	@Test
	public void generateEntryPointTest1(){
		// Testing on airspace with entrypoints
    	EntryPoint result = flight1.generateEntryPoint();
    	assertTrue(result == airspace.getListOfEntryPoints().get(0) || result == airspace.getListOfEntryPoints().get(1) || result == airspace.getListOfEntryPoints().get(2));
		
	}
	

	
	
	// Testing generate_altitude()

	@Test
	public void generateAltitudeTest1() {
    	int result = flight1.generateAltitude();
    	assertTrue(result >=27000 && result<= 30000);
    	
	}
	
	// Testing calculate_heading_to_first_waypoint()
	
	@Test
	public void calculateHeadingToFirstWaypointTest1(){
		flight1.setX(150);
		flight1.setY(400);
		assertEquals(38.65, flight1.calculateHeadingToFirstWaypoint(350,150), 0.01);
		
	}
	
	@Test
	public void calculateHeadingToFirstWaypointTest2(){
		flight1.setX(350);
		flight1.setY(400);
		assertEquals(321.34, flight1.calculateHeadingToFirstWaypoint(150,150), 0.01);
		
	}
	

	
	
	// Testing turn_flight_left(int degrees_turned_by)
	
	
	@Test
	public void turnFlightLeftTest1(){
		flight1.setTargetHeading(10);
		flight1.setCurrentHeading(10);
		flight1.turnFlightLeft(20);
		assertEquals(350, flight1.getTargetHeading(), 0.1);
	}
	
	@Test
	public void turnFlightLeftTest2(){
		flight1.setTargetHeading(270);
		flight1.setCurrentHeading(270);
		flight1.turnFlightLeft(90);
		assertEquals(180, flight1.getTargetHeading(), 0.1);
	}
	
	@Test
	public void turnFlightLeftTest3(){
		flight1.setTargetHeading(90);
		flight1.setCurrentHeading(90);
		flight1.turnFlightLeft(90);
		assertEquals(0, flight1.getTargetHeading(), 0.1);
	}
	
	
	// Testing turn_flight_right(int degrees_turned_by)
	
	@Test
	public void turnFlightRightTest1(){
		flight1.setTargetHeading(0);
		flight1.setCurrentHeading(0);
		flight1.turnFlightRight(30);
		assertEquals(30, flight1.getTargetHeading(), 0.1);
	}
	
	@Test
	public void turnFlightRightTest2(){
		flight1.setTargetHeading(270);
		flight1.setCurrentHeading(270);
		flight1.turnFlightRight(100);
		assertEquals(10, flight1.getTargetHeading(),0.1);
	}
	
	@Test
	public void turnFlightRightTest3(){
		flight1.setTargetHeading(270);
		flight1.setCurrentHeading(270);
		flight1.turnFlightRight(90);
		assertEquals(0, flight1.getTargetHeading(), 0.1);
	}
	

	
	// Testing give_heading(int new_heading)
	
	@Test
	public void giveHeadingTest1(){
		flight1.setTargetHeading(0);
		flight1.setCurrentHeading(0);
		flight1.giveHeading(0);
		assertEquals(0, flight1.getTargetHeading(), 0.1);
	}
	
	@Test
	public void giveHeadingTest2(){
		flight1.setTargetHeading(0);
		flight1.setCurrentHeading(0);
		flight1.giveHeading(360);
		assertEquals(0, flight1.getTargetHeading(), 0.1);
	}
	
	@Test
	public void giveHeadingTest3(){
		flight1.setTargetHeading(0);
		flight1.setCurrentHeading(0);
		flight1.giveHeading(90);
		assertEquals(90, flight1.getTargetHeading(), 0.1);
	}
	
	@Test
	public void giveHeadingTest4(){
		flight1.setTargetHeading(0);
		flight1.setCurrentHeading(0);
		flight1.giveHeading(3610);
		assertEquals(10, flight1.getTargetHeading(), 0.1);
	}
	
	
	// Testing check_if_flight_at_waypoint()
	
	
	@Test
	public void checkIfFlightAtWaypoinTest1(){
		// Exactly 15 away
		Waypoint waypoint = new Waypoint(350, 150);
		flight1.setX(335);
		flight1.setY(135);
		assertTrue(flight1.checkIfFlightAtWaypoint(waypoint));
	}
	
	@Test
	public void checkIfFlightAtWaypointTest2(){
		//Within 15
		Waypoint waypoint = new Waypoint(350, 150);
		flight1.setX(350);
		flight1.setY(150);
		assertTrue(flight1.checkIfFlightAtWaypoint(waypoint));
	}
	
	@Test
	public void checkIfFlightAtWaypointTest3(){
		// Both outside 15
		Waypoint waypoint = new Waypoint(350, 150);
		flight1.setX(1000);
		flight1.setY(1000);
		assertFalse(flight1.checkIfFlightAtWaypoint(waypoint));
	}
	
	@Test
	public void checkIfFlightAtWaypointTest4(){
		// X too far away but Y close enough
		Waypoint waypoint = new Waypoint(350, 150);
		flight1.setX(1000);
		flight1.setY(150);
		assertFalse(flight1.checkIfFlightAtWaypoint(waypoint));
	}
	
	@Test
	public void checkIfFlightAtWaypointTest5(){
		// Y too far away but X close enough
		Waypoint waypoint = new Waypoint(350, 150);
		flight1.setX(350);
		flight1.setY(1000);
		assertFalse(flight1.checkIfFlightAtWaypoint(waypoint));
	}
	
	
	
	// Testing update_x_y_coordinates()
	
	
	
	//Testing update_altitude()
	
	@Test
	public void updateAltitudeTest1(){
		flight1.setAltitude(27000);
		flight1.setTargetAltitude(28000);
		flight1.updateAltitude();
		assertEquals(27001, flight1.getAltitude(), 0.1);
		
	}
	
	@Test
	public void updateAltitudeTest2(){
		flight1.setCurrentAltitude(27000);
		flight1.setTargetAltitude(27000);
		flight1.updateAltitude();
		assertEquals(27000, flight1.getAltitude(), 0.1);
		
	}
	
	@Test
	public void updateAltitudeTest3(){
		flight1.setCurrentAltitude(26999);
		flight1.setTargetAltitude(27000);
		flight1.updateAltitude();
		assertEquals(27000, flight1.getAltitude(), 0.1);
		
	}
	
	@Test
	public void updateAltitudeTest4(){
		flight1.setCurrentAltitude(28000);
		flight1.setTargetAltitude(27000);
		flight1.updateAltitude();
		assertEquals(27999, flight1.getAltitude(), 0.1);
		
	}
	
	@Test
	public void updateAltitudeTest5(){
		flight1.setCurrentAltitude(27001);
		flight1.setTargetAltitude(27000);
		flight1.updateAltitude();
		assertEquals(27000, flight1.getAltitude(), 0.1);
		
	}
	
	
	//Testing update_current_heading()
	
	@Test
	public void updateCurrentHeadingTest1(){
		flight1.setCurrentHeading(288);
		flight1.setTargetHeading(0);
		flight1.updateCurrentHeading();
		assertTrue(flight1.getTurningRight());
		assertFalse(flight1.getTurningLeft());
		assertEquals(288.5, flight1.getCurrentHeading(), 0.1);
		
	}
	
	@Test
	public void updateCurrentHeadingTest2(){
		flight1.setCurrentHeading(288);
		flight1.setTargetHeading(270);
		flight1.updateCurrentHeading();
		assertTrue(flight1.getTurningLeft());
		assertFalse(flight1.getTurningRight());
		assertEquals(287.5, flight1.getCurrentHeading(), 0.1);
		
	}
	
	@Test
	public void updateCurrentHeadingTest3(){
		flight1.setCurrentHeading(270);
		flight1.setTargetHeading(90);
		flight1.updateCurrentHeading();
		assertTrue(flight1.getTurningRight());
		assertFalse(flight1.getTurningLeft());
		assertEquals(270.5, flight1.getCurrentHeading(), 0.1);
		
	}
	
	@Test
	public void updateCurrentHeadingTest4(){
		flight1.setCurrentHeading(288);
		flight1.setTargetHeading(300);
		flight1.updateCurrentHeading();
		assertTrue(flight1.getTurningRight());
		assertFalse(flight1.getTurningLeft());
		assertEquals(288.5, flight1.getCurrentHeading(), 0.1);
		
	}
	
	@Test
	public void updateCurrentHeadingTest5(){
		flight1.setCurrentHeading(150);
		flight1.setTargetHeading(200);
		flight1.updateCurrentHeading();
		assertTrue(flight1.getTurningRight());
		assertFalse(flight1.getTurningLeft());
		assertEquals(150.5, flight1.getCurrentHeading(), 0.1);
		
	}
	
	@Test
	public void updateCurrentHeadingTest6(){
		flight1.setCurrentHeading(20);
		flight1.setTargetHeading(290);
		flight1.updateCurrentHeading();
		assertTrue(flight1.getTurningLeft());
		assertFalse(flight1.getTurningRight());
		assertEquals(19.5, flight1.getCurrentHeading(), 0.1);
		
	}
	
	@Test
	public void updateCurrentHeadingTest7(){
		flight1.setCurrentHeading(359.5);
		flight1.setTargetHeading(10);
		flight1.updateCurrentHeading();
		assertTrue(flight1.getTurningRight());
		assertFalse(flight1.getTurningLeft());
		assertEquals(0, flight1.getCurrentHeading(), 0.1);
		
	}
	
	@Test
	public void updateCurrentHeadingTest8(){
		flight1.setCurrentHeading(0.5);
		flight1.setTargetHeading(290);
		flight1.updateCurrentHeading();
		assertTrue(flight1.getTurningLeft());
		assertFalse(flight1.getTurningRight());
		assertEquals(360, flight1.getCurrentHeading(), 0.1);
		
	}
	
	
	
	
	//Testing update_flight_plan()
	
	
	
	
	// Testing Mutators and Accessors
	
	
	@Test
	public void getXTest1(){
		flight1.setX(0);
		assertEquals(0, flight1.getX(), 0.1);
		
	}
	
	@Test
	public void setXTest1(){
		flight1.setX(0);
		assertEquals(0, flight1.getX(), 0.1);
		
	}
	
	@Test
	public void getYTest1(){
		flight1.setY(0);
		assertEquals(0, flight1.getY(),0.1);
		
	}
	
	@Test
	public void setYTest1(){
		flight1.setY(0);
		assertEquals(0, flight1.getY(), 0.1);
		
	}
	
	@Test
	public void getCurrentHeadingTest1(){
		flight1.setCurrentHeading(0);
		assertEquals(0, flight1.getCurrentHeading(), 0.1);
		
	}
	
	@Test
	public void setCurrentHeadingTest1(){
		flight1.setCurrentHeading(0);
		assertEquals(0, flight1.getCurrentHeading(), 0.1);
		
	}
	
	@Test
	public void getTargetHeadingTest1(){
		flight1.setTargetHeading(0);
		assertEquals(0, flight1.getTargetHeading(), 0.1);
		
	}
	
	@Test
	public void setTargetHeadingTest1(){
		flight1.setTargetHeading(0);
		assertEquals(0, flight1.getTargetHeading(), 0.1);
		
	}
	
	@Test
	public void getTargetAltitudeTest1(){
		flight1.setTargetAltitude(0);
		assertEquals(0, flight1.getTargetAltitude(), 0.1);
		
	}
	
	@Test
	public void setTargetAltitudeTest1(){
		flight1.setTargetAltitude(0);
		assertEquals(0, flight1.getTargetAltitude(), 0.1);
		
	}
	
	@Test
	public void getAltitudeTest1(){
		flight1.setAltitude(0);
		assertEquals(0, flight1.getAltitude(), 0.1);
		
	}
	
	@Test
	public void setAltitudeTest1(){
		flight1.setAltitude(0);
		assertEquals(0, flight1.getAltitude(), 0.1);
		
	}
	
	@Test
	public void isTurningRightTest1(){
		assertEquals(false, flight1.getTurningRight());
	}
	
	@Test
	public void setTurningRightTest1(){
		flight1.setTurningRight(true);
		assertEquals(true, flight1.getTurningRight());
	}
	
	@Test
	public void isTurningLeftTest1(){
		assertEquals(false, flight1.getTurningLeft());
	}
	
	@Test
	public void setTurningLeftTest1(){
		flight1.setTurningLeft(true);
		assertEquals(true, flight1.getTurningLeft());
	}
	
	@Test
	public void getFlightNumTest1(){
		flight1.setFlightNum(1);
		assertEquals(1, flight1.getFlightNum());
	}
	
	@Test
	public void setFlightNumTest1(){
		flight1.setFlightNum(1);
		assertEquals(1, flight1.getFlightNum());
	}
	
	
	
	@Test
	public void getFlightNameTest1(){
		flight1.setFlightName("test");
		assertEquals("test", flight1.getFlightName());
	}
	
	@Test
	public void setFlightNameTest1(){
		flight1.setFlightName("test");
		assertEquals("test", flight1.getFlightName());
	}
	
	@Test
	public void isSelectedTest1(){
		assertEquals(false, flight1.isSelected());
	}
	
	@Test
	public void setSelectedTest1(){
		flight1.setSelected(true);
		assertEquals(true, flight1.isSelected());
	}
	
	@Test
	public void getCurrentAltitudeTest1(){
		flight1.setCurrentAltitude(0);
		assertEquals(0, flight1.getAltitude());
	}
	
	@Test
	public void setCurrentAltitudeTest1(){
		flight1.setCurrentAltitude(0);
		assertEquals(0, flight1.getAltitude());
	}
	
	
	@Test
	public void getFlightPlanTest1(){
		assertTrue(flight1.getFlightPlan() instanceof FlightPlan);
	}
	


}
