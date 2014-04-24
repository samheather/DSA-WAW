package game.tests;

import org.junit.Test;
import org.junit.Before;

import static org.junit.Assert.*;

import java.io.IOException;
import java.net.UnknownHostException;
import java.security.NoSuchAlgorithmException;

import game.struct.Game;
import game.struct.AbstractPlane;

public class PlaneTests {

	Game game;
	AbstractPlane plane1;

	@Before
	public void beforeTests() throws NoSuchAlgorithmException, UnknownHostException, IOException {
		game = new Game(50, 100, 0, false);
		plane1 = new AbstractPlane(1, 500, 3000, 50, game, 0);

	}

	// Testing Check if flight at waypoint.

	/**
	 * Test ID P.1.1
	 */

	@Test
	public void checkIfFlightAtWaypointTest1() {
		plane1.setX(10000);
		plane1.setY(10000);
		assertFalse(plane1.checkIfFlightAtWaypoint(game.getAirport(), game));
	}

	/**
	 * Test ID P.1.2
	 */

	@Test
	public void checkIfFlightAtWaypointTest2() {

		plane1.setX(plane1.getFlightPlan().getCurrentRoute().get(0).getX() + 15);
		plane1.setY(plane1.getFlightPlan().getCurrentRoute().get(0).getY() + 15);
		assertTrue(plane1.checkIfFlightAtWaypoint(plane1.getFlightPlan()
				.getCurrentRoute().get(0), game));
	}

	/**
	 * Test ID P.1.3
	 */

	@Test
	public void checkIfFlightAtWaypointTest3() {

		plane1.setX(plane1.getFlightPlan().getCurrentRoute().get(0).getX() + 10);
		plane1.setY(plane1.getFlightPlan().getCurrentRoute().get(0).getY() + 10);
		assertTrue(plane1.checkIfFlightAtWaypoint(plane1.getFlightPlan()
				.getCurrentRoute().get(0), game));
	}

	/**
	 * Test ID P.1.4
	 */

	@Test
	public void checkIfFlightAtWaypointTest4() {

		plane1.setX(plane1.getFlightPlan().getCurrentRoute().get(0).getX() + 20);
		plane1.setY(plane1.getFlightPlan().getCurrentRoute().get(0).getY() + 20);
		assertFalse(plane1.checkIfFlightAtWaypoint(plane1.getFlightPlan()
				.getCurrentRoute().get(0), game));
	}

	/**
	 * Test ID P.1.5
	 */

	@Test
	public void checkIfFlightAtWaypointTest5() {

		plane1.setX(plane1.getFlightPlan().getCurrentRoute().get(0).getX() + 10);
		plane1.setY(plane1.getFlightPlan().getCurrentRoute().get(0).getY() + 10);
		plane1.setLanding(false);
		assertFalse(plane1.checkIfFlightAtWaypoint(game.getAirport()
				.getBeginningOfRunway(), game));
	}

	// Testing increment bearing

	/**
	 * Test ID P.2.1
	 */

	@Test
	public void incrementBearingTest1() {
		plane1.setBearing(0);
		plane1.setTargetBearing(0);
		plane1.incrementBearing();
		assertEquals(1, plane1.getBearing(), 0.1);
		assertEquals(plane1.getBearing(), plane1.getTargetBearing(), 0.1);
		assertTrue(plane1.isTurningRight());
		assertFalse(plane1.isTurningLeft());

	}

	/**
	 * Test ID P.2.2
	 */

	@Test
	public void incrementBearingTest2() {
		plane1.setBearing(359);
		plane1.setTargetBearing(359);
		plane1.incrementBearing();
		assertEquals(0, plane1.getBearing(), 0.1);
		assertEquals(plane1.getBearing(), plane1.getTargetBearing(), 0.1);
		assertTrue(plane1.isTurningRight());
		assertFalse(plane1.isTurningLeft());

	}

	// Testing decrement bearing

	/**
	 * Test ID P.3.1
	 */

	@Test
	public void decrementBearingTest1() {
		plane1.setBearing(180);
		plane1.setTargetBearing(180);
		plane1.decrementBearing();
		assertEquals(179, plane1.getBearing(), 0.1);
		assertEquals(plane1.getBearing(), plane1.getTargetBearing(), 0.1);
		assertFalse(plane1.isTurningRight());
		assertTrue(plane1.isTurningLeft());

	}

	/**
	 * Test ID P.3.2
	 */

	@Test
	public void decrementBearingTest2() {
		plane1.setBearing(0);
		plane1.setTargetBearing(0);
		plane1.decrementBearing();
		assertEquals(359, plane1.getBearing(), 0.1);
		assertEquals(plane1.getBearing(), plane1.getTargetBearing(), 0.1);
		assertFalse(plane1.isTurningRight());
		assertTrue(plane1.isTurningLeft());

	}

	/**
	 * Test ID P.4
	 */

	// Testing Increment Altitude

	@Test
	public void incrementAltitudeTest1() {
		plane1.setAltitude(0);
		plane1.incrementAltitude();
		assertEquals(5, plane1.getAltitude(), 0.1);
	}

	// Testing Decrement Altitude

	/**
	 * Test ID P.5
	 */

	@Test
	public void decrementAltitudeTest1() {
		plane1.setAltitude(100);
		plane1.decrementAltitude();
		assertEquals(95, plane1.getAltitude(), 0.1);
	}

	// Testing Increment Target Altitude

	/**
	 * Test ID P.6.1
	 */

	@Test
	public void incrementTargetAltitudeTest1() {
		plane1.setTargetAltitude(3000);
		plane1.incrementTargetAltitude();
		assertEquals(4000, plane1.getTargetAltitude(), 0.1);

	}

	/**
	 * Test ID P.6.2
	 */

	@Test
	public void incrementTargetAltitudeTest2() {
		plane1.setTargetAltitude(7000);
		plane1.incrementTargetAltitude();
		assertEquals(7000, plane1.getTargetAltitude(), 0.1);

	}

	// Testing Decrement Target Altitude

	/**
	 * Test ID P.7.1
	 */

	@Test
	public void decrementTargetAltitudeTest1() {
		plane1.setTargetAltitude(3000);
		plane1.decrementTargetAltitude();
		assertEquals(2000, plane1.getTargetAltitude(), 0.1);

	}

	/**
	 * Test ID P.7.2
	 */

	@Test
	public void decrementTargetAltitudeTest2() {
		plane1.setTargetAltitude(2000);
		plane1.decrementTargetAltitude();
		assertEquals(2000, plane1.getTargetAltitude(), 0.1);

	}

	// Testing Calculating the heading to next Waypoint

	/**
	 * Test ID P.8
	 */

	@Test
	public void calculateBearingToNextWaypointTest1() {
		// Testing it calculates the heading to the first waypoint.
		plane1.setX(150);
		plane1.setY(400);
		plane1.setTarget(game.getListOfWaypoints().get(0));
		plane1.calculateBearingToNextWaypoint();
		assertEquals(111.8, plane1.getTargetBearing(), 0.01);
		assertFalse(plane1.isTurningLeft());
		assertFalse(plane1.isTurningRight());

	}

	// Testing Update Current Bearing

	/**
	 * Test ID P.9.1
	 */

	@Test
	public void updateCurrentBearingTest1() {
		// Test that the current heading moves towards the target heading
		plane1.setBearing(288);
		plane1.setTargetBearing(0);
		plane1.updateCurrentBearing();
		assertTrue(plane1.isTurningRight());
		assertFalse(plane1.isTurningLeft());
		assertEquals(288.9, plane1.getBearing(), 0.1);

	}

	/**
	 * Test ID P.9.2
	 */

	@Test
	public void updateCurrentBearingTest2() {
		// Test that the current heading moves towards the target heading
		plane1.setBearing(288);
		plane1.setTargetBearing(270);
		plane1.updateCurrentBearing();
		assertTrue(plane1.isTurningLeft());
		assertFalse(plane1.isTurningRight());
		assertEquals(287.1, plane1.getBearing(), 0.1);

	}

	/**
	 * Test ID P.9.3
	 */

	@Test
	public void updateCurrentBearingTest3() {
		// Test that the current heading moves towards the target heading
		plane1.setBearing(270);
		plane1.setTargetBearing(90);
		plane1.updateCurrentBearing();
		assertTrue(plane1.isTurningRight());
		assertFalse(plane1.isTurningLeft());
		assertEquals(270.9, plane1.getBearing(), 0.1);

	}

	/**
	 * Test ID P.9.4
	 */

	@Test
	public void updateCurrentBearingTest4() {
		// Test that the current heading moves towards the target heading
		plane1.setBearing(288);
		plane1.setTargetBearing(300);
		plane1.updateCurrentBearing();
		assertTrue(plane1.isTurningRight());
		assertFalse(plane1.isTurningLeft());
		assertEquals(288.9, plane1.getBearing(), 0.1);

	}

	/**
	 * Test ID P.9.5
	 */

	@Test
	public void updateCurrentBearingTest5() {
		// Test that the current heading moves towards the target heading
		plane1.setBearing(150);
		plane1.setTargetBearing(200);
		plane1.updateCurrentBearing();
		assertTrue(plane1.isTurningRight());
		assertFalse(plane1.isTurningLeft());
		assertEquals(150.9, plane1.getBearing(), 0.1);

	}

	/**
	 * Test ID P.9.6
	 */

	@Test
	public void updateCurrentBearingTest6() {
		// Test that the current heading moves towards the target heading
		plane1.setBearing(20);
		plane1.setTargetBearing(290);
		plane1.updateCurrentBearing();
		assertTrue(plane1.isTurningLeft());
		assertFalse(plane1.isTurningRight());
		assertEquals(19.1, plane1.getBearing(), 0.1);

	}

	/**
	 * Test ID P.9.7
	 */

	@Test
	public void updateCurrentBearingTest7() {
		// Test that the current heading moves towards the target heading
		plane1.setBearing(359.5);
		plane1.setTargetBearing(10);
		plane1.updateCurrentBearing();
		assertTrue(plane1.isTurningRight());
		assertFalse(plane1.isTurningLeft());
		assertEquals(0, plane1.getBearing(), 0.1);

	}

	/**
	 * Test ID P.9.8
	 */

	@Test
	public void updateCurrentBearingTest8() {
		// Test that the current heading moves towards the target heading
		plane1.setBearing(0.5);
		plane1.setTargetBearing(290);
		plane1.updateCurrentBearing();
		assertTrue(plane1.isTurningLeft());
		assertFalse(plane1.isTurningRight());
		assertEquals(360, plane1.getBearing(), 0.1);

	}

	// Testing findLandingDescentRate

	/**
	 * Test ID P.10
	 */

	@Test
	public void findLandingDescentRateTest1() {
		plane1.setX(695);
		plane1.setY(465);
		plane1.setVelocity(1);
		game.setSpeedDifficulty(0.5);
		plane1.setAltitude(2000);
		assertEquals(39.223, plane1.findLandingDescentRate(), 0.001);

	}

	// Testing Landing Plane

	/**
	 * Test ID P.11
	 */

	@Test
	public void landPlaneTest1() {
		plane1.setX(695);
		plane1.setY(465);
		plane1.setAltitude(2000);
		plane1.setBearing(170);
		plane1.land(false);
		assertTrue(game.getAirport().isPlaneLanding());
		assertFalse(plane1.getNeedsToLand());
		assertTrue(plane1.isLanding());
		assertEquals(plane1.getTarget(), plane1.getFlightPlan()
				.getCurrentRoute().get(0));
		assertFalse(game.getManualPlanes().contains(plane1));
		assertEquals(null, game.getCurrentPlane());

	}

	// Testing Update X and Y Coordinates

	/**
	 * Test ID P.12
	 */

	@Test
	public void updateXYCoordinatesTest1() {
		plane1.setX(150);
		plane1.setY(400);
		plane1.setVelocity(1);
		plane1.setBearing(100);
		game.setSpeedDifficulty(0.5);
		plane1.updateXYCoordinates();
		assertEquals(150.086, plane1.getX(), 0.001);
		assertEquals(399.507, plane1.getY(), 0.001);

	}

	// Testing Move Plane

	/**
	 * Test ID P.13.1
	 */

	@Test
	public void movePlaneTest1() {
		plane1.setX(150);
		plane1.setY(400);
		plane1.setVelocity(1);
		plane1.setBearing(100);
		plane1.setAltitude(100);
		plane1.setTargetAltitude(100);
		plane1.setLanding(false);
		game.setSpeedDifficulty(0.5);
		plane1.movePlane();
		assertEquals(150.086, plane1.getX(), 0.001);
		assertEquals(399.507, plane1.getY(), 0.001);

	}

	/**
	 * Test ID P.13.2
	 */

	@Test
	public void movePlaneTest2() {
		plane1.setX(150);
		plane1.setY(400);
		plane1.setVelocity(1);
		plane1.setBearing(100);
		plane1.setTargetBearing(150);
		plane1.setAltitude(3000);
		game.setSpeedDifficulty(0.5);
		game.getManualPlanes().add(plane1);
		plane1.movePlane();
		assertEquals(150.094, plane1.getX(), 0.001);
		assertEquals(399.509, plane1.getY(), 0.001);
		assertEquals(100.9, plane1.getBearing(), 0.1);

	}

	/**
	 * Test ID P.13.3
	 */

	@Test
	public void movePlaneTest3() {
		plane1.setX(150);
		plane1.setY(400);
		plane1.setVelocity(1);
		plane1.setBearing(100);
		plane1.setTargetBearing(100);
		plane1.setAltitude(3000);
		plane1.setTarget(game.getListOfWaypoints().get(0));
		game.setSpeedDifficulty(0.5);
		plane1.movePlane();
		assertEquals(150.094, plane1.getX(), 0.001);
		assertEquals(399.509, plane1.getY(), 0.001);
		assertEquals(111.8, plane1.getTargetBearing(), 0.1);
		assertEquals(100.9, plane1.getBearing(), 0.1);

	}

	// Testing Take Off

	/**
	 * Test ID P.14
	 */

	@Test
	public void takeOffTest1() {
		plane1.takeOff();
		assertTrue(plane1.getVelocity() != 0);
		assertFalse(game.getManualPlanes().contains(plane1));
		assertFalse(plane1.getNeedsToTakeOff());
		assertTrue(plane1.isTakingOff());
		assertTrue(game.getCurrentPlane() == null);
	}

	// Testing Update Plane Altitude

	/**
	 * Test ID P.15.1
	 */

	@Test
	public void updatePlaneAltitudeTest1() {
		plane1.setLandingDescentRate(10);
		plane1.setAltitude(-10);
		plane1.updatePlaneAltitude();
		assertEquals(0, plane1.getAltitude(), 0);
		assertEquals(0, plane1.getLandingDescentRate(), 0);
		assertEquals(0, plane1.getTargetAltitude(), 0);

	}

	/**
	 * Test ID P.15.2
	 */

	@Test
	public void updatePlaneAltitudeTest2() {
		plane1.setLandingDescentRate(10);
		plane1.setAltitude(100);
		plane1.updatePlaneAltitude();
		assertEquals(90, plane1.getAltitude(), 0);

	}

	/**
	 * Test ID P.15.3
	 */

	@Test
	public void updatePlaneAltitudeTest3() {
		plane1.setLandingDescentRate(0);
		plane1.setAltitude(100);
		plane1.setTargetAltitude(150);
		plane1.updatePlaneAltitude();
		assertEquals(105, plane1.getAltitude(), 0);

	}

	/**
	 * Test ID P.15.4
	 */

	@Test
	public void updatePlaneAltitudeTest4() {
		plane1.setLandingDescentRate(0);
		plane1.setAltitude(100);
		plane1.setTargetAltitude(90);
		plane1.updatePlaneAltitude();
		assertEquals(95, plane1.getAltitude(), 0);

	}

}
