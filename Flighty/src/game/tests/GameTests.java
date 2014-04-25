package game.tests;

import org.junit.Test;
import org.junit.Before;
import static org.junit.Assert.*;

import java.io.IOException;
import java.net.UnknownHostException;
import java.security.NoSuchAlgorithmException;

import game.struct.Game;
import game.struct.Plane;
import game.struct.SingleplayerGame;

public class GameTests {

	Game game;
	Plane plane1;

	// @Test
	// public

	@Before
	public void beforeTests() throws NoSuchAlgorithmException, UnknownHostException, IOException {

		game = new SingleplayerGame(50, 100, 0);
		plane1 = new Plane(1, 500, 3000, 50, game, 0);

	}

	/**
	 * Test ID X.1
	 */

	@Test
	public void testCreatePlane() {
		assertTrue(this.game.getCurrentPlanes().size() == 0);
		this.game.createPlane();
		assertTrue(this.game.getCurrentPlanes().size() == 1);
	}

	/**
	 * Test ID X.2
	 */

	@Test
	public void testGenerateAltitude() {
		for (int i = 0; i < 200; i++) {
			int alt = this.game.generateAltitude();
			assertTrue(alt >= 1000 && alt <= 7000);
		}
	}

	/**
	 * Test ID X.3
	 */

	@Test
	public void testGenerateVelocity() {
		for (int i = 0; i < 200; i++) {
			double velocity = this.game.generateVelocity();
			assertTrue(velocity == 1 || velocity == 1.1 || velocity == 1.2);
		}
	}

	/**
	 * Test ID X.4
	 */

	@Test
	public void testRemovePlane() {
		this.game.createPlane();
		assertTrue(this.game.getCurrentPlanes().size() == 1);
		this.game.removePlane(this.game.getCurrentPlanes().get(0));
		assertTrue(this.game.getCurrentPlanes().size() == 0);
	}

	/**
	 * Test ID X.5
	 */

	@Test
	public void testGetPlaneFromID() {
		game.createPlane();
		game.createPlane();
		game.createPlane();
		game.getCurrentPlanes().get(0).setID(100);
		Plane plane = game.getPlaneFromID(100);
		assertEquals(100, plane.getID(), 0);

	}

	/**
	 * Test ID X.6
	 */

	@Test
	public void testRemoveFromManual() {
		this.game.createPlane();
		this.game.getManualPlanes().add(this.game.getCurrentPlanes().get(0));
		assertTrue(this.game.getManualPlanes().size() == 1);
		this.game.removeFromManual(this.game.getCurrentPlanes().get(0));
		assertTrue(this.game.getManualPlanes().size() == 0);
	}

	/**
	 * Test ID X.7
	 */

	@Test
	public void testDeleteFromManual() {
		this.game.getManualPlanes().add(plane1);
		this.game.deleteFromManual(plane1);
		assertFalse(this.game.getManualPlanes().contains(plane1));
	}

	/**
	 * Test ID X.8.1
	 */

	@Test
	public void collsionTest1() {
		game.createPlane();
		game.createPlane();
		game.getCurrentPlanes().get(0).setX(100);
		game.getCurrentPlanes().get(0).setY(100);
		game.getCurrentPlanes().get(0).setAltitude(1000);
		game.getCurrentPlanes().get(1).setX(100);
		game.getCurrentPlanes().get(1).setY(100);
		game.getCurrentPlanes().get(1).setAltitude(1000);
		assertTrue(game.collision(game.getCurrentPlanes().get(0)));

	}

	/**
	 * Test ID X.8.2
	 */

	@Test
	public void collsionTest2() {
		game.createPlane();
		game.createPlane();
		game.getCurrentPlanes().get(0).setX(100);
		game.getCurrentPlanes().get(0).setY(100);
		game.getCurrentPlanes().get(0).setAltitude(1000);
		game.getCurrentPlanes().get(1).setX(100);
		game.getCurrentPlanes().get(1).setY(100);
		game.getCurrentPlanes().get(1).setAltitude(1600);
		assertFalse(game.collision(game.getCurrentPlanes().get(0)));
		assertFalse(game.getCurrentPlanes().get(0).getAlertStatus());
		assertFalse(game.getCurrentPlanes().get(1).getAlertStatus());

	}

	/**
	 * Test ID X.8.3
	 */

	@Test
	public void collsionTest3() {
		game.createPlane();
		game.createPlane();
		game.getCurrentPlanes().get(0).setX(100);
		game.getCurrentPlanes().get(0).setY(100);
		game.getCurrentPlanes().get(0).setAltitude(1000);
		game.getCurrentPlanes().get(1).setX(50);
		game.getCurrentPlanes().get(1).setY(50);
		game.getCurrentPlanes().get(1).setAltitude(1300);
		assertFalse(game.collision(game.getCurrentPlanes().get(0)));
		assertTrue(game.getCurrentPlanes().get(0).getAlertStatus());
		assertTrue(game.getCurrentPlanes().get(1).getAlertStatus());

	}

	/**
	 * Test ID X.9
	 * @throws IOException 
	 * @throws UnknownHostException 
	 * @throws NoSuchAlgorithmException 
	 */

	@Test
	public void tenWaypointsTest() throws NoSuchAlgorithmException, UnknownHostException, IOException {
		game = new SingleplayerGame(100, 100, 0);
		assertEquals(10, game.getListOfWaypoints().size(), 0);

	}

	/**
	 * Test ID X.10
	 * @throws IOException 
	 * @throws UnknownHostException 
	 * @throws NoSuchAlgorithmException 
	 */

	@Test
	public void tenPlanesTest() throws NoSuchAlgorithmException, UnknownHostException, IOException {
		game = new SingleplayerGame(100, 100, 0);
		for (int i = 0; i < 10; i++) {
			game.createPlane();

		}

		assertEquals(10, game.getCurrentPlanes().size(), 0);
	}

	/**
	 * Test ID X.11
	 * @throws IOException 
	 * @throws UnknownHostException 
	 * @throws NoSuchAlgorithmException 
	 */

	@Test
	public void takingOffRestrictionTest() throws NoSuchAlgorithmException, UnknownHostException, IOException {
		// Checking that when a flight is waiting to take off another flight
		// cannot be designated to take off.

		game = new SingleplayerGame(100, 100, 0);

		game.createPlane();

		// Loop until the plane that is created is one that needs to take off
		while (!game.getCurrentPlanes().get(0).getFlightPlan()
				.getCurrentRoute().get(0)
				.equals(game.getAirport().getEndOfRunway())) {
			game.getCurrentPlanes().remove(0);
			game.createPlane();
		}

		// check that when creating 1000 planes, a flight never needs to take
		// off as a flight already needs to
		for (int i = 1; i < 1001; i++) {
			game.createPlane();
			assertFalse(game.getCurrentPlanes().get(i).getFlightPlan()
					.getCurrentRoute().get(0)
					.equals(game.getAirport().getEndOfRunway()));

		}

	}

}
