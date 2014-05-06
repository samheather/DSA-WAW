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
import game.struct.SingleplayerPlane;

public class GameTests {

	Game game;
	Plane plane1;

	// @Test
	// public

	@Before
	public void beforeTests() throws NoSuchAlgorithmException,
			UnknownHostException, IOException {

		game = new SingleplayerGame(50, 100, 0, 1);
		plane1 = new SingleplayerPlane(1, 500, 3000, 50, game, 0);

	}

	@Test
	public void testCreatePlane() {
		assertTrue(this.game.getCurrentPlanes().size() == 0);
		this.game.createPlane();
		assertTrue(this.game.getCurrentPlanes().size() == 1);
	}

	@Test
	public void testGenerateAltitude() {
		for (int i = 0; i < 200; i++) {
			int alt = this.game.generateAltitude();
			assertTrue(alt >= 1000 && alt <= 7000);
		}
	}

	@Test
	public void testGenerateVelocity() {
		for (int i = 0; i < 200; i++) {
			double velocity = this.game.generateVelocity();
			assertTrue(velocity == 1 || velocity == 1.1 || velocity == 1.2);
		}
	}

	@Test
	public void testGetPlaneFromID() {
		game.createPlane();
		game.createPlane();
		game.createPlane();
		game.getCurrentPlanes().get(0).setID(100);
		Plane plane = game.getPlaneFromID(100);
		assertEquals(100, plane.getID(), 0);

	}

	@Test
	public void collsionTest() {
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

	@Test
	public void tenWaypointsTest() throws NoSuchAlgorithmException,
			UnknownHostException, IOException {
		game = new SingleplayerGame(100, 100, 0, 1);
		assertEquals(10, game.getListOfWaypoints().size(), 0);

	}

	@Test
	public void tenPlanesTest() throws NoSuchAlgorithmException,
			UnknownHostException, IOException {
		game = new SingleplayerGame(100, 100, 0, 1);
		for (int i = 0; i < 10; i++) {
			game.createPlane();

		}

		assertEquals(10, game.getCurrentPlanes().size(), 0);
	}

	@Test
	public void takingOffRestrictionTest() throws NoSuchAlgorithmException,
			UnknownHostException, IOException {
		// Checking that when a flight is waiting to take off another flight
		// cannot be designated to take off.

		game = new SingleplayerGame(100, 100, 0, 1);

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
