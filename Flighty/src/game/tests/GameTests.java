package game.tests;

import org.junit.Test;
import org.junit.Before;
import static org.junit.Assert.*;

import java.util.ArrayList;
import static java.lang.Math.random;

import game.struct.Game;
import game.struct.Plane;

public class GameTests {

	Game game;
	Plane plane1;

	// @Test
	// public

	@Before
	public void beforeTests() {

		game = new Game(100, 100);
		plane1 = new Plane(1, 500, 3000, 50, game);

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
			assertTrue(velocity == 1 || velocity == 0.8 || velocity == 1.2);
		}
	}

	@Test
	public void testRemovePlane() {
		this.game.createPlane();
		assertTrue(this.game.getCurrentPlanes().size() == 1);
		this.game.removePlane(this.game.getCurrentPlanes().get(0));
		assertTrue(this.game.getCurrentPlanes().size() == 0);
	}

	@Test
	public void testGetPlaneFromID() {
		// can't test?
	}

	@Test
	public void testRemoveFromManual() {
		this.game.createPlane();
		this.game.getManualPlanes().add(this.game.getCurrentPlanes().get(0));
		assertTrue(this.game.getManualPlanes().size() == 1);
		this.game.removeFromManual(this.game.getCurrentPlanes().get(0));
		assertTrue(this.game.getManualPlanes().size() == 0);
	}

	@Test
	public void testDeleteFromManual() {
		this.game.getManualPlanes().add(plane1);
		this.game.deleteFromManual(plane1);
		assertFalse(this.game.getManualPlanes().contains(plane1));
	}

	@Test
	public void tenWaypointsTest() {
		game = new Game(100, 100);
		assertEquals(10, game.getListOfWaypoints().size(), 0);

	}

	@Test
	public void tenPlanesTest() {
		game = new Game(100, 100);
		for (int i = 0; i < 10; i++) {
			game.createPlane();

		}

		assertEquals(10, game.getCurrentPlanes().size(), 0);
	}
	
//	@Test
//	public void takingOffRestrictionTest(){
//		game = new Game(100, 100);
//		plane1 = new Plane(1, 500, 3000, 50, game);
//		plane1.setNeedsToTakeOff(true);
//		
//		for (int i =0; i < 100; i++){
//			Plane plane2 = new Plane(1, 500, 3000, 50, game);
//			assertTrue(plane2.getFlightPlan().getEntryPoint())
//			
//		}
//		
//	}

}
