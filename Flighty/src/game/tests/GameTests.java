
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
	//@Test
	//public 
	
	@Before
	public void beforeTests() {
		
		game = new Game(100,100);
	
	}
	
	@Test
	public void testCreatePlane() {
		assertTrue(this.game.getCurrentPlanes().size() == 0);
		this.game.createPlane();
		assertTrue(this.game.getCurrentPlanes().size()==1);
	}
	
	@Test
	public void testGenerateAltitude() {
		for(int i = 0; i<200; i++) {
			int alt = this.game.generateAltitude();
			assertTrue(alt>=1000&&alt<=7000);
		}
	}
	
	@Test
	public void testGenerateVelocity() {
		for(int i = 0; i<200; i++) {
			double velocity = this.game.generateVelocity();
			assertTrue(velocity==1||velocity==0.8||velocity==1.2);
		}
	}
	
	@Test
	public void testRemovePlane() {
		this.game.createPlane();
		assertTrue(this.game.getCurrentPlanes().size()==1);
		this.game.removePlane(this.game.getCurrentPlanes().get(0));
		assertTrue(this.game.getCurrentPlanes().size()==0);
	}
	
	@Test
	public void testGetPlaneFromID() {
		//can't test?
	}
	
	@Test
	public void testRemoveFromManual() {
		this.game.createPlane();
		this.game.getManualPlanes().add(this.game.getCurrentPlanes().get(0));
		assertTrue(this.game.getManualPlanes().size()==1);
		this.game.removeFromManual(this.game.getCurrentPlanes().get(0));
		assertTrue(this.game.getManualPlanes().size()==0);
	}
	
	
	
	/*
	Game game;
	
	@Before
	public void beforeTests() {
		this.game = new Game(50, 100, true);
		
		assertTrue("Game instance could not be created",
				this.game instanceof Game);
	}

	@Test
	public void testGame() {
		// Pre:		sizeof(carriers) = 0
		// Post:	sizeof(carriers) = 5
		
		Game testGame = new Game(50, 100, true);
		
		assertTrue("Game instance could not be created",
				testGame instanceof Game);
		
		assertTrue("Carriers set up incorrectly",
				testGame.getCarriers().size() == 5);
	}

	@Test
	public void testCreatePlane() {
		// Pre:		sizeof(game.currentPlanes) = planeCountBefore
		// Post:	sizeof(game.currentPlanes) = planeCountBefore + 1
		
		int planesToCreate = 30;
		
		// Create some planes, and check that each Plane is added to the game
		for(int i = 0; i < planesToCreate; i++) {
			int planeCountBefore = this.game.getCurrentPlanes().size();
		
			this.game.createPlane();
		
			assertTrue("Plane not added correctly",
					this.game.getCurrentPlanes().size() == planeCountBefore + 1);
		}
	}
	
	@Test
	public void testCreatePlaneIdRandom() {
		// Tests that created plane IDs are not duplicated
		// Note: this imposes a limit of carriers * 100
		
		int planesToCreate = 30;
		
		// Create some test planes
		for(int i = 0; i < planesToCreate; i++) {
			this.game.createPlane();
		}
		
		assertTrue("Test planes created incorrectly",
				this.game.getCurrentPlanes().size() == planesToCreate);
		
		// Check for duplicate IDs
		for(Plane plane : this.game.getCurrentPlanes()) {
			assertFalse("Duplicate plane ID found: " + plane.getID(),
					this.checkForDuplicatePlaneIds(plane));
		}
	}
	
	public boolean checkForDuplicatePlaneIds(Plane searchPlane) {
		boolean selfRefFound = false;
		
		for(Plane curPlane : this.game.getCurrentPlanes()) {
			if(curPlane.equals(searchPlane)) {
				if(!selfRefFound) {
					selfRefFound = true;
				} else {
					return true;
				}
			}
		}
		return false;
	}

	@Test
	public void testRemovePlaneAll() {
		// Pre:		sizeof(game.currentPlanes) = 0
		// Post:	sizeof(game.currentPlanes) = 0
		
		int planesToCreate = 30;
		
		assertTrue("Precondition that game.currentPlanes be empty not met",
				this.game.getCurrentPlanes().size() == 0);
		
		// Create some test planes
		for(int i = 0; i < planesToCreate; i++) {
			this.game.createPlane();
		}
		
		assertTrue("Test planes created incorrectly",
				this.game.getCurrentPlanes().size() == planesToCreate);
		
		// Remove planes
		for(int i = this.game.getCurrentPlanes().size(); i > 0; i--) {
			this.game.removePlane(this.game.getCurrentPlanes().get(i - 1));
		}
		
		assertTrue("Planes removed incorrectly",
				this.game.getCurrentPlanes().size() == 0);
	}
	
	@Test
	public void testRemovePlaneIndividual() {
		// Post:	if(game.currentPlanes.contains(plane))
		//				->	sizeof(game.currentPlanes)
		//						= sizeof(previousPlanes) - 1
		//				->	game.currentPlanes.contains(plane) = false
		// 			else
		//				->	sizeof(game.currentPlanes)
		//						= sizeof(previousPlanes)

		int planesToCreate = 30;
		int randomPoint = (int) (random() * planesToCreate);
		boolean planeFound = false;
		boolean planeRemoved = true;
		
		// Create plane to remove
		Plane planeToRemove = new Plane("TEST", 0, 0, 0, 0, 1d, 0f, 0f);
		
		assertTrue("Test plane created incorrectly",
				planeToRemove instanceof Plane);
		
		// Create some test planes
		// Also add in planeToDelete at a random point
		for(int i = 0; i < planesToCreate; i++) {
			if(i == randomPoint) {
				this.game.getCurrentPlanes().add(planeToRemove);
			} else {
				this.game.createPlane();
			}
		}
		
		assertTrue("Test planes created incorrectly",
				this.game.getCurrentPlanes().size() == planesToCreate);
		
		// Check that planeToRemove is present in game
		for(Plane plane : this.game.getCurrentPlanes()) {
			if(plane.equals(planeToRemove)) {
				planeFound = true;
			}
		}
		
		assertTrue("Test plane has not been added to game",
				planeFound);
		
		// Remove planeToRemove
		this.game.removePlane(planeToRemove);
		
		// Check plane no longer in array
		for(Plane plane : this.game.getCurrentPlanes()) {
			if(plane.equals(planeToRemove)) {
				planeRemoved = false;
			}
		}
		
		assertTrue("Test plane has not been removed from game",
				planeRemoved);
		
		assertTrue("Test plane removed from game incorrectly",
				this.game.getCurrentPlanes().size() == planesToCreate - 1);
	}

	@Test
	public void testGetPlaneFromIDInUse() {
		// Post:	if(game.currentPlanes.getID().contains(planeID))
		//				-> return game.currentPlanes.getID() = planeID
		//			else
		//				-> return null
		
		int planesToCreate = 30;
		String testID = "TEST";
		int randomPoint = (int) (random() * planesToCreate);
		boolean planeFound = false;
		Plane returnedPlane;
		
		// Create plane to find
		Plane planeToFind = new Plane(testID, 0, 0, 0, 0, 1d, 0f, 0f);
		
		assertTrue("Test plane created incorrectly",
				planeToFind instanceof Plane);
		
		// Create some test planes
		// Also add in planeToFind at a random point
		for(int i = 0; i < planesToCreate; i++) {
			if(i == randomPoint) {
				this.game.getCurrentPlanes().add(planeToFind);
			} else {
				this.game.createPlane();
			}
		}
		
		assertTrue("Test planes created incorrectly",
				this.game.getCurrentPlanes().size() == planesToCreate);
		
		// Check that planeToRemove is present in game
		for(Plane plane : this.game.getCurrentPlanes()) {
			if(plane.equals(planeToFind)) {
				planeFound = true;
			}
		}
		
		assertTrue("Test plane has not been added to game",
				planeFound);
		
		// Find the test plane
		returnedPlane = game.getPlaneFromID(testID);
		
		// Check that returned plane is the test plane
		assertTrue("Incorrect plane returned",
				returnedPlane.equals(planeToFind));
	}
	
	@Test
	public void testGetPlaneFromIDNotInUse() {
		// Post:	if(game.currentPlanes.getID().contains(planeID))
		//				-> return game.currentPlanes.getID() = planeID
		//			else
		//				-> return null
		
		int planesToCreate = 30;
		String testID = "TEST";
		boolean planeFound = false;
		Plane returnedPlane;
		
		// Create plane to find
		Plane planeToFind = new Plane(testID, 0, 0, 0, 0, 1d, 0f, 0f);
		
		assertTrue("Test plane created incorrectly",
				planeToFind instanceof Plane);
		
		// Create some test planes
		for(int i = 0; i < planesToCreate; i++) {
			this.game.createPlane();
		}
		
		assertTrue("Test planes created incorrectly",
				this.game.getCurrentPlanes().size() == planesToCreate);
		
		// Check that planeToRemove is NOT present in game
		for(Plane plane : this.game.getCurrentPlanes()) {
			if(plane.equals(planeToFind)) {
				planeFound = true;
			}
		}
		
		assertFalse("Test plane has been added to game",
				planeFound);
		
		// Find the test plane
		returnedPlane = game.getPlaneFromID(testID);
		
		// Check that returned plane is null
		assertTrue("Returned plane isn't null",
				returnedPlane == null);
	}

	@Test
	public void testCollisionHelper() {
		// Post:	ForAll plane2	->	if(dist(plane1, plane2) < sepDist)
		//									-> return {true, alertStatus}
		//								else
		//									-> return {false, alertStatus}
		//			ForAll plane2	->	if(dist(plane1, plane2) < alertDist)
		//									-> return {colliding, true}
		//								else
		//									-> return {colliding, false}
		
		int testFactor = 500;
		
		String testID1 = "TEST1", testID2 = "TEST2";
		
		Plane plane1, plane2;
		ArrayList<Plane> planesToAdd;
		
		boolean[] boolNeither = new boolean[] {false, false};
		boolean[] boolCol = new boolean[] {true, false};
		boolean[] boolAlert = new boolean[] {false, true};
		
		float startX, startY;

		
		for(int i = 0; i < testFactor; i++) {
			// Set up start positions
			startX = (float) (Math.random() * 1024);
			startY = (float) (Math.random() * 512);
			
			// Test with two planes: not colliding or alerting
			planesToAdd = new ArrayList<Plane>();

			// Plane one at (0, 0)
			plane1 = new Plane(testID1, 0, 0, 0, 0, 1d, 0f, 0f);
			// Plane two at ('max', 'max')
			plane2 = new Plane(testID2, 0, 0, 0, 0, 1d, 1024, 512);

			planesToAdd.add(plane1);
			planesToAdd.add(plane2);
			this.game.setCurrentPlanes(planesToAdd);

			// Check planes added correctly
			assertTrue("Test plane one created incorrectly",
					plane1 instanceof Plane);
			assertTrue("Test plane two created incorrectly",
					plane2 instanceof Plane);
			assertTrue("Test planes added to game incorrectly",
					this.game.getCurrentPlanes().size() == 2);
			
			assertTrue("Planes marked as neither",
					compareBoolLists(game.collisionHelper(plane1), boolNeither));
			assertFalse("Planes marked as alerting",
					compareBoolLists(game.collisionHelper(plane1), boolAlert));
			assertFalse("Planes marked as colliding",
					compareBoolLists(game.collisionHelper(plane1), boolCol));



			// Test with two planes: NEARLY alerting
			planesToAdd = new ArrayList<Plane>();

			// Plane one at (startX, startY)
			plane1 = new Plane(testID1, 0, 0, 0, 0, 1d, startX, startY);
			// Plane two at (startX, 'startY + penDist + small change')
			plane2 = new Plane(testID2, 0, 0, 0, 0, 1d, startX,
					startY + (float) (game.getPenaltyDistance() + 0.0001));

			planesToAdd.add(plane1);
			planesToAdd.add(plane2);
			this.game.setCurrentPlanes(planesToAdd);

			// Check planes added correctly
			assertTrue("Test plane one created incorrectly",
					plane1 instanceof Plane);
			assertTrue("Test plane two created incorrectly",
					plane2 instanceof Plane);
			assertTrue("Test planes added to game incorrectly",
					this.game.getCurrentPlanes().size() == 2);

			assertTrue("Planes marked as neither",
					compareBoolLists(game.collisionHelper(plane1), boolNeither));
			assertFalse("Planes marked as alerting",
					compareBoolLists(game.collisionHelper(plane1), boolAlert));
			assertFalse("Planes marked as colliding",
					compareBoolLists(game.collisionHelper(plane1), boolCol));



			// Test with two planes: JUST alerting
			planesToAdd = new ArrayList<Plane>();

			// Plane one at (startX, startY)
			plane1 = new Plane(testID1, 0, 0, 0, 0, 1d, startX, startY);
			// Plane two at (startX, 'startY + penDist - small change')
			plane2 = new Plane(testID2, 0, 0, 0, 0, 1d, startX,
					startY + (float) (game.getPenaltyDistance() - 0.0001));

			planesToAdd.add(plane1);
			planesToAdd.add(plane2);
			this.game.setCurrentPlanes(planesToAdd);

			// Check planes added correctly
			assertTrue("Test plane one created incorrectly",
					plane1 instanceof Plane);
			assertTrue("Test plane two created incorrectly",
					plane2 instanceof Plane);
			assertTrue("Test planes added to game incorrectly",
					this.game.getCurrentPlanes().size() == 2);

			assertFalse("Planes marked as neither",
					compareBoolLists(game.collisionHelper(plane1), boolNeither));
			assertTrue("Planes marked as alerting",
					compareBoolLists(game.collisionHelper(plane1), boolAlert));
			assertFalse("Planes marked as colliding",
					compareBoolLists(game.collisionHelper(plane1), boolCol));



			// Test with two planes: alerting but not colliding
			planesToAdd = new ArrayList<Plane>();

			// Plane one at (startX, startY)
			plane1 = new Plane(testID1, 0, 0, 0, 0, 1d, startX, startY);
			// Plane two at (startX, 'startY + half way between sepDist and penDist')
			plane2 = new Plane(testID2, 0, 0, 0, 0, 1d, startX,
					startY + (float) ((game.getPenaltyDistance() + game.getSeparationDistance()) / 2));

			planesToAdd.add(plane1);
			planesToAdd.add(plane2);
			this.game.setCurrentPlanes(planesToAdd);

			// Check planes added correctly
			assertTrue("Test plane one created incorrectly",
					plane1 instanceof Plane);
			assertTrue("Test plane two created incorrectly",
					plane2 instanceof Plane);
			assertTrue("Test planes added to game incorrectly",
					this.game.getCurrentPlanes().size() == 2);

			assertFalse("Planes marked as neither",
					compareBoolLists(game.collisionHelper(plane1), boolNeither));
			assertTrue("Planes marked as alerting",
					compareBoolLists(game.collisionHelper(plane1), boolAlert));
			assertFalse("Planes marked as colliding",
					compareBoolLists(game.collisionHelper(plane1), boolCol));



			// Test with two planes: NEARLY colliding
			planesToAdd = new ArrayList<Plane>();

			// Plane one at (startX, startY)
			plane1 = new Plane(testID1, 0, 0, 0, 0, 1d, startX, startY);
			// Plane two at (startX, 'startY + sepDist + 0.0001')
			plane2 = new Plane(testID2, 0, 0, 0, 0, 1d, startX,
					startY + (float) (game.getSeparationDistance() + 0.0001));

			planesToAdd.add(plane1);
			planesToAdd.add(plane2);
			this.game.setCurrentPlanes(planesToAdd);

			// Check planes added correctly
			assertTrue("Test plane one created incorrectly",
					plane1 instanceof Plane);
			assertTrue("Test plane two created incorrectly",
					plane2 instanceof Plane);
			assertTrue("Test planes added to game incorrectly",
					this.game.getCurrentPlanes().size() == 2);

			assertFalse("Planes marked as neither",
					compareBoolLists(game.collisionHelper(plane1), boolNeither));
			assertTrue("Planes marked as alerting",
					compareBoolLists(game.collisionHelper(plane1), boolAlert));
			assertFalse("Planes marked as colliding",
					compareBoolLists(game.collisionHelper(plane1), boolCol));



			// Test with two planes: JUST colliding
			planesToAdd = new ArrayList<Plane>();

			// Plane one at (startX, startY)
			plane1 = new Plane(testID1, 0, 0, 0, 0, 1d, startX, startY);
			// Plane two at (startX, 'startY + sepDist - 0.0001')
			plane2 = new Plane(testID2, 0, 0, 0, 0, 1d, startX,
					startY + (float) (game.getSeparationDistance() - 0.0001));

			planesToAdd.add(plane1);
			planesToAdd.add(plane2);
			this.game.setCurrentPlanes(planesToAdd);

			// Check planes added correctly
			assertTrue("Test plane one created incorrectly",
					plane1 instanceof Plane);
			assertTrue("Test plane two created incorrectly",
					plane2 instanceof Plane);
			assertTrue("Test planes added to game incorrectly",
					this.game.getCurrentPlanes().size() == 2);

			assertFalse("Planes marked as neither",
					compareBoolLists(game.collisionHelper(plane1), boolNeither));
			assertFalse("Planes marked as alerting",
					compareBoolLists(game.collisionHelper(plane1), boolAlert));
			assertTrue("Planes marked as colliding",
					compareBoolLists(game.collisionHelper(plane1), boolCol));



			// Test with two planes: colliding
			planesToAdd = new ArrayList<Plane>();

			// Plane one at (startX, startY)
			plane1 = new Plane(testID1, 0, 0, 0, 0, 1d, startX, startY);
			// Plane two at (startX, 'startY + half the sepDist')
			plane2 = new Plane(testID2, 0, 0, 0, 0, 1d, startX,
					startY + (float) (game.getSeparationDistance() / 2));

			planesToAdd.add(plane1);
			planesToAdd.add(plane2);
			this.game.setCurrentPlanes(planesToAdd);

			// Check planes added correctly
			assertTrue("Test plane one created incorrectly",
					plane1 instanceof Plane);
			assertTrue("Test plane two created incorrectly",
					plane2 instanceof Plane);
			assertTrue("Test planes added to game incorrectly",
					this.game.getCurrentPlanes().size() == 2);

			assertFalse("Planes marked as neither",
					compareBoolLists(game.collisionHelper(plane1), boolNeither));
			assertFalse("Planes marked as alerting",
					compareBoolLists(game.collisionHelper(plane1), boolAlert));
			assertTrue("Planes marked as colliding",
					compareBoolLists(game.collisionHelper(plane1), boolCol));
		}
	}
	
	public boolean compareBoolLists(boolean[] list1, boolean[] list2) {
		if(list1 == null) {
			if(list2 == null) {
				return true;
			}
		}
		
		if(!(list1 instanceof boolean[])
				|| !(list2 instanceof boolean[])) {
			return false;
		}
		
		if(list1.length != list2.length) {
			return false;
		}
		
		for(int i = 0; i < list1.length; i++) {
			if(list1[i] != list2[i]) {
				return false;
			}
		}
		
		return true;
	}
*/
}


