package game.tests;


import org.junit.Test;
import org.junit.Before;
import static org.junit.Assert.*;

import java.util.ArrayList;

import game.struct.Game;
import game.struct.Plane;
import game.struct.Waypoint;


public class PlaneTests {
	
	Game game;
	
	@Before
	public void beforeTests() {
		this.game = new Game(50, 100, true);
		
		assertTrue("Game instance could not be created",
						this.game instanceof Game);
	}

	@Test
	public void testGenerateFlightPlanTemplate() {
		// Post: 	sizeof(templatePoints) = pointsToAdd + 2

		for(int pointsToAdd = 0; pointsToAdd < 20; pointsToAdd++) {
			for(int i = 0; i < 500; i++) {
				// Add test plane
				Plane plane = game.createPlane(true);
				
				assertTrue("Plane instance could not be created",
						plane instanceof Plane);

				// Generate flight plan
				ArrayList<double[]> templatePoints = plane
										.generateFlightPlanTemplate(pointsToAdd,
												this.game);

				assertTrue("Incorrect number of template points created",
							templatePoints.size() == (pointsToAdd + 2));
			}
		}
	}

	@Test
	public void testInterpolateCurve() {
		// Post: 	sizeof(target) = ((pointsToAdd + 1) * 40) + (pointsToAdd + 2)
		
		Plane testPlane = game.createPlane(true);
		ArrayList<double[]> testPoints;
		Waypoint target;
		int targetSize;
		
		assertTrue("Plane instance could not be created",
				testPlane instanceof Plane);

		for(int pointsToAdd = 0; pointsToAdd < 20; pointsToAdd++) {
			// Generate test points
			testPoints = testPlane.generateFlightPlanTemplate(pointsToAdd,
					this.game);
			target = testPlane.interpolateCurve(testPoints);
			targetSize = 0;
			
			// Count number of Waypoints in target list
			while(target != null) {
				targetSize++;
				target = target.getNext();
			}
			
			assertTrue("Incorrect number of target Waypoints generated",
						targetSize == (((pointsToAdd + 1) * 60)
								+ (pointsToAdd + 2)) - (10 * (pointsToAdd + 1)));
		}
	}
}
