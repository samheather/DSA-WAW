package game.tests;
import org.junit.Test;
import org.junit.Before;
import static org.junit.Assert.*;
import java.util.ArrayList;
import game.struct.*;

public class ScoreTests {

	Game game;
	Plane plane;
	
	@Before
	public void beforeTests(){
		game = new Game(50, 100);
		plane = new Plane(1, 500, 3000, 50, game);
	}
	
	@Test
	public void initialScoreTest(){
		assertTrue(game.getScore().getScore() == 0);
	}
	
	@Test
	public void initialMultiplierTest(){
		assertTrue(game.getScore().getMultiplier() == 1);
	}
	
	@Test
	public void addScoreTest(){
		
		// Set (x, y) of plane to next waypoint coordinates
		plane.setX(plane.getFlightPlan().getCurrentRoute().get(0).getX());
		plane.setY(plane.getFlightPlan().getCurrentRoute().get(0).getY());
		
		// Run addScore method
		game.getScore().addScore(plane, game);
		
		// Testing of values
		assertTrue(game.getScore().getScore() == 5 || game.getScore().getScore() == 10 || game.getScore().getScore() == 0);
	}
	
	@Test
	public void planeLeftAirspaceOrWaitingToTakeoffMinusTest1(){
		
		// Set (x, y) of plane to location outside of airspace
		plane.setX(1201);
		plane.setY(601);
				
		/*
		 * Testing - Plane left airspace
		 * 		   - Current score = 0;
		 * 		   - Current multiplier = 1;
		 */
	
		// Run planeLeftAirspaceOrWaitingToTakeoffMinusScore
		game.getScore().planeLeftAirspaceOrWaitingToTakeOffMinusScore();
		
		// Testing of values
		assertTrue(game.getScore().getScore() == 0);
	}
	
	@Test 
	public void planeLeftAirspaceOrWaitingToTakeoffMinusTest2(){
		
		// Set (x, y) of plane to location outside of airspace
		plane.setX(1201);
		plane.setY(601);
				
		/*
		 * Testing - Plane left airspace
		 * 		   - Current score = 10;
		 * 		   - Current multiplier = 1;
		 */
		
		// Set current score = 10;
		game.getScore().setScore(10);
		
		// Run planeLeftAirspaceOrWaitingToTakeoffMinusScore
		game.getScore().planeLeftAirspaceOrWaitingToTakeOffMinusScore();
		
		// Testing of values
		assertTrue(game.getScore().getScore() == 0);
	}
	
	@Test
	public void planeLeftAirspaceOrWaitingToTakeoffMinusTest3(){
		
		// Set (x, y) of plane to location outside of airspace
		plane.setX(1201);
		plane.setY(601);
		
		/*
		 * Testing - Plane left airspace
		 * 		   - Current score = 30;
		 * 		   - Current multiplier = 2;
		 */
		
		// Set current score = 20;
		game.getScore().setScore(30);
		
		// Set current multiplier = 2;
		game.getScore().setMultiplier(2);
		
		// Run planeLeftAirspaceOrWaitingToTakeoffMinusScore
		game.getScore().planeLeftAirspaceOrWaitingToTakeOffMinusScore();
		
		// Testing of values
		assertTrue(game.getScore().getScore() == 10);
	}
	
	@Test
	public void planePilotedPerfectlyMultiplierBonusTest1(){
		
		/*
		 * Testing - plane.getViolationOccurred() = false;
		 * 		   - Current multiplier = 1;
		 */
		
		// Run planePilotedPerfectlyMultiplierBonus
		game.getScore().planePilotedPerfectlyMultiplierBonus(plane);
		
		// Testing of values
		assertTrue(game.getScore().getMultiplier() == 2);
	}
	
	@Test
	public void planePilotedPerfectlyMultiplierBonusTest2(){
		
		/*
		 * Testing - plane.getViolationOccurred() = false;
		 * 		   - Current multiplier = 20;
		 */
		
		// Set current multiplier = 20;
		game.getScore().setMultiplier(20);
		
		// Run planePilotedPerfectlyMultiplierBonus
		game.getScore().planePilotedPerfectlyMultiplierBonus(plane);
		
		// Testing of values
		assertTrue(game.getScore().getMultiplier() == 20);
	}
	
	@Test
	public void planePilotedPerfectlyMultiplierBonusTest3(){
		
		/*
		 * Testing - plane.getViolationOccurred() = true;
		 * 		   - Current multiplier = 1;
		 */
		
		// Set plane.getViolationOccurred() = true;
		plane.setViolationOccurred();
		
		// Set current multiplier = 1;
		game.getScore().setMultiplier(1);
		
		//Run planePilotedPerfectlyMultiplierBonus
		game.getScore().planePilotedPerfectlyMultiplierBonus(plane);
		
		// Testing of values
		assertTrue(game.getScore().getMultiplier() == 1);
	}
	
	@Test
	public void planeCollisionWarningMultAndScorePenaltiesTest1(){
	
		/*
		 * Testing - Current score = 5;
		 *         - Current multiplier = 1;
		 */
		
		// Set current score = 5;
		game.getScore().setScore(5);
		
		// Run planeCollisionWarningMultAndScorePenalties
		game.getScore().planeCollisionWarningMultAndScorePenalties();
		
		// Testing of values
		assertTrue(game.getScore().getScore() == 0 && game.getScore().getMultiplier() == 1);
	}
	
	@Test
	public void planeCollisionWarningMultAndScorePenaltiesTest2(){
		
		/*
		 * Testing - Current score = 5;
		 * 		   - Current multiplier = 2;
		 */
		
		// Set current score = 5;
		game.getScore().setScore(5);
		
		// Set current multiplier = 2;
		game.getScore().setMultiplier(2);
		
		// Run planeCollisionWarningMultAndScorePenalties	
		game.getScore().planeCollisionWarningMultAndScorePenalties();
		
		// Testing of values
		assertTrue(game.getScore().getScore() == 0 && game.getScore().getMultiplier() == 1);
	}
	
	@Test
	public void planeCollisionWarningMultAndScorePenaltiesTest3(){
		
		/*
		 * Testing - Current score = 15;
		 * 		   - Current multiplier = 2;
		 */
		
		// Set current score = 15;
		game.getScore().setScore(15);
		
		// Set current multiplier = 2;
		game.getScore().setMultiplier(2);
		
		// Run planeCollisionWarningMultAndScorePenalties
		game.getScore().planeCollisionWarningMultAndScorePenalties();
		
		// Testing of values
		assertTrue(game.getScore().getScore() == 5 && game.getScore().getMultiplier() == 1);
	}
	
	@Test
	public void planeCollisionWarningMultAndScorePenaltiesTest4(){
		
		/*
		 * Testing - Current score = 0;
		 * 		   - Current multiplier = 1;
		 */
		
		// Set current score = 0;
		game.getScore().setScore(0);
				
		// Set current multiplier = 1;
		game.getScore().setMultiplier(1);
				
		// Run planeCollisionWarningMultAndScorePenalties
		game.getScore().planeCollisionWarningMultAndScorePenalties();
				
		// Testing of values
		assertTrue(game.getScore().getScore() == 0 && game.getScore().getMultiplier() == 1);
	}
}
