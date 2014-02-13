
package game.tests;


import org.junit.Test;
import org.junit.Before;
import static org.junit.Assert.*;

import java.util.ArrayList;

import game.struct.*;
public class FlightPlanTests {

	Game game;
	Plane plane;
	FlightPlan plan;
	
	@Before
	public void beforeTests() {
		game = new Game(50, 100);
		plane = new Plane(1, 500, 3000, 50, game);
		plan = new FlightPlan(game, plane);
	}
	
	@Test
	public void generateEntryPointTest(){
		assertTrue(game.getListOfEntryPoints().contains(plan.generateEntryPoint(game)));
	}
	
	
	@Test
	public void buildRouteTest(){
		assertTrue(game.getListOfEntryPoints().retainAll(plan.buildRoute(game, plan.getEntryPoint()))
				   && game.getListOfExitPoints().retainAll(plan.buildRoute(game, plan.getEntryPoint())));
	}
}