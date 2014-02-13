
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
	public void generateEntryPointTest1(){
		assertTrue(game.getListOfEntryPoints().contains(plan.generateEntryPoint(game)));
	}
	
	@Test
	public void generateEntryPointTest2(){
		// Testing that the function generates a entrypoint that is contained within the entrypoint list
    	Point result = plane.getFlightPlan().generateEntryPoint(game);
    	assertTrue(result == game.getListOfEntryPoints().get(0) || result == game.getListOfEntryPoints().get(1) || result == game.getListOfEntryPoints().get(2) | result == game.getListOfEntryPoints().get(3));
		
	}
	
	
	@Test
	public void buildRouteTest1(){
		assertTrue(game.getListOfEntryPoints().retainAll(plan.buildRoute(game, plan.getEntryPoint()))
				   && game.getListOfExitPoints().retainAll(plan.buildRoute(game, plan.getEntryPoint())));
	
	}
	
	@Test 
	public void buildRouteTest2(){
		
		//Testing that it doesn't repeat waypoints
		for (int i = 0; i < 100; i++){
			ArrayList<Point> route = plan.buildRoute(game, plane.getFlightPlan().getEntryPoint());
			boolean samePoint = false;
			
			
			for (int j = 0; j < route.size(); j++) {
				  for (int k = j+1; k < route.size(); k++) {
				    if (route.get(j).equals(route.get(k))){
				    	samePoint = true;
				    }
				  }
			}
			assertFalse(samePoint);
		}
		
		
	}
	
	@Test
	public void buildRouteTest3(){
		
		// Testing that it doesn't build a route if no game has no exitpoints
		
		Game gameWithoutExitpoints = new Game(50, 100);
		
		//Removing All ExitPoints
		
		gameWithoutExitpoints.getListOfExitPoints().removeAll(gameWithoutExitpoints.getListOfExitPoints());


    	Plane plane2 = new Plane(1, 500, 3000, 50, gameWithoutExitpoints);
    	
    	
    	ArrayList<Point> route = plan.buildRoute(gameWithoutExitpoints, plane2.getFlightPlan().getEntryPoint());
    	assertTrue(route.size() == 0);
		
	}
	
	@Test
	public void buildRouteTest4(){
		
		// Testing that it doesn't build a route if no game has no Waypoints
		
		Game gameWithoutWaypoints = new Game(50, 100);
		
		//Removing All Waypoints
		
		gameWithoutWaypoints.getListOfWaypoints().removeAll(gameWithoutWaypoints.getListOfWaypoints());


    	Plane plane2 = new Plane(1, 500, 3000, 50, gameWithoutWaypoints);
    	
    	
    	ArrayList<Point> route = plan.buildRoute(gameWithoutWaypoints, plane2.getFlightPlan().getEntryPoint());
    	assertTrue(route.size() == 0);
		
	}
	
	@Test
	public void buildRouteTest5(){
		
		// Testing Length of Route
		for (int i = 0; i < 100; i++){
			ArrayList<Point> route = plan.buildRoute(game, plane.getFlightPlan().getEntryPoint());
			assertTrue(route.size() >= 3 && route.size() <= 5);
		}
	}
	
	
	
	
}