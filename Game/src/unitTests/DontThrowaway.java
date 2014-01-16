package unitTests;



import static org.junit.Assert.*;
import org.junit.Test;
import logicClasses.*;


import org.newdawn.slick.*;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.AppGameContainer;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;

public class DontThrowaway {

@Test
	public void test() throws SlickException {
		
        new AppGameContainer(new BasicGame("Test") {
           
 
            @Override
            public void render(GameContainer container, Graphics g)
                    throws SlickException {
            	container.exit();
                
            }
 
            @Override
            public void update(GameContainer container, int delta)
                    throws SlickException {
            	Airspace airspace = new Airspace();
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
            	Flight flight1 = new Flight(airspace);

            	int result = flight1.generateAltitude();
            	assertEquals(27000, result);
            	container.exit();
            	

            }
 
            @Override
            public void init(GameContainer container) throws SlickException {

            }
                
            
        }).start();
		
		
		
		
	}
	
}
	
	
	


	

