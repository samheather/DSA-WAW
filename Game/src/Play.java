import java.awt.Cursor;
import java.util.Random;

import org.lwjgl.input.Mouse;
import org.newdawn.slick.*;
import org.newdawn.slick.state.*;

public class Play extends BasicGameState {
	
	private Airspace a; 
	private int i;
	
	
	public Play(int state) {
		a = new Airspace();
		i=1;
		
	}
	
	public void init(GameContainer gc, StateBasedGame sbj) throws SlickException {		
		a.new_waypoint(700, 250);
		a.new_waypoint(300, 150);
		a.new_waypoint(900, 50);
		a.init();
		a.new_waypoint(200, 150);
		a.new_waypoint(400, 150);
		a.new_waypoint(600, 150);
		a.new_waypoint(800, 150);
		a.new_waypoint(1000, 150);
		a.new_waypoint(200, 450);
		a.new_waypoint(400, 450);
		a.new_waypoint(600, 450);
		a.new_waypoint(800, 450);
		a.new_waypoint(1000, 450);
		a.new_entry_point(1200, 400);
		a.new_entry_point(1200, 200);
		a.new_entry_point(600, 0);
		a.new_exit_point(0, 100);
		a.new_exit_point(0, 400);
		a.new_exit_point(0, 550);
		
		
		
		
		
	}
	
	public void render(GameContainer gc, StateBasedGame sbj, Graphics g) throws SlickException {
		if(a.new_flight2(i)) {
			i++;
		}
		a.render(g);

	}
	
	public void update(GameContainer gc, StateBasedGame sbj, int delta) throws SlickException {
		
		a.update(gc);
	}
	public int getID() {
		return 1;
	}
	
}