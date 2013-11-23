import java.util.Random;

import org.newdawn.slick.*;
import org.newdawn.slick.state.*;

public class Play extends BasicGameState {
	
	Airspace a; 
	Calculations calc;
	
	
	public Play(int state) {
		a = new Airspace();

		
	}
	
	public void init(GameContainer gc, StateBasedGame sbj) throws SlickException {
		a.new_flight(1);
		a.new_flight(2);
		a.new_flight(3);
		a.new_flight(4);
		a.new_flight(5);
		a.new_flight(6);
		a.new_flight(7);
		a.new_flight(8);
		a.new_flight(9);
		a.new_waypoint(100, 100);
		
		a.init();
		
		
	}
	
	public void render(GameContainer gc, StateBasedGame sbj, Graphics g) throws SlickException {
		a.render(g);

	}
	
	public void update(GameContainer gc, StateBasedGame sbj, int delta) throws SlickException {

		a.update(gc);
	}
	public int getID() {
		return 1;
	}
	
}