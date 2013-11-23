import java.util.Random;

import org.newdawn.slick.*;
import org.newdawn.slick.state.*;

public class Play extends BasicGameState {
	
	Airspace a; 
	Calculations calc;
	int i;
	
	
	public Play(int state) {
		a = new Airspace();
		i=1;
		
	}
	
	public void init(GameContainer gc, StateBasedGame sbj) throws SlickException {
		
		a.new_waypoint(100, 100);
		
		a.init();
		
		
		
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