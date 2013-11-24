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

		a.new_waypoint(100, 100);
		a.new_entry_point(1200, 400);
		a.new_entry_point(1200, 200);
		a.new_entry_point(600, 0);
		
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