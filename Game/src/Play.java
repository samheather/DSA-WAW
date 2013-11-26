import java.awt.Cursor;
import java.util.Random;

import org.lwjgl.input.Mouse;
import org.newdawn.slick.*;
import org.newdawn.slick.state.*;
import org.newdawn.slick.Color;

public class Play extends BasicGameState {
	
	private Airspace a; 
	private int i;
	Image cursorImg; 
	
	
	public Play(int state) {
		a = new Airspace();
		i=1;
		
	}
	
	public void init(GameContainer arg0, StateBasedGame arg1)
			throws SlickException {
		// TODO Auto-generated method stub
		
		/*cursorImg= new Image("res/cursor.png");
		gc.setMouseCursor(cursorImg, 16, 16);
		if someone can make a decent cursor image we can have a better cursor*/
		a.init(arg0);
		a.new_waypoint(150, 150);
		a.new_waypoint(400, 470);
		a.new_waypoint(700, 60);
		a.new_waypoint(800, 320);
		a.new_waypoint(600, 418);
		a.new_waypoint(500, 220);
		a.new_waypoint(950, 188);
		a.new_waypoint(1050, 272);
		a.new_waypoint(900, 420);
		a.new_waypoint(240, 250);
		a.new_entry_point(1200, 400);
		a.new_entry_point(1200, 200);
		a.new_entry_point(600, 0);
		a.new_exit_point(800, 0);
		a.new_exit_point(100, 200);
		a.new_exit_point(1200, 300);

	}
				


	
	public void render(GameContainer gc, StateBasedGame sbj, Graphics g) throws SlickException {
		g.setColor(Color.black);
		g.fillRect(0, 0, (float) 1200, (float)600);
		g.setColor(Color.lightGray);
		g.fillRect(0, 500, 1200, 100);
		g.fillRect(0, 0, 100, 600);
		
		
		a.render(g, gc);

	}
	
	public void update(GameContainer gc, StateBasedGame sbj, int delta) throws SlickException {
		if(a.new_flight2(i,gc)) {
			i++;
		}
		a.update(gc);
	}
	public int getID() {
		return 1;
	}


	
}