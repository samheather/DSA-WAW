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
		a.new_exit_point(100, 100);
		a.new_exit_point(100, 300);
		a.new_exit_point(100, 350);
	}
				


	
	public void render(GameContainer gc, StateBasedGame sbj, Graphics g) throws SlickException {
		a.render(g);
		g.setColor(Color.blue);
		g.fillRect(0, 0, (float) 1200, (float)600);
		g.setColor(Color.lightGray);
		g.fillRoundRect(0, 500, 1200, 100,5);
		g.fillRoundRect(0, 0, 100, 600,5);
		
		
		a.render(g);

	}
	
	public void update(GameContainer gc, StateBasedGame sbj, int delta) throws SlickException {
		if(a.new_flight2(i)) {
			i++;
		}
		a.update(gc);
	}
	public int getID() {
		return 1;
	}


	
}