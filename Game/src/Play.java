import org.newdawn.slick.*;
import org.newdawn.slick.state.*;

public class Play extends BasicGameState {
	
	Flight flight = new Flight();
	Waypoint wp1;
	
	public Play(int state) {
		flight.setX(200);
		flight.setY(200);
		flight.setFlight_num(1);
		flight.setTarget_heading(45);
	}
	
	public void init(GameContainer gc, StateBasedGame sbj) throws SlickException {
		flight.init();
		wp1 = new Waypoint(200,200);
	}
	
	public void render(GameContainer gc, StateBasedGame sbj, Graphics g) throws SlickException {
		flight.render(g);
		wp1.render(g);
	}
	
	public void update(GameContainer gc, StateBasedGame sbj, int delta) throws SlickException {
		flight.update();
	}
	public int getID() {
		return 1;
	}
	
}