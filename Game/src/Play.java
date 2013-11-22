import org.newdawn.slick.*;
import org.newdawn.slick.state.*;

public class Play extends BasicGameState {
	
	Flight flight = new Flight();
	
	public Play(int state) {
		flight.setX(200);
		flight.setY(200);
		flight.setFlight_num(1);
		flight.setTarget_heading(90);
	}
	
	public void init(GameContainer gc, StateBasedGame sbj) throws SlickException {
		flight.init();
	}
	
	public void render(GameContainer gc, StateBasedGame sbj, Graphics g) throws SlickException {
		flight.render(g);
	}
	
	public void update(GameContainer gc, StateBasedGame sbj, int delta) throws SlickException {
		flight.update();
	}
	public int getID() {
		return 1;
	}
	
}