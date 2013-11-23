import org.newdawn.slick.*;
import org.newdawn.slick.state.*;

public class Play extends BasicGameState {
	
	Airspace a = new Airspace();
	Flight flight;
	Calculations calc;
	
	
	public Play(int state) {
		
	}
	
	public void init(GameContainer gc, StateBasedGame sbj) throws SlickException {
		
		flight = new Flight(a);
		a.add_flight(flight);
		flight.init();
		flight.setX(1100);
		flight.setY(600);
		flight.setFlight_num(1);
	}
	
	public void render(GameContainer gc, StateBasedGame sbj, Graphics g) throws SlickException {
		flight.render(g);
		a.render(g);

	}
	
	public void update(GameContainer gc, StateBasedGame sbj, int delta) throws SlickException {
		flight.update();
	}
	public int getID() {
		return 1;
	}
	
}