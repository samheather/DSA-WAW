import org.newdawn.slick.*;
import org.newdawn.slick.state.*;
import org.lwjgl.input.Mouse;
import java.awt.Font;


public class GameOverState extends BasicGameState {
	
	public GameOverState(int state) {

	}
	
	



	public void init(GameContainer gc, StateBasedGame sbj)
				throws SlickException {


	}

	public void render(GameContainer gc, StateBasedGame sbj, Graphics g)
				throws SlickException {
		
		g.setColor(Color.white);
		g.drawString("Game Over", 10, 40);
		
		

	}

	public void update(GameContainer gc, StateBasedGame sbg, int delta)
				throws SlickException {
	}

	public int getID() {
			return 3;
	}


}
