package states;
import org.newdawn.slick.*;
import org.newdawn.slick.state.*;



public class GameOverState extends BasicGameState {
	
	private Image game_over_background, play_again_button, quit_button, menu_button;
	
	public GameOverState(int state) {
		
	}
	

	public void init(GameContainer gc, StateBasedGame sbg)
				throws SlickException {
		
		game_over_background = new Image("res/graphics/menu_graphics/gameover_screen.png");
		play_again_button = new Image("res/graphics/menu_graphics/playagain_button.png");
		quit_button = new Image("res/graphics/menu_graphics/quit_button.png");
		menu_button = new Image("res/graphics/menu_graphics/menu_button.png");


	}

	public void render(GameContainer gc, StateBasedGame sbg, Graphics g)
				throws SlickException{
		
		game_over_background.draw(0,0);
		play_again_button.draw(354,380);
		menu_button.draw(728,380);
		quit_button.draw(1148,556);
		g.setColor(Color.white);
	}

	public void update(GameContainer gc, StateBasedGame sbg, int delta)
				throws SlickException {
	}

	public int getID() {
			return 3;
	}


}
