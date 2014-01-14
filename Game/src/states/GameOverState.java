package states;
import org.lwjgl.input.Mouse;
import org.newdawn.slick.*;
import org.newdawn.slick.state.*;



public class GameOverState extends BasicGameState {
	
	private Image gameOverBackground, playAgainButton, quitButton, menuButton;
	
	public GameOverState(int state) {
		
	}
	
	public void init(GameContainer gc, StateBasedGame sbg)
				throws SlickException {
		
		gameOverBackground = new Image("res/graphics/menu_graphics/gameover_screen.png");
		playAgainButton = new Image("res/graphics/menu_graphics/playagain_button.png");
		quitButton = new Image("res/graphics/menu_graphics/quit_button.png");
		menuButton = new Image("res/graphics/menu_graphics/menu_button.png");
	}

	public void render(GameContainer gc, StateBasedGame sbg, Graphics g)
				throws SlickException{
		
		gameOverBackground.draw(0,0);
		playAgainButton.draw(354,380);
		menuButton.draw(728,380);
		quitButton.draw(1148,556);
		g.setColor(Color.white);
	}

	public void update(GameContainer gc, StateBasedGame sbg, int delta)throws SlickException {
		
		int posX=Mouse.getX();
		int posY=Mouse.getY();
		
		if(posX>354&&posX<354+228&&posY>176&&posY<220&&Mouse.isButtonDown(0)) {
			sbg.getState(1).init(gc, sbg);
			sbg.enterState(1);
		}
		
	}

	public int getID() {
			return 3;
	}
}
