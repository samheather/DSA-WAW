package states;
import org.lwjgl.input.Mouse;
import org.newdawn.slick.*;
import org.newdawn.slick.state.*;



public class GameOverState extends BasicGameState {
	
	private Image gameOverBackground, playAgainButton, quitButton, menuButton;
	private Image playAgainHover, quitHover, menuHover;
	
	public GameOverState(int state) {
		
	}
	
	public void init(GameContainer gc, StateBasedGame sbg)
				throws SlickException {
		
		gameOverBackground = new Image("res/graphics/menu_graphics/gameover_screen.png");
		playAgainButton = new Image("res/graphics/menu_graphics/playagain_button.png");
		quitButton = new Image("res/graphics/menu_graphics/quit_button.png");
		menuButton = new Image("res/graphics/menu_graphics/menu_button.png");
		playAgainHover = new Image("res/graphics/menu_graphics/playagain_hover.png");
		quitHover = new Image("res/graphics/menu_graphics/quit_hover.png");
		menuHover = new Image("res/graphics/menu_graphics/menu_hover.png");
	}

	public void render(GameContainer gc, StateBasedGame sbg, Graphics g)
				throws SlickException{
		
		gameOverBackground.draw(0,0);
		
		int posX=Mouse.getX();
		int flippedposY=Mouse.getY();
		//Fixing posY to reflect graphics coords
		int posY = 600 - flippedposY;
		
		if (posX>778&&posX<844+228&&posY>380&&posY<426){
			menuHover.draw(728,380);
		} else {
			menuButton.draw(728,380);
		}
		
		if (posX>354&&posX<582+228&&posY>380&&posY<424){
			playAgainHover.draw(354,380);
		} else {
			playAgainButton.draw(354,380);
		}
		
		if ((posX > 1150 && posX < 1170) && (posY > 550 && posY < 580)){
			quitHover.draw(1148,556);
		} else {
			quitButton.draw(1148,556);
		}
		
		g.setColor(Color.white);
	}

	public void update(GameContainer gc, StateBasedGame sbg, int delta)throws SlickException {
		
		int posX=Mouse.getX();
		int posY=Mouse.getY();
		
		posY = 600 - posY;
		
		if(posX>354&&posX<582+228&&posY>380&&posY<424&&Mouse.isButtonDown(0)) {
			sbg.enterState(1);
		}
		
		if(posX>778&&posX<844+228&&posY>380&&posY<426&&Mouse.isButtonDown(0)) { // 116 46
			sbg.enterState(0);
		}
		
		if((posX > 1150 && posX < 1170) && (posY > 550 && posY < 580)) {
			if(Mouse.isButtonDown(0)) {
				System.exit(0);
			}
			
		}
		else {
		}
	}

	public int getID() {
			return 2;
	}
}
