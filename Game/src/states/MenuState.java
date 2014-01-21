package states;
import org.newdawn.slick.*;
import org.newdawn.slick.state.*;
import org.lwjgl.input.Mouse;
import java.awt.Font;
import java.io.InputStream;
import org.newdawn.slick.util.ResourceLoader;
import org.newdawn.slick.TrueTypeFont;



public class MenuState extends BasicGameState {
	public static TrueTypeFont font;
	private Image creditsHover, controlsHover, menuBackground, playButton, quitButton, playHover, quitHover, creditsButton, controlsButton;
	

	public MenuState(int state) {

	}

	public void init(GameContainer gc, StateBasedGame sbg)
			throws SlickException {
		
		menuBackground = new Image("res/graphics/menu_graphics/menu_screen.png");
		playButton = new Image("res/graphics/menu_graphics/play_button.png");
		playHover = new Image("res/graphics/menu_graphics/play_hover.png");
		quitButton = new Image("res/graphics/menu_graphics/quit_button.png");
		quitHover = new Image("res/graphics/menu_graphics/quit_hover.png");
		creditsButton = new Image("res/graphics/menu_graphics/credits.png");
		creditsHover = new Image("res/graphics/menu_graphics/credits_hover.png");
		controlsButton = new Image("res/graphics/menu_graphics/controls_silver.png");
		controlsHover = new Image("res/graphics/menu_graphics/controls_hover.png");
		
	
	}

	public void render(GameContainer gc, StateBasedGame sbg, Graphics g)
			throws SlickException {
		g.setColor(Color.white);
		g.setFont(font);
		menuBackground.draw(0,0);
		
		
		int posX = Mouse.getX();
		int posY = Mouse.getY();
		
		if ((posX > 439 && posX < 762) && (posY > 165 && posY < 255)){
			playHover.draw(439,349);
		}
		else{
			playButton.draw(439,349);
		}
		
		if((posX > 1140 && posX < 1262) && (posY > 25 && posY < 50)){
			quitHover.draw(1148,556);
		}
		else{
			quitButton.draw(1148,556);
		}
		
		if (posX>20 && posX< 136 && posY>20 && posY<66){
			creditsHover.draw(20,534);
		} else {
			creditsButton.draw(20,534);
		}
		
		if (posX>490 && posX<725 && posY>20 && posY<66){
			controlsHover.draw(490,534);
		} else {
			controlsButton.draw(490,534);
		}


		g.setColor(Color.white);
		gc.setShowFPS(false);

	}

	public void update(GameContainer gc, StateBasedGame sbg, int delta) throws SlickException {
		
		int posX = Mouse.getX();
		int posY = Mouse.getY();
		
		// Mapping Mouse coords onto graphics coords
		posY = 600 - posY;
		if (Mouse.isButtonDown(0)) {
			System.out.println(posX);
			System.out.println(posY);
		}

		if ((posX > 439 && posX < 762) && (posY > 349 && posY < 439)) {
			playHover.draw(439,349);
			if (Mouse.isButtonDown(0)) {
				sbg.enterState(1);
			}

		} 
		
		if ((posX > 490 && posX < 725) && (posY > 534 && posY < 596)) {
			if (Mouse.isButtonDown(0)) {
				sbg.enterState(5);
			}

		} 
		
		if ((posX > 1148 && posX < 1172) && (posY > 556 && posY < 582)) {
			if (Mouse.isButtonDown(0)) {
				System.exit(0);
			}
		}
		
		if( (posX>20 && posX< 178 && posY>534 && posY<575) && Mouse.isButtonDown(0)) {
			sbg.enterState(4);
		}
	}

	public int getID() {
		return 0;
	}

}
