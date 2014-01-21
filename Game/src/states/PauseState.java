package states;
import org.newdawn.slick.*;
import org.newdawn.slick.state.*;
import org.newdawn.slick.util.ResourceLoader;
import org.lwjgl.input.Mouse;

import java.awt.Font;
import java.io.InputStream;

public class PauseState extends BasicGameState {
	
	public static TrueTypeFont font;
	private int pageNumber;

	private Image pauseBackgroundPage1,pauseBackgroundPage2, backButton, nextPageButton, previousPageButton, menuButton, quitButton;
	
	
	public PauseState(int state) {
		
	}

	public void init(GameContainer gc, StateBasedGame sbj) throws SlickException {
		try{
			pageNumber = 1;
			
			pauseBackgroundPage1 = new Image("res/menu_graphics/pause_screen.jpg");
			pauseBackgroundPage2 = new Image("res/menu_graphics/controls2.jpg");
			backButton = new Image("res/menu_graphics/back.png");
			nextPageButton = new Image("res/menu_graphics/next page.png");
			previousPageButton = new Image("res/menu_graphics/previous page.png");
			menuButton = new Image("res/menu_graphics/menu_button.png");
			quitButton = new Image("res/menu_graphics/quit_button.png");
			
			InputStream inputStream = ResourceLoader.getResourceAsStream("res/blue_highway_font/bluehigh.ttf");
			Font awtFont= Font.createFont(Font.TRUETYPE_FONT, inputStream);
			awtFont = awtFont.deriveFont(20f);
			font = new TrueTypeFont(awtFont, false);
			
			
		}catch(Exception e){
			e.printStackTrace();
		}
		gc.setShowFPS(false);
	}
	
	public void render(GameContainer gc, StateBasedGame sbg, Graphics g) throws SlickException {
		
		g.setFont(font);
		if (pageNumber == 1) {
			pauseBackgroundPage1.draw(0,0);
			backButton.draw(20,20);
			nextPageButton.draw(1030,280);
			//menuButton.draw(1050, 20);
			quitButton.draw(1150,550);
			g.setColor(Color.white);
		}
		
		else if (pageNumber == 2){
			pauseBackgroundPage2.draw(0,0);
			backButton.draw(20,20);
			previousPageButton.draw(30,280);
			//menuButton.draw(1050, 20);
			quitButton.draw(1150,550);
			
		}
	
	}
	
	public void update(GameContainer gc, StateBasedGame sbg, int delta) throws SlickException {
		
		int posX = Mouse.getX();
		int posY = Mouse.getY();
		
		posY = 600 - posY;
		
		Input input = gc.getInput();
		
		if(input.isKeyPressed(Input.KEY_P)) {
			pageNumber = 1;
			sbg.enterState(1);
		}
		
		
		if((posX > 20 && posX < 40) && (posY > 20 && posY < 40)) {
			if(Mouse.isButtonDown(0)) {
				pageNumber = 1;
				sbg.enterState(1);
			}
			
		}
		else {
		}
		
		
		if((posX > 1150 && posX < 1170) && (posY > 550 && posY < 580)) {
			if(Mouse.isButtonDown(0)) {
				System.exit(0);
			}
			
		}
		else {
		}
		
		if (pageNumber == 1){
			
			if((posX > 1030 && posX < 1193) && (posY > 280 && posY < 315)) {
				if(Mouse.isButtonDown(0)) {
					pageNumber = 2;
				}
				
			}
			else {
			}
		}
		
		if (pageNumber == 2){
			if((posX > 30 && posX < 241) && (posY > 280 && posY < 315)) {
				if(Mouse.isButtonDown(0)) {
					pageNumber = 1;
				}
				
			}
			else {
			}
		}
	}
	
	public int getID(){
		return 3;
	}
}
