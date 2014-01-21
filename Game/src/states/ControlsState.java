package states;

import java.awt.Font;
import java.io.InputStream;

import org.newdawn.slick.*;
import org.newdawn.slick.state.*;
import org.newdawn.slick.util.ResourceLoader;
import org.lwjgl.input.Mouse;


public class ControlsState extends BasicGameState {
	
	private int pageNumber;
	private Image controlsBackgroundPage1,controlsBackgroundPage2, backButton, nextPageButton, previousPageButton, menuButton, quitButton;
    private Image nextPageHover, previousPageHover, quitHover;
    
	public ControlsState(int state){
		
	}
	
	public void init(GameContainer gc, StateBasedGame sbg)
			throws SlickException {
	
		try{
			pageNumber = 1;
			
			controlsBackgroundPage1 = new Image("res/menu_graphics/controls1.jpg");
			controlsBackgroundPage2 = new Image("res/menu_graphics/controls2.jpg");
			backButton = new Image("res/menu_graphics/back.png");
			nextPageButton = new Image("res/menu_graphics/next page.png");
			nextPageHover = new Image("res/menu_graphics/next page_hover.png");
			previousPageButton = new Image("res/menu_graphics/previous page.png");
			previousPageHover = new Image("res/menu_graphics/previous hover.png");
			quitButton = new Image("res/menu_graphics/quit_button.png");
			quitHover = new Image("res/menu_graphics/quit_hover.png");
			
			
			
		}catch(Exception e){
			e.printStackTrace();
		}
	
	}

	public void render(GameContainer gc, StateBasedGame sbg, Graphics g)
			throws SlickException{
		
		int posX=Mouse.getX();
		int flippedposY=Mouse.getY();
		//Fixing posY to reflect graphics coords
		int posY = 600 - flippedposY;
		
		if (pageNumber == 1) {
			controlsBackgroundPage1.draw(0,0);
			backButton.draw(20,20);
			
			if ((posX > 1030 && posX < 1193) && (posY > 280 && posY < 315)){
				nextPageHover.draw(1030,280);
			} else {
				nextPageButton.draw(1030,280);
			}
			//menuButton.draw(1050, 20);
			
			if ((posX > 1150 && posX < 1170) && (posY > 550 && posY < 580)){
				quitHover.draw(1148,556);
			} else {
				quitButton.draw(1148,556);
			}
			
		}
		
		else if (pageNumber == 2){
			controlsBackgroundPage2.draw(0,0);
			backButton.draw(20,20);

			if ((posX > 30 && posX < 241) && (posY > 280 && posY < 315)){
				previousPageHover.draw(30,280);
			} else {
				previousPageButton.draw(30,280);
			}
			
			//menuButton.draw(1050, 20);
			
			if ((posX > 1150 && posX < 1170) && (posY > 550 && posY < 580)){
				quitHover.draw(1148,556);
			} else {
				quitButton.draw(1148,556);
			}
			
		}

		
	}

	public void update(GameContainer gc, StateBasedGame sbg, int delta)
			throws SlickException {
	

		int posX = Mouse.getX();
		int posY = Mouse.getY();
		
		posY = 600 - posY;
		
		Input input = gc.getInput();
		

		
		
		if((posX > 20 && posX < 40) && (posY > 20 && posY < 40)) {
			if(Mouse.isButtonDown(0)) {
				pageNumber = 1;
				sbg.enterState(0);
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
		return 5;
	}
	
	
}
