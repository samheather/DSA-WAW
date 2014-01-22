package states;

import org.newdawn.slick.*;
import org.newdawn.slick.state.*;
import org.lwjgl.input.Mouse;


public class CreditsState extends BasicGameState {
	
	private Image menuBackground, menuButton, menuHover;

	public CreditsState(int state){
		
	}
	
	public void init(GameContainer gc, StateBasedGame sbg)
			throws SlickException {
	
	menuBackground = new Image("res/menu_graphics/menu_screen.png");
	menuButton = new Image("res/menu_graphics/menu_button.png");
	menuHover = new Image("res/menu_graphics/menu_hover.png");
	
	}

	public void render(GameContainer gc, StateBasedGame sbg, Graphics g)
			throws SlickException{
		
		int posX=Mouse.getX();
		int flippedposY=Mouse.getY();
		//Fixing posY to reflect graphics coords
		int posY = 600 - flippedposY;
	
		menuBackground.draw(0,0);
		
		if (posX>20 && posX< 136 && posY>20 && posY<66){
			menuHover.draw(20,20);
		} else {
			menuButton.draw(20,20);
		}
		
		g.setColor(new Color(250, 235, 215, 50));
		g.fillRoundRect (50, 230, 1100, 320, 5);
		
		g.setColor(Color.white);
		//setting text for credits
		String Title = "Music Assets";
		String MusicAssetsL1 = "\"Jarvic 8\" Kevin MacLeod (incompetech.com)";
		String MusicAssetsL3 = "Licensed under Creative Commons: By Attribution 3.0";
		String MusicAssetsL4 = "http://creativecommons.org/licenses/by/3.0/";
		
		String Title2 = "Images";
		String Image2 = "Background image attirubted to Rijksdienst voor het Cultureel Erfgoed";
		String Image3 = "This files are licensed under the Creative Commons Attribution-Share Alike 3.0 Netherlands license.";
		
		String Image4 = "Clouds image attirubted to Keith Pomakis";
		


		
		g.drawString(Title, 60f, 240f);
		g.drawString(MusicAssetsL1, 60f, 255f);
		g.drawString(MusicAssetsL3, 60f, 270f);
		g.drawString(MusicAssetsL4, 60f, 285f);
		
		
		g.drawString(MusicAssetsL3, 60f, 345f);
		g.drawString(MusicAssetsL4, 60f, 360f);
		
		g.drawString(Title2, 60f, 405f);
		g.drawString(Image2, 60f, 420f);
		g.drawString(Image4, 60f, 435f);
		g.drawString(Image3, 60f, 450f);
		
	}

	public void update(GameContainer gc, StateBasedGame sbg, int delta)
			throws SlickException {
	
		int posX=Mouse.getX();
		int flippedposY=Mouse.getY();
		//Fixing posY to reflect graphics coords
		int posY = 600 - flippedposY;
	
		if((posX>20 && posX< 136 && posY>20 && posY<66) && Mouse.isButtonDown(0)) {
			sbg.enterState(0);
		}
	
	}

	public int getID(){
		return 4;
	}
	
	
}
