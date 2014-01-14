package states;
import org.newdawn.slick.*;
import org.newdawn.slick.state.*;
import org.newdawn.slick.util.ResourceLoader;
import org.lwjgl.input.Mouse;

import java.awt.Font;
import java.io.InputStream;

public class PauseState extends BasicGameState {
	
	public static TrueTypeFont font;
	private Color colourContinueButton = Color.green;
	private Color colourQuitButton = Color.green;
	private Image menuBackground;
	
	
	public PauseState(int state) {
		
	}

	public void init(GameContainer gc, StateBasedGame sbj) throws SlickException {
		try{
			menuBackground = new Image("res/graphics/menu_graphics/menu_screen.png");
			InputStream inputStream = ResourceLoader.getResourceAsStream("res/blue_highway font/bluehigh.ttf");
			Font awtFont= Font.createFont(Font.TRUETYPE_FONT, inputStream);
			awtFont = awtFont.deriveFont(20f);
			font = new TrueTypeFont(awtFont, false);
			
			
		}catch(Exception e){
			e.printStackTrace();
		}
		gc.setShowFPS(false);
	}
	
	public void render(GameContainer gc, StateBasedGame sbg, Graphics g) throws SlickException {
		menuBackground.draw(0,0);
		g.setColor(Color.white);
		g.setFont(font);
		g.drawString("Pause", 10, 40);
		g.setColor(colourContinueButton);
		g.fillRoundRect(100, 100, 200, 70, 15);
		g.setColor(colourQuitButton);
		g.fillRoundRect(100, 200, 200, 70, 15);
		g.setColor(Color.black);
		g.setFont(font);
		g.drawString("Continue", 135, 115);
		g.drawString("Quit",  135,  215);
		g.setColor(Color.white);
	}
	
	public void update(GameContainer gc, StateBasedGame sbg, int delta) throws SlickException {
		
		int posX = Mouse.getX();
		int posY = Mouse.getY();
		
		posY = 600 - posY;
		
		Input input = gc.getInput();
		
		if(input.isKeyPressed(Input.KEY_P)) {
			sbg.enterState(1);
		}
		
		if((posX>100&&posX<300)&&(posY>100 &&posY<170)) {
			colourContinueButton=Color.white;
			if(Mouse.isButtonDown(0)) {
				sbg.enterState(1);
			}
			
		}
		else {
			colourContinueButton=Color.green;
		}
		if((posX>100&&posX<300)&&(posY>200&&posY<270)) {
			colourQuitButton=Color.white;
			if(Mouse.isButtonDown(0)) {
				System.exit(0);
			}
			
		}
		else {
			colourQuitButton=Color.green;
		}
	}
	
	public int getID(){
		return 3;
	}
}
