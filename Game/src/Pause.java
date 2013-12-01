import org.newdawn.slick.*;
import org.newdawn.slick.state.*;
import org.newdawn.slick.util.ResourceLoader;
import org.lwjgl.input.Mouse;
import java.awt.Font;
import java.io.InputStream;

public class Pause extends BasicGameState {
	
	public static TrueTypeFont font;
	private Color colourContinueButton = Color.green;
	private Color colourQuitButton = Color.green;
	
	
	public Pause(int state) {
		
	}

	public void init(GameContainer gc, StateBasedGame sbj) throws SlickException {
		try{
			InputStream inputStream = ResourceLoader.getResourceAsStream("res/blue_highway font/bluehigh.ttf");
			Font awtFont= Font.createFont(Font.TRUETYPE_FONT, inputStream);
			awtFont = awtFont.deriveFont(20f);
			font = new TrueTypeFont(awtFont, false);
			
			
		}catch(Exception e){
			e.printStackTrace();
		}
		gc.setShowFPS(false);
	}
	
	public void render(GameContainer gc, StateBasedGame sbj, Graphics g) throws SlickException {
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
		
		int posiX = Mouse.getX();
		int posiY = Mouse.getY();
		
		Input input = gc.getInput();
		
		if(input.isKeyPressed(Input.KEY_P)) {
			sbg.enterState(2);
		}
		
		if((posiX>100&&posiX<300)&&(posiY>430&&posiY<500)) {
			colourContinueButton=Color.white;
			if(Mouse.isButtonDown(0)) {
				sbg.enterState(2);
			}
			
		}
		else {
			colourContinueButton=Color.green;
		}
		if((posiX>100&&posiX<300)&&(posiY>330&&posiY<400)) {
			colourQuitButton=Color.white;
			if(Mouse.isButtonDown(0)) {
				sbg.enterState(3);
			}
			
		}
		else {
			colourQuitButton=Color.green;
		}
	}
	
	public int getID(){
		return 4;
	}
}
