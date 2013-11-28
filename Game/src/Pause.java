import org.newdawn.slick.*;
import org.newdawn.slick.state.*;
import org.lwjgl.input.Mouse;
import java.awt.Font;

public class Pause extends BasicGameState {
	
	private TrueTypeFont titleFont;
	private TrueTypeFont buttonFont;
	private Color colourContinueButton = Color.green;
	private Color colourQuitButton = Color.green;
	
	public Pause(int state) {
		
	}

	public void init(GameContainer gc, StateBasedGame sbj) throws SlickException {
		Font tFont = new Font("Courier",Font.BOLD,30);
		titleFont = new TrueTypeFont(tFont, false);
		Font bFont = new Font("Courier",Font.BOLD,25);
		buttonFont = new TrueTypeFont(bFont, false);
		gc.setShowFPS(false);
	}
	
	public void render(GameContainer gc, StateBasedGame sbj, Graphics g) throws SlickException {
		g.setColor(Color.white);
		g.setFont(titleFont);
		g.drawString("Pause", 10, 40);
		g.setColor(colourContinueButton);
		g.fillRoundRect(100, 100, 200, 70, 15);
		g.setColor(colourQuitButton);
		g.fillRoundRect(100, 200, 200, 70, 15);
		g.setColor(Color.black);
		g.setFont(buttonFont);
		g.drawString("Continue", 135, 115);
		g.drawString("Quit",  135,  215);
		g.setColor(Color.white);
	}
	
	public void update(GameContainer gc, StateBasedGame sbg, int delta) throws SlickException {
		
		int posiX = Mouse.getX();
		int posiY = Mouse.getY();
		
		Input input = gc.getInput();
		
		if(input.isKeyPressed(Input.KEY_ESCAPE)) {
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
