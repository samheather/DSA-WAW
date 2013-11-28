import org.newdawn.slick.*;
import org.newdawn.slick.state.*;
import org.lwjgl.input.Mouse;
import java.awt.Font;

public class Difficulty extends BasicGameState {
	
	private TrueTypeFont fontD1;
	private TrueTypeFont fontD2;
	private Color colorEasyBtn = Color.green;
	private Color colorMediumBtn = Color.green;
	private Color colorHardBtn = Color.green;
	
	
	public Difficulty(int state){
		
	}
	
	public void init(GameContainer gc, StateBasedGame sbj) throws SlickException {
		Font fonts1 = new Font("Courier",Font.BOLD,30);
		fontD1 = new TrueTypeFont(fonts1, false);
		Font fonts2 = new Font("Courier",Font.BOLD,25);
		fontD2 = new TrueTypeFont(fonts2, false);
		gc.setShowFPS(false);
	}
	
	public void render(GameContainer gc, StateBasedGame sbj, Graphics g) throws SlickException {
		g.setColor(Color.white);
		g.setFont(fontD1);
		g.drawString("Choose Difficulty", 10, 40);
		g.setColor(colorEasyBtn);
		g.fillRoundRect(100, 100, 200, 70, 15);
		g.setColor(colorMediumBtn);
		g.fillRoundRect(100, 200, 200, 70, 15);
		g.setColor(colorHardBtn);
		g.fillRoundRect(100, 300, 200, 70, 15);
		g.setColor(Color.black);
		g.setFont(fontD2);
		g.drawString("Easy",135,115);
		g.drawString("Medium", 135, 215);
		g.drawString("Hard", 135, 315);
		g.setColor(Color.white);
		
	}
	
	public void update(GameContainer gc, StateBasedGame sbg, int delta) throws SlickException {
		
		int posiX = Mouse.getX();
		int posiY = Mouse.getY();
		
		if((posiX>100&&posiX<300)&&(posiY>430&&posiY<500)) {
			colorEasyBtn=Color.white;
			if(Mouse.isButtonDown(0)) {
				sbg.enterState(2);
			}
			
		}
		else {
			colorEasyBtn=Color.green;
		}
		if((posiX>100&&posiX<300)&&(posiY>330&&posiY<400)) {
			colorMediumBtn=Color.white;
			if(Mouse.isButtonDown(0)) {
				sbg.enterState(2);
			}
			
		}
		else {
			colorMediumBtn=Color.green;
		}
		if((posiX>100&&posiX<300)&&(posiY>230&&posiY<300)) {
			colorHardBtn=Color.white;
			if(Mouse.isButtonDown(0)){
				sbg.enterState(2);
			}
		}
		else {
			colorHardBtn=Color.green;
		}
	}
	
	public int getID() {
		return 1;
	}
	
	
	

}
