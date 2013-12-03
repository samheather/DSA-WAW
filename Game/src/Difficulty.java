import org.newdawn.slick.*;
import org.newdawn.slick.state.*;
import org.newdawn.slick.util.ResourceLoader;
import org.lwjgl.input.Mouse;
import java.awt.Font;
import java.io.InputStream;

public class Difficulty extends BasicGameState {
	
	private TrueTypeFont font;
	private Color colorEasyBtn = Color.green;
	private Color colorMediumBtn = Color.green;
	private Color colorHardBtn = Color.green;
	private int difficultyVal = 1; //Ram - Setting this screen to return a value to an 
								   // Object of Separation Rules to select a set of game over conditions
								   // Default it easy.
	
	public Difficulty(int state){
		
	}
	
	//Method to return difficultyVal to airspace.
	public int getDifficulty() {
		return difficultyVal;
	}
	
	public void init(GameContainer gc, StateBasedGame sbj) throws SlickException {
		try{
			InputStream inputStream = ResourceLoader.getResourceAsStream("res/Virgo-01/virgo.ttf");
			Font awtFont= Font.createFont(Font.TRUETYPE_FONT, inputStream);
			awtFont = awtFont.deriveFont(20f);
			font = new TrueTypeFont(awtFont, false);
			
			
		}catch(Exception e){
			e.printStackTrace();
		}
	}
	
	public void render(GameContainer gc, StateBasedGame sbj, Graphics g) throws SlickException {
		g.setColor(Color.white);
		g.setFont(font);
		g.drawString("Choose Difficulty", 10, 40);
		g.setColor(colorEasyBtn);
		g.fillRoundRect(100, 100, 200, 70, 15);
		g.setColor(colorMediumBtn);
		g.fillRoundRect(100, 200, 200, 70, 15);
		g.setColor(colorHardBtn);
		g.fillRoundRect(100, 300, 200, 70, 15);
		g.setColor(Color.black);
		g.setFont(font);
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
				difficultyVal = 3; // Button labelled Hard sets game to use Hard SeperationRules set.
				sbg.enterState(2);
			}
			
		}
		else {
			colorEasyBtn=Color.green;
		}
		if((posiX>100&&posiX<300)&&(posiY>330&&posiY<400)) {
			colorMediumBtn=Color.white;
			if(Mouse.isButtonDown(0)) {
				difficultyVal = 2; // Medium SeperationRules set.
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
