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
	private Color colorBtn1 = Color.green;
	private Color colorBtn2 = Color.green;

	public MenuState(int state) {

	}

	public void init(GameContainer gc, StateBasedGame sbj)
			throws SlickException {
		
		try{

			InputStream inputStream = ResourceLoader.getResourceAsStream("res/Virgo-01/virgo.ttf");
			Font awtFont= Font.createFont(Font.TRUETYPE_FONT, inputStream);
			awtFont = awtFont.deriveFont(30f);
			font = new TrueTypeFont(awtFont, false);
			
			
		}catch(Exception e){
			e.printStackTrace();
		}
	}

	public void render(GameContainer gc, StateBasedGame sbj, Graphics g)
			throws SlickException {
		g.setColor(Color.white);
		g.setFont(font);
		g.drawString("Turbulence", 470, 40);
		g.setColor(colorBtn1);
		g.fillRoundRect(470, 100, 200, 70, 15);
		g.setColor(colorBtn2);
		g.fillRoundRect(470, 200, 200, 70, 15);
		g.setColor(Color.black);
		g.drawString("Play Game", 490, 115);
		g.drawString("Exit Game", 490, 215);
		g.setColor(Color.white);

	}

	public void update(GameContainer gc, StateBasedGame sbg, int delta) throws SlickException {
		
		int posX = Mouse.getX();
		int posY = Mouse.getY();

		if ((posX > 470 && posX < 670) && (posY > 430 && posY < 500)) {
			colorBtn1 = Color.white;
			if (Mouse.isButtonDown(0)) {
				sbg.enterState(1);
			}

		} else {
			colorBtn1 = Color.green;
		}
		if ((posX > 470 && posX < 670) && (posY > 330 && posY < 400)) {
			colorBtn2 = Color.white;
			if (Mouse.isButtonDown(0)) {
				System.exit(0);
			}

		} else {
			colorBtn2 = Color.green;
		}
	}

	public int getID() {
		return 0;
	}

}
