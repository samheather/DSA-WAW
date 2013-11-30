import org.newdawn.slick.*;
import org.newdawn.slick.state.*;
import org.lwjgl.input.Mouse;
import java.awt.Font;

public class Menu extends BasicGameState {
	private TrueTypeFont font;
	private TrueTypeFont font2;
	private Color colorBtn1 = Color.green;
	private Color colorBtn2 = Color.green;

	public Menu(int state) {

	}

	public void init(GameContainer gc, StateBasedGame sbj)
			throws SlickException {
		Font awtFont = new Font("Courier", Font.BOLD, 30);
		font = new TrueTypeFont(awtFont, false);
		Font awtFont2 = new Font("Courier", Font.BOLD, 25);
		font2 = new TrueTypeFont(awtFont2, false);
		gc.setShowFPS(false);
	}

	public void render(GameContainer gc, StateBasedGame sbj, Graphics g)
			throws SlickException {
		g.setColor(Color.white);
		g.setFont(font);
		g.drawString("Saving Flight Ryan", 350, 40);
		g.setColor(colorBtn1);
		g.fillRoundRect(470, 100, 200, 70, 15);
		g.setColor(colorBtn2);
		g.fillRoundRect(470, 200, 200, 70, 15);
		g.setColor(Color.black);
		g.setFont(font2);
		g.drawString("Play Game", 505, 115);
		g.drawString("Exit Game", 505, 215);
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
