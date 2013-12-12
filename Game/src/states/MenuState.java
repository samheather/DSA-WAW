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
	private Image menu_background, play_button, quit_button, play_hover, quit_hover;
	
	
	

	public MenuState(int state) {

	}

	public void init(GameContainer gc, StateBasedGame sbg)
			throws SlickException {
		
		menu_background = new Image("res/graphics/menu_graphics/menu_screen.png");
		play_button = new Image("res/graphics/menu_graphics/play_button.png");
		play_hover = new Image("res/graphics/menu_graphics/play_hover.png");
		quit_button = new Image("res/graphics/menu_graphics/quit_button.png");
		quit_hover = new Image("res/graphics/menu_graphics/exit_hover.png");
		
		
		
		
		try{

			InputStream inputStream = ResourceLoader.getResourceAsStream("res/Virgo-01/virgo.ttf");
			Font awtFont= Font.createFont(Font.TRUETYPE_FONT, inputStream);
			awtFont = awtFont.deriveFont(30f);
			font = new TrueTypeFont(awtFont, false);
			
			
		}catch(Exception e){
			e.printStackTrace();
		}
	}

	public void render(GameContainer gc, StateBasedGame sbg, Graphics g)
			throws SlickException {
		g.setColor(Color.white);
		g.setFont(font);
		menu_background.draw(0,0);
		
		
		int posX = Mouse.getX();
		int posY = Mouse.getY();
		
		if ((posX > 439 && posX < 762) && (posY > 100 && posY < 350)){
			play_hover.draw(439,349);
		}
		else{
			play_button.draw(439,349);
		}
		
		if((posX > 1100 && posX < 1172) && (posY > 0 && posY < 100)){
			quit_hover.draw(1148,556);
		}
		else{
			quit_button.draw(1148,556);
		}

		g.setColor(Color.white);

	}

	public void update(GameContainer gc, StateBasedGame sbg, int delta) throws SlickException {
		
		int posX = Mouse.getX();
		int posY = Mouse.getY();
		
		// Mapping Mouse coords onto graphics coords
		posY = 600 - posY;
		if (Mouse.isButtonDown(0)) {
			System.out.println(posX);
			System.out.println(posY);
		}

		if ((posX > 439 && posX < 762) && (posY > 349 && posY < 439)) {
			play_hover.draw(439,349);
			if (Mouse.isButtonDown(0)) {
				sbg.enterState(1);
			}

		} 
		
		if ((posX > 1148 && posX < 1172) && (posY > 556 && posY < 582)) {
			if (Mouse.isButtonDown(0)) {
				System.exit(0);
			}

		} 
	}

	public int getID() {
		return 0;
	}

}
