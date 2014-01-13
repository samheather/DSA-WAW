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
	private Image menuBackground, playButton, quitButton, playHover, quitHover, creditsButton, creditsHover;
	

	public MenuState(int state) {

	}

	public void init(GameContainer gc, StateBasedGame sbg)
			throws SlickException {
		
		menuBackground = new Image("res/graphics/menu_graphics/menu_screen.png");
		playButton = new Image("res/graphics/menu_graphics/play_button.png");
		playHover = new Image("res/graphics/menu_graphics/play_hover.png");
		quitButton = new Image("res/graphics/menu_graphics/quit_button.png");
		quitHover = new Image("res/graphics/menu_graphics/quit_hover.png");
		//credits screen access needs their own graphics assets, currently using placeholder
		//placeholder graphics has dimensions 116x46 pxls
		creditsButton = new Image("res/graphics/menu_graphics/menu_button.png");
		creditsHover = new Image("res/graphics/menu_graphics/menu_hover.png");
		
		
		
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
		menuBackground.draw(0,0);
		
		
		int posX = Mouse.getX();
		int posY = Mouse.getY();
		
		if ((posX > 439 && posX < 762) && (posY > 165 && posY < 255)){
			playHover.draw(439,349);
		}
		else{
			playButton.draw(439,349);
		}
		
		if((posX > 1140 && posX < 1262) && (posY > 25 && posY < 50)){
			quitHover.draw(1148,556);
		}
		else{
			quitButton.draw(1148,556);
		}
		
		if (posX>20 && posX< 136 && posY>20 && posY<66){
			creditsHover.draw(20,534);
		} else {
			creditsButton.draw(20,534);
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
			playHover.draw(439,349);
			if (Mouse.isButtonDown(0)) {
				sbg.enterState(1);
			}

		} 
		
		if ((posX > 1148 && posX < 1172) && (posY > 556 && posY < 582)) {
			if (Mouse.isButtonDown(0)) {
				System.exit(0);
			}
		}
		
		if( (posX>20 && posX< 136 && posY>534 && posY<580) && Mouse.isButtonDown(0)) {
			sbg.enterState(5);
		}
	}

	public int getID() {
		return 0;
	}

}
