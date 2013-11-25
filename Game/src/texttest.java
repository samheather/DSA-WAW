import java.awt.Font;

import org.newdawn.slick.*;
import org.newdawn.slick.font.effects.ColorEffect;
import org.newdawn.slick.gui.TextField;
 
public class texttest extends BasicGame{
	    private TrueTypeFont font;
        private TextField text1;
        private TextField text2;
       
        public texttest() {
                super("Test");
        }

        public void init(GameContainer container) throws SlickException {
        	    Font awtFont = new Font("Courier",Font.BOLD,15);
    		    font = new TrueTypeFont(awtFont, false);
                text1 = new TextField(container, font, 50,50,100,25);
                text2 = new TextField(container, font, 155,50,100,25);
        }

        public void update(GameContainer gc, int delta) throws SlickException {
                System.out.println(text1.getText());
        }

        public void render(GameContainer gc, Graphics g) throws SlickException {
                text1.render(gc, g);
                text2.render(gc, g);
        }
        public static void main(String[] args) throws SlickException  {
                texttest g = new texttest();
                AppGameContainer gc = new AppGameContainer(g);
                gc.setDisplayMode(500, 500, false);
                gc.start();
        }
}