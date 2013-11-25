import org.newdawn.slick.*;
import org.newdawn.slick.font.effects.ColorEffect;
import org.newdawn.slick.gui.TextField;
 
public class texttest extends BasicGame{
        private UnicodeFont uFont = null;
        private String fontPath = "C:\\Windows\\Fonts\\Arial.ttf";
        private TextField text1;
        private TextField text2;
       
        public texttest() {
                super("Test");
        }
        @Override
        public void init(GameContainer container) throws SlickException {
                uFont = new UnicodeFont(fontPath,13,false,false);
                uFont.addAsciiGlyphs();   //Add Glyphs
                uFont.getEffects().add(new ColorEffect(java.awt.Color.WHITE));
                uFont.loadGlyphs();  //Load Glyphs
                text1 = new TextField(container, uFont, 50,50,100,25);
                text2 = new TextField(container, uFont, 155,50,100,25);
        }
        @Override
        public void update(GameContainer gc, int delta) throws SlickException {
                System.out.println(text1.getText());
        }
        @Override
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