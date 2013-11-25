import java.awt.Font;
import org.newdawn.slick.Color;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.TrueTypeFont;
import org.newdawn.slick.gui.TextField;
import org.newdawn.slick.GameContainer;


public class Controls{
	
	private TrueTypeFont font;
	private TextField headingControlTB;
	private TextField altControlTB;
	
	public Controls(GameContainer gc){	
		
	}
	public void init(GameContainer gc) {
		Font awtFont = new Font("Courier",Font.BOLD,15);
		font = new TrueTypeFont(awtFont, false);
		this.headingControlTB = new TextField(gc,font,0,30,100,25);
		this.altControlTB = new TextField(gc,font,0,85,100,25);		
	}
	
	public void update(GameContainer gc){
			//System.out.println(this.headingControlTB.getText());
			//System.out.println(this.altControlTB.getText());
	}
	
	public void render(GameContainer gc, Graphics g)throws SlickException {
		g.setColor(Color.white);
		this.headingControlTB.render(gc,g);
		this.altControlTB.render(gc,g);
	}
	public void clear_all() {
		this.headingControlTB.setAcceptingInput(false);
		this.altControlTB.setAcceptingInput(false);
	}
	public void allow_all() {
		this.headingControlTB.setAcceptingInput(true);
		this.altControlTB.setAcceptingInput(true);
	}

}
