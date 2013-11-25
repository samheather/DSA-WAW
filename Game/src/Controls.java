import java.awt.Font;
import org.lwjgl.input.Mouse;
import org.newdawn.slick.Color;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.TrueTypeFont;
import org.newdawn.slick.gui.TextField;
import org.newdawn.slick.GameContainer;


public class Controls{
	
	
	private double heading;
	private int altitude;
	private TrueTypeFont font;
	private TextField headingControlTB;
	private TextField altControlTB;
	private boolean selected;
	
	public Controls(GameContainer gc){
		this.heading = headingControl();
		this.altitude = altControl();	
		this.selected = false;
		Font awtFont = new Font("Courier",Font.BOLD,15);
		font = new TrueTypeFont(awtFont, false);
		headingControlTB = new TextField(gc,font,0,30,100,25);
		altControlTB = new TextField(gc,font,0,85,100,25);	
	}

	public int altControl() {
		return 0;
	}


	public double headingControl() {
			return 0.1;
	}
	
	public void update(){
		int posX=Mouse.getX();
		int posY=Mouse.getY();
		if (this.selected == false);
		
	}
	

	public String getHeading(){
		headingControlTB.setFocus(true);
		return headingControlTB.getText();
		
	}
	
	public String getAlt(){
		return altControlTB.getText();
	}
	
	public void render(GameContainer gc, Graphics g)throws SlickException {
		g.setColor(Color.white);
		headingControlTB.render(gc,g);
		altControlTB.render(gc,g);
	}

}
