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
	private boolean headingSelected;
	private boolean altSelected;
	
	public Controls(GameContainer gc){
		this.heading = headingControl();
		this.altitude = altControl();	
		this.headingSelected = false;
		this.altSelected = false;
		Font awtFont = new Font("Courier",Font.BOLD,15);
		font = new TrueTypeFont(awtFont, false);
		this.headingControlTB = new TextField(gc,font,0,30,100,25);
		this.altControlTB = new TextField(gc,font,0,85,100,25);	
		
	}

	public int altControl() {
		return 0;
	}


	public double headingControl() {
			return 0.1;
	}
	
	public void update(GameContainer gc){
		int posX=Mouse.getX();
		int posY=Mouse.getY();

		if((posX>0&&posX<100)&&(posY>30&&posY<55)) {
			this.headingSelected = true;
		}
		else{
			this.headingSelected = false;
		}
	
		if(this.headingSelected == true){
			this.headingControlTB.setFocus(true);
			System.out.println(this.headingControlTB.getText());
		}
	}
	
	public void render(GameContainer gc, Graphics g)throws SlickException {
		g.setColor(Color.white);
		this.headingControlTB.render(gc,g);
		this.altControlTB.render(gc,g);
	}

}
