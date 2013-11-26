import java.awt.Font;
import org.newdawn.slick.Color;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Input;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.TrueTypeFont;
import org.newdawn.slick.gui.TextField;
import org.newdawn.slick.GameContainer;


public class Controls{
	
	//FIELDS
	private TrueTypeFont font; 
	private TextField headingControlTB;
	private TextField altControlTB;
	private boolean headingHasFocus; //Is the textbox currently selected?
	private boolean altHasFocus; //Is the textbox currently selected?
	private boolean heading_cleared_this_focus; //Has the focus been reset?
	private boolean alt_cleared_this_focus; //Has the focus been reset?
	private Flight flight;
	private String text;
	
	public Controls(GameContainer gc,Flight flight){
		this.flight = flight;
	}
	
	//CONSTRUCTOR
	public void init(GameContainer gc) {
		Font awtFont = new Font("Courier",Font.BOLD,15);
		font = new TrueTypeFont(awtFont, false);
		this.headingControlTB = new TextField(gc,font,0,30,100,25);
		this.altControlTB = new TextField(gc,font,0,85,100,25);	
		this.headingHasFocus = false;
		this.altHasFocus = false;
		this.heading_cleared_this_focus=false;
	}
	

	public void update(GameContainer gc){
		Input input = gc.getInput();
		this.headingHasFocus = this.headingControlTB.hasFocus();
		if(this.headingHasFocus) {
			if(!this.heading_cleared_this_focus){
				this.heading_cleared_this_focus=true;
				this.headingControlTB.setText("");
			}
			if(input.isKeyDown(Input.KEY_ENTER)){
				this.text = this.headingControlTB.getText();
				this.text = this.text.replaceAll("\\D+","");
				if (!this.text.isEmpty()){
					this.flight.give_heading(Integer.valueOf(this.text));
				}
				this.headingControlTB.setFocus(false);
				
			}
		}
		
		if(this.heading_cleared_this_focus && !this.headingHasFocus) {
			this.heading_cleared_this_focus=false;
			this.text = this.headingControlTB.getText();
			this.text = this.text.replaceAll("\\D+","");
			if (!this.text.isEmpty()){
				this.flight.give_heading(Integer.valueOf(this.text));
			}
			
		}	
		this.altHasFocus = this.altControlTB.hasFocus();

		if(this.altHasFocus){
			if(!this.alt_cleared_this_focus){
				this.alt_cleared_this_focus=true;
				this.altControlTB.setText("");
			}
			if(input.isKeyDown(Input.KEY_ENTER)){
				this.text = this.altControlTB.getText();
				this.text = this.text.replaceAll("\\D+","");
				if (!this.text.isEmpty()){
					this.flight.setTarget_altitude(Double.valueOf(this.text));
				}
				this.altControlTB.setFocus(false);
			}

		}

	//METHODS
		if(this.alt_cleared_this_focus && !this.altHasFocus) {
			this.alt_cleared_this_focus=false;
			this.text = this.altControlTB.getText();
			this.text = this.text.replaceAll("\\D+","");
			if (!this.text.isEmpty()){
				this.flight.setTarget_altitude(Double.valueOf(this.text));
			}

		}
		
	}
		
	
	public TextField getHeadingControlTB() {
		return headingControlTB;
	}
	public boolean headingHasFocus() {
		return this.headingHasFocus;
	}
	public boolean altHasFocus(){
		return this.altHasFocus;
	}
	public TextField getAltControlTB() {
		return altControlTB;
	}
	public void render(GameContainer gc, Graphics g)throws SlickException {
		g.setColor(Color.white);
		g.drawString("Heading:", 0,10);
		this.headingControlTB.render(gc,g);
		g.drawString("Altitude:", 0, 60);
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

	//UPDATE AND RENDER
	
	
}
