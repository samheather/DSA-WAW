import java.awt.Font;

import org.lwjgl.input.Mouse;
import org.newdawn.slick.Color;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Input;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.TrueTypeFont;
import org.newdawn.slick.gui.TextField;
import org.newdawn.slick.GameContainer;

public class Controls {

	// FIELDS
	private TrueTypeFont font;
	private TextField headingControlTB;
	//private TextField altControlTB;
	private TextField turnRightTB;
	private TextField turnLeftTB;
	private boolean headingHasFocus; // Is the text box currently selected?
	//private boolean altHasFocus; // Is the text box currently selected?
	private boolean turnLeftHasFocus; // Is the text box currently selected?
	private boolean turnRightHasFocus; // Is the text box currently selected?
	private boolean heading_cleared_this_focus; // Has the text box been reset?
	private boolean increase_alt_clicked,decrease_alt_clicked, mouse_pressed, max_alt, min_alt;
	//private boolean alt_cleared_this_focus; // Has the text box been reset?
	private boolean right_cleared_this_focus;
	private boolean left_cleared_this_focus;
	private Flight flight;
	private String text;
	private int boxselected, increase_alt, decrease_alt, target_alt;

	public int getIncrease_alt() {
		return increase_alt;
	}

	public void setIncrease_alt(int increase_alt) {
		this.increase_alt = increase_alt;
	}

	public int getDecrease_alt() {
		return decrease_alt;
	}

	public void setDecrease_alt(int decrease_alt) {
		this.decrease_alt = decrease_alt;
	}

	public Controls(GameContainer gc, Flight flight) {
		this.flight = flight;
	}

	// CONSTRUCTOR
	public void init(GameContainer gc) {
		Font awtFont = new Font("Courier", Font.BOLD, 15);
		font = new TrueTypeFont(awtFont, false);

		this.turnLeftTB = new TextField(gc, font, 10, 70, 100, 23);
		this.headingControlTB = new TextField(gc, font, 10, 170, 100, 23);
		//this.altControlTB = new TextField(gc, font, 10, 270, 100, 23);
		this.turnRightTB = new TextField(gc, font, 10, 370, 100, 23);
		this.headingHasFocus = false;
		//this.altHasFocus = false;
		this.turnLeftHasFocus = false;
		this.turnRightHasFocus = false;
		this.heading_cleared_this_focus = false;
		//this.alt_cleared_this_focus = false;
		this.left_cleared_this_focus = false;
		this.right_cleared_this_focus = false;
		this.increase_alt_clicked=false;
		this.decrease_alt_clicked=false;
		this.boxselected = 0;
		this.mouse_pressed=false;
		this.increase_alt=0;
		this.decrease_alt=0;
		this.max_alt=false;
		this.min_alt=false;
		this.target_alt=0;

	}
	
	

	// METHODS
	public void check_alt_buttons_clicked() {
		int posX=Mouse.getX();
		int posY=Mouse.getY();

		if(!this.mouse_pressed) {
			if(posX>10&&posX<130&&posY<300&&posY>280&&Mouse.isButtonDown(0)) {
				if(this.increase_alt<=31000) {
					this.increase_alt_clicked=true;
					this.mouse_pressed=true;
					this.min_alt=false;
				}
			}

			else if(posX>10&&posX<130&&posY<270&&posY>250&&Mouse.isButtonDown(0)) {
				if(this.decrease_alt>=26000) {
					this.mouse_pressed=true;
					this.decrease_alt_clicked=true;
					
					this.max_alt=false;
				}
			}
			else {
				this.increase_alt_clicked=false;
				this.decrease_alt_clicked=false;
			}
		}
		else {
			this.increase_alt_clicked=false;
			this.decrease_alt_clicked=false;
		}
		if(this.increase_alt>31000) {
			this.max_alt=true;
		}
		else {
			this.max_alt=false;
		}
		if(this.decrease_alt<26000) {
			this.min_alt=true;
		}
		else {
			this.min_alt=false;
		}
		if(!Mouse.isButtonDown(0)){
			this.mouse_pressed=false;
		}
	}


	public TextField getHeadingControlTB() {
		return headingControlTB;
	}

	public boolean headingHasFocus() {
		return this.headingHasFocus;
	}

	/*public boolean altHasFocus() {
		return this.altHasFocus;
	}*/

	/*public TextField getAltControlTB() {
		return altControlTB;
	}*/

	public void clear_all() {
		this.headingControlTB.setAcceptingInput(false);
		//this.altControlTB.setAcceptingInput(false);
		this.turnLeftTB.setAcceptingInput(false);
		this.turnRightTB.setAcceptingInput(false);
	}

	public void allow_all() {
		this.headingControlTB.setAcceptingInput(true);
		//this.altControlTB.setAcceptingInput(true);
		this.turnLeftTB.setAcceptingInput(true);
		this.turnRightTB.setAcceptingInput(true);
	}

	// UPDATE AND RENDER
	public void update(GameContainer gc) {
		Input input = gc.getInput();

		// Update Heading TextField
		this.headingHasFocus = this.headingControlTB.hasFocus();
		if (this.headingHasFocus) {
			if (!this.heading_cleared_this_focus) {
				this.heading_cleared_this_focus = true;
				this.headingControlTB.setText("");
			}
			if (input.isKeyDown(Input.KEY_ENTER)) {
				this.text = this.headingControlTB.getText();
				this.text = this.text.replaceAll("\\D+", "");
				if (!this.text.isEmpty()) {
					this.flight.give_heading(Integer.valueOf(this.text));

				}
				this.headingControlTB.setFocus(false);
			}
		}
		if (this.heading_cleared_this_focus && !this.headingHasFocus) {
			this.heading_cleared_this_focus = false;
		}

		// Update Altitude Text Field
		/*this.altHasFocus = this.altControlTB.hasFocus();
		if (this.altHasFocus) {
			if (!this.alt_cleared_this_focus) {
				this.alt_cleared_this_focus = true;
				this.altControlTB.setText("");
			}
			if (input.isKeyDown(Input.KEY_ENTER)) {
				this.text = this.altControlTB.getText();
				this.text = this.text.replaceAll("\\D+", "");
				if (!this.text.isEmpty()) {
					this.flight.setTarget_altitude(Double.valueOf(this.text));
				}
				this.altControlTB.setFocus(false);
			}
		}*/

		/*if (this.alt_cleared_this_focus && !this.altHasFocus) {
			this.alt_cleared_this_focus = false;
			this.text = this.altControlTB.getText();
			this.text = this.text.replaceAll("\\D+", "");
			if (!this.text.isEmpty()) {
				this.flight.setTarget_altitude(Double.valueOf(this.text));
			}
		}*/

		// Update Turn Left Text Field
		this.turnLeftHasFocus = this.turnLeftTB.hasFocus();
		if (this.turnLeftHasFocus) {
			if (!this.left_cleared_this_focus) {
				this.left_cleared_this_focus = true;
				this.turnLeftTB.setText("");
			}
			if (input.isKeyDown(Input.KEY_ENTER)) {
				this.text = this.turnLeftTB.getText();
				this.text = this.text.replaceAll("\\D+", "");
				if (!this.text.isEmpty()) {
					this.flight.turn_flight_left(Integer.valueOf(this.text));
					this.turnLeftTB.setText("");
				}
				this.turnLeftTB.setFocus(false);
			}
		}
		if (this.left_cleared_this_focus && !this.turnLeftHasFocus) {
			this.left_cleared_this_focus = false;
		}

		// Update Turn Right Text Field
		this.turnRightHasFocus = this.turnRightTB.hasFocus();
		if (this.turnRightHasFocus) {
			if (!this.right_cleared_this_focus) {
				this.right_cleared_this_focus = true;
				this.turnRightTB.setText("");
			}
			if (input.isKeyDown(Input.KEY_ENTER)) {
				this.text = this.turnRightTB.getText();
				this.text = this.text.replaceAll("\\D+", "");
				if (!this.text.isEmpty()) {
					this.flight.turn_flight_right(Integer.valueOf(this.text));
					this.turnRightTB.setText("");
				}
				this.turnRightTB.setFocus(false);
			}
		}
		if (this.right_cleared_this_focus && !this.turnRightHasFocus) {
			this.right_cleared_this_focus = false;
		}

		// CHECK KEYSTROKES
		if (input.isKeyPressed(Input.KEY_DOWN)) {
			if (boxselected == 4) {
				boxselected = 0;
			}
			this.boxselected++;
		}
		if (input.isKeyPressed(Input.KEY_UP)) {
			if (boxselected == 1) {
				boxselected = 5;
			}
			this.boxselected--;
		}

		// UPDATE SELECTED BOX
		switch (boxselected) {
		case 1:
			turnLeftTB.setFocus(true);
			break;
		case 2:
			headingControlTB.setFocus(true);
			break;
		case 3:
			//altControlTB.setFocus(true);
			break;
		case 4:
			turnRightTB.setFocus(true);
			break;
		}
		this.check_alt_buttons_clicked();
	}

	public void render(GameContainer gc, Graphics g) throws SlickException {
		g.setColor(Color.white);
		g.drawString("Turn Left:", 10, 50);
		this.turnLeftTB.render(gc, g);
		g.drawString("Target Heading:", 10, 150);
		this.headingControlTB.render(gc, g);
		g.drawString("Change Altitude:", 10, 250);
		//this.altControlTB.render(gc, g);
		g.drawString("Turn Right:", 10, 350);
		this.turnRightTB.render(gc, g);
		g.setColor(Color.blue);
		g.fillRect(10, 300, 130, 20);
		g.fillRect(10, 330, 130, 20);
		g.setColor(Color.white);
		g.drawString("Target: "+this.target_alt+"ft", 15, 270);
		if(!this.max_alt){
			g.drawString("Increase to "+this.increase_alt, 15, 300);
		}
		else {
			g.drawString("At max altitude", 15, 300);
		}
		if(!this.min_alt){
			g.drawString("Decrease to "+this.decrease_alt, 15, 330);
		}
		else {
			g.drawString("At min altitude", 15, 330);
		}
		
		

	}

	public boolean isIncrease_alt_clicked() {
		return increase_alt_clicked;
	}

	public void setIncrease_alt_clicked(boolean increase_alt_clicked) {
		this.increase_alt_clicked = increase_alt_clicked;
	}

	public boolean isDecrease_alt_clicked() {
		return decrease_alt_clicked;
	}

	public void setDecrease_alt_clicked(boolean decrease_alt_clicked) {
		this.decrease_alt_clicked = decrease_alt_clicked;
	}

	public int getTarget_alt() {
		return target_alt;
	}

	public void setTarget_alt(int target_alt) {
		this.target_alt = target_alt;
	}
}
