package logicClasses;
import java.awt.Font;

import org.lwjgl.input.Mouse;
import org.newdawn.slick.Color;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
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
	private Image altitudeButton;
	
	
	// CONSTRUCTOR
	public Controls() {
		
	}


	// INIT
	public void init(GameContainer gc) throws SlickException {
		Font awtFont = new Font("Courier", Font.BOLD, 15);
		font = new TrueTypeFont(awtFont, false);

		this.turnLeftTB = new TextField(gc, font, 10, 70, 100, 23);
		this.headingControlTB = new TextField(gc, font, 10, 140, 100, 23);
		this.turnRightTB = new TextField(gc, font, 10, 210, 100, 23);
		this.headingHasFocus = false;
		this.turnLeftHasFocus = false;
		this.turnRightHasFocus = false;
		this.heading_cleared_this_focus = false;
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
		altitudeButton = new Image("res/graphics/graphics/altitudebutton.png");

	}
	
	

	// METHODS
	public void check_alt_buttons_clicked() {
		int posX=Mouse.getX();
		int posY=Mouse.getY();
		
		posY = 600 - posY; // Mapping Mouse coords onto graphic coords

		if(!this.mouse_pressed) {
			if(posX>10&&posX<150&&posY<340&&posY>320&&Mouse.isButtonDown(0)) {
				if(this.increase_alt<=31000) {
					this.increase_alt_clicked=true;
					this.mouse_pressed=true;
					this.min_alt=false;
				}
			}

			else if(posX>10&&posX<150&&posY<370&&posY>350&&Mouse.isButtonDown(0)) {
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



	public void clear_all() {
		this.headingControlTB.setAcceptingInput(false);
		this.turnLeftTB.setAcceptingInput(false);
		this.turnRightTB.setAcceptingInput(false);
	}

	public void allow_all() {
		this.headingControlTB.setAcceptingInput(true);
		this.turnLeftTB.setAcceptingInput(true);
		this.turnRightTB.setAcceptingInput(true);
	}
	

	// RENDER AND UPDATE


	public void render(GameContainer gc, Graphics g) throws SlickException {
		if(this.increase_alt!=0) {
			g.setColor(Color.white);
			
			g.drawString("Turn Left:", 10, 50);
			this.turnLeftTB.render(gc, g);
			g.drawString("DEG", 115, 70);
			
			g.drawString("Target Heading:", 10, 120);
			this.headingControlTB.render(gc, g);
			g.drawString("DEG", 115, 140);
			
			g.drawString("Turn Right:", 10, 190);
			this.turnRightTB.render(gc, g);
			g.drawString("DEG", 115, 210);
			
			
			g.drawString("Change Altitude:", 10, 260);
			g.setColor(Color.blue);
			altitudeButton.draw(0,320);
			altitudeButton.draw(0,350);
			g.setColor(Color.white);
			g.drawString("Target: "+this.target_alt+"Ft", 10, 290);
			if(!this.max_alt){
				g.drawString("Increase to "+this.increase_alt, 10, 320);
			}
			else {
				g.drawString("At max altitude", 10, 320);
			}
			if(!this.min_alt){
				g.drawString("Decrease to "+this.decrease_alt, 10, 350);
			}
			else {
				g.drawString("At min altitude", 10, 350);
			}
		}
	}
	
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
				if (!this.text.isEmpty() && Integer.valueOf(this.text) <= 360) {
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
				if (!this.text.isEmpty() && Integer.valueOf(this.text) <= 360) {
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
	

		// UPDATE SELECTED BOX
		switch (boxselected) {
		case 1:
			turnLeftTB.setFocus(true);
			break;
		case 2:
			headingControlTB.setFocus(true);
			break;
		case 3:
			turnRightTB.setFocus(true);
			break;
		}
		this.check_alt_buttons_clicked();
		
		
		//ALTITUDE KEYS
				if(input.isKeyPressed(Input.KEY_UP)){
					if(this.increase_alt<=31000) {
					this.increase_alt_clicked = true;
					}
				}
				if(input.isKeyPressed(Input.KEY_DOWN)){
					if(this.decrease_alt>=26000) {
					this.decrease_alt_clicked = true;
					}
				}
	}
	
	//MUTATORS AND ACCESSORS

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
	
	public void setFlight(Flight flight1){
		this.flight = flight1;
	}


	
	public TextField getHeadingControlTB() {
		return headingControlTB;
	}

	public boolean headingHasFocus() {
		return this.headingHasFocus;
	}
}
//test lock


