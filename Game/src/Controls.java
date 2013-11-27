import java.awt.Font;
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
	private TextField altControlTB;
	private TextField turnRightTB;
	private TextField turnLeftTB;
	private boolean headingHasFocus; // Is the text box currently selected?
	private boolean altHasFocus; // Is the text box currently selected?
	private boolean turnLeftHasFocus; // Is the text box currently selected?
	private boolean turnRightHasFocus; // Is the text box currently selected?
	private boolean heading_cleared_this_focus; // Has the text box been reset?
	private boolean alt_cleared_this_focus; // Has the text box been reset?
	private boolean right_cleared_this_focus;
	private boolean left_cleared_this_focus;
	private Flight flight;
	private String text;

	public Controls(GameContainer gc, Flight flight) {
		this.flight = flight;
	}

	// CONSTRUCTOR
	public void init(GameContainer gc) {
		Font awtFont = new Font("Courier", Font.BOLD, 15);
		font = new TrueTypeFont(awtFont, false);
		this.headingControlTB = new TextField(gc, font, 0, 30, 100, 25);
		this.altControlTB = new TextField(gc, font, 0, 85, 100, 25);
		this.turnLeftTB = new TextField(gc, font, 0, 140, 100, 25);
		this.turnRightTB = new TextField(gc, font, 0, 195, 100, 25);
		this.headingHasFocus = false;
		this.altHasFocus = false;
		this.turnLeftHasFocus = false;
		this.turnRightHasFocus = false;
		this.heading_cleared_this_focus = false;
		this.alt_cleared_this_focus = false;
		this.left_cleared_this_focus = false;
		this.right_cleared_this_focus = false;

	}

	// METHODS

	public TextField getHeadingControlTB() {
		return headingControlTB;
	}

	public boolean headingHasFocus() {
		return this.headingHasFocus;
	}

	public boolean altHasFocus() {
		return this.altHasFocus;
	}

	public TextField getAltControlTB() {
		return altControlTB;
	}

	public void clear_all() {
		this.headingControlTB.setAcceptingInput(false);
		this.altControlTB.setAcceptingInput(false);
		this.turnLeftTB.setAcceptingInput(false);
		this.turnRightTB.setAcceptingInput(false);
	}

	public void allow_all() {
		this.headingControlTB.setAcceptingInput(true);
		this.altControlTB.setAcceptingInput(true);
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
		this.altHasFocus = this.altControlTB.hasFocus();
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
		}

		if (this.alt_cleared_this_focus && !this.altHasFocus) {
			this.alt_cleared_this_focus = false;
			this.text = this.altControlTB.getText();
			this.text = this.text.replaceAll("\\D+", "");
			if (!this.text.isEmpty()) {
				this.flight.setTarget_altitude(Double.valueOf(this.text));
			}
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
	}

	public void render(GameContainer gc, Graphics g) throws SlickException {
		g.setColor(Color.white);
		g.drawString("Heading:", 0, 10);
		this.headingControlTB.render(gc, g);
		g.drawString("Altitude:", 0, 60);
		this.altControlTB.render(gc, g);
		g.drawString("Turn Left:", 0, 110);
		this.turnLeftTB.render(gc, g);
		g.drawString("Turn Right:", 0, 160);
		this.turnRightTB.render(gc, g);
	}
}
