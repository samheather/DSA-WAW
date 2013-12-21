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
	private TextField headingControlTextBox;
	private TextField turnRightTextBox;
	private TextField turnLeftTextBox;
	private boolean headingTextBoxHasFocus; // Is the text box currently selected?
	private boolean turnLeftTextBoxHasFocus; // Is the text box currently selected?
	private boolean turnRightTextBoxHasFocus; // Is the text box currently selected?
	private boolean focusOnHeadingTextBoxCleared; // Has the text box been reset?
	private boolean mouseHeldDownOnAltitudeButton, mouseHeldDownOnFlight;
	private boolean focusOnRightTextBoxCleared;
	private boolean focusOnLeftTextBoxCleared;
	private final int  MAXIMUM_ALTITUDE = 31000;
	private final int  MINIMUM_ALTITUDE = 26000;
	private Flight selectedFlight;
	private String text;
	private int altitudeToIncreaseTo, altitudeToDecreaseTo, targetAltitude;
	private Image altitudeButton, changePlanButton;
	
	
	// CONSTRUCTOR
	public Controls() {
		
		this.headingTextBoxHasFocus = false;
		this.turnLeftTextBoxHasFocus = false;
		this.turnRightTextBoxHasFocus = false;
		this.focusOnHeadingTextBoxCleared = false;
		this.focusOnLeftTextBoxCleared = false;
		this.focusOnRightTextBoxCleared = false;
		this.mouseHeldDownOnAltitudeButton=false;
		this.mouseHeldDownOnFlight = false;
		this.altitudeToIncreaseTo=0;
		this.altitudeToDecreaseTo=0;
		this.targetAltitude=0;
		this.selectedFlight = null;
		
		
	}


	// INIT
	public void init(GameContainer gc) throws SlickException {
		Font awtFont = new Font("Courier", Font.BOLD, 15);
		font = new TrueTypeFont(awtFont, false);
		this.turnLeftTextBox = new TextField(gc, font, 10, 145, 100, 23);
		this.headingControlTextBox = new TextField(gc, font, 10, 215, 100, 23);
		this.turnRightTextBox = new TextField(gc, font, 10, 285, 100, 23);
		altitudeButton = new Image("res/graphics/graphics/altitudebutton.png");
		changePlanButton = new Image("res/graphics/graphics/altitudebutton.png"); // same as altitude button
		
	}
	
	

	// METHODS
	public void handleAndUpdateAltitudeButtons() {
		
		if(this.mouseHeldDownOnAltitudeButton) {
			return;
		}
		else{
			this.mouseHeldDownOnAltitudeButton = true;
		}
		
		int posX=Mouse.getX();
		int posY=Mouse.getY();
		
		posY = 600 - posY; // Mapping Mouse coords onto graphic coords

		
			if(posX>10&&posX<150&&posY<410&&posY>390&&Mouse.isButtonDown(0)) {
				if(this.selectedFlight.getTarget_altitude() < MAXIMUM_ALTITUDE) {
					this.selectedFlight.setTarget_altitude(this.selectedFlight.getTarget_altitude()+1000);
				}
			}

			else if(posX>10&&posX<150&&posY<440&&posY>420&&Mouse.isButtonDown(0)) {
				if(this.selectedFlight.getTarget_altitude()> MINIMUM_ALTITUDE) {
					this.selectedFlight.setTarget_altitude(this.selectedFlight.getTarget_altitude()-1000);
				}
			}
			
			this.setIncrease_alt((int)Math.round(this.selectedFlight.getTarget_altitude())+1000);
			this.setDecrease_alt((int)Math.round(this.selectedFlight.getTarget_altitude())-1000);
			this.setTarget_alt((int)Math.round(this.selectedFlight.getTarget_altitude()));
	
		
	}
	
	public void changeModeByClickingOnFlight(Flight nearestFlight){
		
		
		if (this.selectedFlight.getFlight_plan().getChangingPlan() == true){
			nearestFlight.getFlight_plan().setChangingPlan(false);
		}
		else{
			nearestFlight.getFlight_plan().setChangingPlan(true);
		}
		
	}
	

	public void check_selected(int pointX, int pointY, Airspace airspace ){
		
		double minimumDistanceBetweenFlightAndMouseClick;
		Flight nearestFlight;
		int indexOfNearestFlightInAirspaceListOfFlights;
		
		// If mouse is being held down don't change selected flight. 
		if (this.mouseHeldDownOnFlight){
			return;
		}
		else{
			this.mouseHeldDownOnFlight = true;
		}
		
		// Checking if user is dragging a waypoint they can't change flights
		if (this.selectedFlight != null){
			if (this.selectedFlight.getFlight_plan().getDraggingWaypoint()){
				return;
			}
		}
		
		
	 
		
		// Working out nearest flight to click
		
		if(airspace.getList_of_flights().size()>=1){
			minimumDistanceBetweenFlightAndMouseClick = Math.sqrt(Math.pow(pointX-airspace.getList_of_flights().get(0).getX(), 2)+Math.pow(pointY-airspace.getList_of_flights().get(0).getY(), 2));
			nearestFlight = airspace.getList_of_flights().get(0);
			indexOfNearestFlightInAirspaceListOfFlights = 0;
			
			for (int i =0; i< airspace.getList_of_flights().size(); i++){
				if(Math.sqrt(Math.pow(pointX-airspace.getList_of_flights().get(i).getX(), 2)+Math.pow(pointY-airspace.getList_of_flights().get(i).getY(), 2)) < minimumDistanceBetweenFlightAndMouseClick){
					minimumDistanceBetweenFlightAndMouseClick = Math.sqrt(Math.pow(pointX-airspace.getList_of_flights().get(i).getX(), 2)+Math.pow(pointY-airspace.getList_of_flights().get(i).getY(), 2));
					nearestFlight = airspace.getList_of_flights().get(i);
					indexOfNearestFlightInAirspaceListOfFlights = i;
				}
			}
			
			// Working out whether the nearest flight to click is close enough
			// to be selected.
			
			if (minimumDistanceBetweenFlightAndMouseClick <= 50){
				
				if (nearestFlight == this.selectedFlight){
					this.changeModeByClickingOnFlight(nearestFlight);
				}
				
				nearestFlight.setSelected(true);
				this.setFlight(nearestFlight);
				for (int i =0; i< airspace.getList_of_flights().size(); i++){
					if(i != indexOfNearestFlightInAirspaceListOfFlights){
						airspace.getList_of_flights().get(i).setSelected(false);
						airspace.getList_of_flights().get(i).getFlight_plan().setChangingPlan(false);
					}
				}
			}
		}
	}
	
	public void give_heading_with_mouse(int pointX, int pointY, Airspace airspace){
		
		
		double deltaX, deltaY;
		double distance_between_mouse_and_plane;
		
		
		distance_between_mouse_and_plane = Math.sqrt(Math.pow(pointX-this.selectedFlight.getX(), 2)+Math.pow(pointY-this.selectedFlight.getY(), 2));
		
		
		if (distance_between_mouse_and_plane < 50)
		{
			deltaY = pointY - this.selectedFlight.getY();
			deltaX = pointX - this.selectedFlight.getX();
			double angle = Math.toDegrees(Math.atan2(deltaY, deltaX));
			angle+=90;
			if (angle < 0) {
				angle += 360;
			}
			this.selectedFlight.give_heading((int)angle);
		
		}
		
	}

//	public void allow_all() {
//		this.headingControlTB.setAcceptingInput(true);
//		this.turnLeftTB.setAcceptingInput(true);
//		this.turnRightTB.setAcceptingInput(true);
//	}
//	

	// RENDER AND UPDATE


	public void render(GameContainer gc, Graphics g) throws SlickException {
		if(this.selectedFlight != null) {
			if(!this.selectedFlight.getFlight_plan().getChangingPlan()){
				g.setColor(Color.white);
				
				g.drawString("Turn Left:", 10, 125);
				this.turnLeftTextBox.render(gc, g);
				g.drawString("DEG", 114, 153);
				
				g.drawString("Target Heading:", 10, 195);
				this.headingControlTextBox.render(gc, g);
				g.drawString("DEG", 114, 224);
				
				g.drawString("Turn Right:", 10, 265);
				this.turnRightTextBox.render(gc, g);
				g.drawString("DEG", 114, 294);
				
				
				g.drawString("Change Altitude:", 10, 330);
				g.setColor(Color.blue);
				altitudeButton.draw(0,390);
				altitudeButton.draw(0,420);
				g.setColor(Color.white);
				g.drawString("Target: "+this.targetAltitude+"Ft", 10, 360);
				if(this.selectedFlight.getTarget_altitude() != Math.round(31000)){
					g.drawString("Increase to "+this.altitudeToIncreaseTo, 10, 390);
				}
				else {
					g.drawString("At max altitude", 10, 390);
				}
				if(this.selectedFlight.getTarget_altitude() != Math.round(26000)){
					g.drawString("Decrease to "+this.altitudeToDecreaseTo, 10, 420);
				}
				else {
					g.drawString("At min altitude", 10, 420);
				}
				
				
				
				}
			changePlanButton.draw(0,45);
			changePlanButton.draw(0, 75);
			if(this.selectedFlight.getFlight_plan().getChangingPlan() == true){
				g.setColor(Color.yellow);
				g.drawString("Plan Mode", 10, 45);
				g.setColor(Color.white);
				g.drawString("Navigator Mode", 10, 75);
			}
			else{
				g.setColor(Color.yellow);
				g.drawString("Navigator Mode", 10, 75);
				g.setColor(Color.white);
				g.drawString("Plan Mode", 10, 45);
				
			}
		}	
	}
	
	public void updateHeadingTextBox(Input input){
		this.headingTextBoxHasFocus = this.headingControlTextBox.hasFocus();
		if (this.headingTextBoxHasFocus) {
			if (!this.focusOnHeadingTextBoxCleared) {
				this.focusOnHeadingTextBoxCleared = true;
				this.headingControlTextBox.setText("");
			}
			if (input.isKeyDown(Input.KEY_ENTER)) {
				this.text = this.headingControlTextBox.getText();
				this.text = this.text.replaceAll("\\D+", "");
				if (!this.text.isEmpty()) {
					this.selectedFlight.give_heading(Integer.valueOf(this.text));

				}
				this.headingControlTextBox.setFocus(false);
			}
		}
		if (this.focusOnHeadingTextBoxCleared && !this.headingTextBoxHasFocus) {
			this.focusOnHeadingTextBoxCleared = false;
		}
	}
	
	public void updateTurnLeftTextBox(Input input){
		this.turnLeftTextBoxHasFocus = this.turnLeftTextBox.hasFocus();
		if (this.turnLeftTextBoxHasFocus) {
			if (!this.focusOnLeftTextBoxCleared) {
				this.focusOnLeftTextBoxCleared = true;
				this.turnLeftTextBox.setText("");
			}
			if (input.isKeyDown(Input.KEY_ENTER)) {
				this.text = this.turnLeftTextBox.getText();
				this.text = this.text.replaceAll("\\D+", "");
				if (!this.text.isEmpty() && Integer.valueOf(this.text) <= 360) {
					this.selectedFlight.turn_flight_left(Integer.valueOf(this.text));
					this.turnLeftTextBox.setText("");
				}
				this.turnLeftTextBox.setFocus(false);
			}
		}
		if (this.focusOnLeftTextBoxCleared && !this.turnLeftTextBoxHasFocus) {
			this.focusOnLeftTextBoxCleared = false;
		}

	}
	
	public void updateTurnRightTextBox(Input input){
		this.turnRightTextBoxHasFocus = this.turnRightTextBox.hasFocus();
		if (this.turnRightTextBoxHasFocus) {
			if (!this.focusOnRightTextBoxCleared) {
				this.focusOnRightTextBoxCleared = true;
				this.turnRightTextBox.setText("");
			}
			if (input.isKeyDown(Input.KEY_ENTER)) {
				this.text = this.turnRightTextBox.getText();
				this.text = this.text.replaceAll("\\D+", "");
				if (!this.text.isEmpty() && Integer.valueOf(this.text) <= 360) {
					this.selectedFlight.turn_flight_right(Integer.valueOf(this.text));
					this.turnRightTextBox.setText("");
				}
				this.turnRightTextBox.setFocus(false);
			}
		}
		if (this.focusOnRightTextBoxCleared && !this.turnRightTextBoxHasFocus) {
			this.focusOnRightTextBoxCleared = false;
		}
	

	}
	
	
	
	public void update(GameContainer gc, Airspace airspace) {
		Input input = gc.getInput();
		int posX=Mouse.getX();
		int posY=Mouse.getY();
		posY = 600-Mouse.getY();

		if (this.selectedFlight != null ){
			
			// Only allow controls if user isn't changing a plan
			
			if(!(this.selectedFlight.getFlight_plan().getChangingPlan())){
				
				if(posX>10&&posX<150&&posY<65&&posY>45&&Mouse.isButtonDown(0)){
					this.selectedFlight.getFlight_plan().setChangingPlan(true);
				}
				
				if(Mouse.isButtonDown(1)){
					this.give_heading_with_mouse(posX, posY, airspace);
				}

				this.updateHeadingTextBox(input);
				this.updateTurnLeftTextBox(input);
				this.updateTurnRightTextBox(input);
		
			
				
				// Handle and update Altitude Buttons
				
				this.handleAndUpdateAltitudeButtons();
				
				
				//ALTITUDE KEYS
				if(input.isKeyPressed(Input.KEY_UP)){
					if(this.selectedFlight.getTarget_altitude() < MAXIMUM_ALTITUDE) {
						this.selectedFlight.setTarget_altitude(this.selectedFlight.getTarget_altitude()+1000);
					}
				}
				if(input.isKeyPressed(Input.KEY_DOWN)){
					if(this.selectedFlight.getTarget_altitude() > MINIMUM_ALTITUDE) {
						this.selectedFlight.setTarget_altitude(this.selectedFlight.getTarget_altitude()-1000);
					}
				}
				
				
				if (!this.headingHasFocus()) {
					this.getHeadingControlTB().setText(
							String.valueOf(Math.round(this.selectedFlight.getTarget_heading())));
				}
			
			}
			
			else{
				if(posX>10&&posX<150&&posY<95&&posY>75&&Mouse.isButtonDown(0)){
					this.selectedFlight.getFlight_plan().setChangingPlan(false);
				}
			}
		
		}
		
		if(Mouse.isButtonDown(0)){
			this.check_selected(posX,posY,airspace);
			}
		
		if(!Mouse.isButtonDown(0)){
			this.mouseHeldDownOnFlight = false;
			this.mouseHeldDownOnAltitudeButton = false;
		}

	}
	
	//MUTATORS AND ACCESSORS


	public int getTarget_alt() {
		return targetAltitude;
	}

	public void setTarget_alt(int target_alt) {
		this.targetAltitude = target_alt;
	}
	
	public int getIncrease_alt() {
		return altitudeToIncreaseTo;
	}

	public void setIncrease_alt(int increase_alt) {
		this.altitudeToIncreaseTo = increase_alt;
	}

	public int getDecrease_alt() {
		return altitudeToDecreaseTo;
	}

	public void setDecrease_alt(int decrease_alt) {
		this.altitudeToDecreaseTo = decrease_alt;
	}
	
	public void setFlight(Flight flight1){
		this.selectedFlight = flight1;
	}


	
	public TextField getHeadingControlTB() {
		return headingControlTextBox;
	}

	public boolean headingHasFocus() {
		return this.headingTextBoxHasFocus;
	}
	
	public Flight getFlight(){
		return this.selectedFlight;
	}
}
//test lock


