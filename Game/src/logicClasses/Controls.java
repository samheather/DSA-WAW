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
	private boolean selectingHeadingUsingTextBox; // Has the text box been reset?
	private boolean mouseHeldDownOnAltitudeButton, mouseHeldDownOnFlight, headingAlreadyChangedByMouse;
	private final int  MAXIMUMALTITUDE = 31000;
	private final int  MINIMUMALTITUDE = 26000;
	private Flight selectedFlight;
	private String text;
	private Image altitudeButton, changePlanButton;
	private int difficultyValueOfGame;
	
	
	// CONSTRUCTOR
	public Controls() {
		
		this.selectingHeadingUsingTextBox = false; 
		this.mouseHeldDownOnAltitudeButton=false;
		this.mouseHeldDownOnFlight = false;
		this.headingAlreadyChangedByMouse = false;
		this.selectedFlight = null;
		
		

	}


	// INIT
	public void init(GameContainer gc) throws SlickException {
		Font awtFont = new Font("Courier", Font.BOLD, 15);
		font = new TrueTypeFont(awtFont, false);
		this.turnLeftTextBox = new TextField(gc, font, 10, 145, 100, 23);
		this.headingControlTextBox = new TextField(gc, font, 10, 215, 100, 23);
		this.turnRightTextBox = new TextField(gc, font, 10, 285, 100, 23);
		this.turnLeftTextBox.setMaxLength(3);
		this.turnRightTextBox.setMaxLength(3);
		this.headingControlTextBox.setMaxLength(3);
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
				if(this.selectedFlight.getTargetAltitude() < MAXIMUMALTITUDE) {
					this.selectedFlight.setTargetAltitude(this.selectedFlight.getTargetAltitude()+1000);
				}
			}

			else if(posX>10&&posX<150&&posY<440&&posY>420&&Mouse.isButtonDown(0)) {
				if(this.selectedFlight.getTargetAltitude()> MINIMUMALTITUDE) {
					this.selectedFlight.setTargetAltitude(this.selectedFlight.getTargetAltitude()-1000);
				}
			}
			
	
		
	}
	
	public void changeModeByClickingOnFlight(Flight nearestFlight){
		
		
		if (this.selectedFlight.getFlightPlan().getChangingPlan() == true){
			nearestFlight.getFlightPlan().setChangingPlan(false);
		}
		else{
			nearestFlight.getFlightPlan().setChangingPlan(true);
		}
		
	}
	

	public void checkSelected(int pointX, int pointY, Airspace airspace ){
		
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
			if (this.selectedFlight.getFlightPlan().getDraggingWaypoint()){
				return;
			}
		}
		
		
	 
		
		// Working out nearest flight to click
		
		if(airspace.getListOfFlights().size()>=1){
			minimumDistanceBetweenFlightAndMouseClick = Math.sqrt(Math.pow(pointX-airspace.getListOfFlights().get(0).getX(), 2)+Math.pow(pointY-airspace.getListOfFlights().get(0).getY(), 2));
			nearestFlight = airspace.getListOfFlights().get(0);
			indexOfNearestFlightInAirspaceListOfFlights = 0;
			
			for (int i =0; i< airspace.getListOfFlights().size(); i++){
				if(Math.sqrt(Math.pow(pointX-airspace.getListOfFlights().get(i).getX(), 2)+Math.pow(pointY-airspace.getListOfFlights().get(i).getY(), 2)) < minimumDistanceBetweenFlightAndMouseClick){
					minimumDistanceBetweenFlightAndMouseClick = Math.sqrt(Math.pow(pointX-airspace.getListOfFlights().get(i).getX(), 2)+Math.pow(pointY-airspace.getListOfFlights().get(i).getY(), 2));
					nearestFlight = airspace.getListOfFlights().get(i);
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
				this.setSelectedFlight(nearestFlight);
				for (int i =0; i< airspace.getListOfFlights().size(); i++){
					if(i != indexOfNearestFlightInAirspaceListOfFlights){
						airspace.getListOfFlights().get(i).setSelected(false);
						airspace.getListOfFlights().get(i).getFlightPlan().setChangingPlan(false);
						this.turnLeftTextBox.setText("");
						this.turnRightTextBox.setText("");
					}
				}
			}
		}
	}
	
	public void giveHeadingWithMouse(int pointX, int pointY, Airspace airspace){
		
		
		double deltaX, deltaY;
		double distanceBetweenMouseAndPlane;
		
		// If mouse is being held down don't change selected flight. 
		if (this.headingAlreadyChangedByMouse){
			return;
		}
		else{
			this.headingAlreadyChangedByMouse = true;
		}
		
		
		distanceBetweenMouseAndPlane = Math.sqrt(Math.pow(pointX-this.selectedFlight.getX(), 2)+Math.pow(pointY-this.selectedFlight.getY(), 2));
		
		
		if (distanceBetweenMouseAndPlane < 50)
		{
			deltaY = pointY - this.selectedFlight.getY();
			deltaX = pointX - this.selectedFlight.getX();
			double angle = Math.toDegrees(Math.atan2(deltaY, deltaX));
			angle+=90;
			if (angle < 0) {
				angle += 360;
			}
			this.selectedFlight.giveHeading((int)angle);
		
		}
		
	}
	
	public void updateHeadingTextBox(Input input){
		boolean headingTextBoxHasFocus = this.headingControlTextBox.hasFocus();
		if (headingTextBoxHasFocus) {
			
			// If the user has just selected the textbox, clear the textbox 
			if (!this.selectingHeadingUsingTextBox) {
				this.selectingHeadingUsingTextBox = true;
				this.headingControlTextBox.setText("");
			}
			
			if (input.isKeyDown(Input.KEY_ENTER)) {
				this.text = this.headingControlTextBox.getText();
				this.text = this.text.replaceAll("\\D+", "");
				if (!this.text.isEmpty()) {
					this.selectedFlight.giveHeading(Integer.valueOf(this.text));
					this.headingControlTextBox.setFocus(false);

				}
				
			}
		}
		
		if (this.selectingHeadingUsingTextBox && !headingTextBoxHasFocus) {
			this.selectingHeadingUsingTextBox = false;
		}
	}
	
	public void updateTurnLeftTextBox(Input input){
		
		boolean turnLeftTextBoxHasFocus = this.turnLeftTextBox.hasFocus();
		if (turnLeftTextBoxHasFocus) {
			
			if (input.isKeyDown(Input.KEY_ENTER)) {
				this.text = this.turnLeftTextBox.getText();
				this.text = this.text.replaceAll("\\D+", "");
				if (!this.text.isEmpty() && Integer.valueOf(this.text) <= 360) {
					this.selectedFlight.turnFlightLeft(Integer.valueOf(this.text));
					this.turnLeftTextBox.setText("");
				}
				this.turnLeftTextBox.setFocus(false);
				
			}
		}
		else{
			this.turnLeftTextBox.setText("");
		}
		
	}
	
	public void updateTurnRightTextBox(Input input){
		
		if (this.turnRightTextBox.hasFocus()) {

			if (input.isKeyDown(Input.KEY_ENTER)) {
				this.text = this.turnRightTextBox.getText();
				this.text = this.text.replaceAll("\\D+", "");
				if (!this.text.isEmpty() && Integer.valueOf(this.text) <= 360) {
					this.selectedFlight.turnFlightRight(Integer.valueOf(this.text));
					this.turnRightTextBox.setText("");
				}
				this.turnRightTextBox.setFocus(false);
				
			}
		}
		else{
			this.turnRightTextBox.setText("");
		}

	}


	// RENDER AND UPDATE


	public void render(GameContainer gc, Graphics g) throws SlickException {
		if(this.selectedFlight != null) {
			if(!this.selectedFlight.getFlightPlan().getChangingPlan()){
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
				g.drawString("Target: "+this.selectedFlight.getTargetAltitude()+"Ft", 10, 360);
				if(this.selectedFlight.getTargetAltitude() != Math.round(31000)){
					g.drawString("Increase to "+ (this.selectedFlight.getTargetAltitude()+1000), 10, 390);
				}
				else {
					g.drawString("At max altitude", 10, 390);
				}
				if(this.selectedFlight.getTargetAltitude() != Math.round(26000)){
					g.drawString("Decrease to "+ (this.selectedFlight.getTargetAltitude()-1000), 10, 420);
				}
				else {
					g.drawString("At min altitude", 10, 420);
				}
				
				
				
				}
			changePlanButton.draw(0,45);
			changePlanButton.draw(0, 75);
			if(this.selectedFlight.getFlightPlan().getChangingPlan() == true){
				g.setColor(Color.white);
				g.drawString("Plan Mode", 10, 45);
				g.setColor(Color.lightGray);
				g.drawString("Navigator Mode", 10, 75);
			}
			else{
				g.setColor(Color.white);
				g.drawString("Navigator Mode", 10, 75);
				g.setColor(Color.lightGray);
				g.drawString("Plan Mode", 10, 45);
				
			}
		}	
	}
	
	
	
	
	public void update(GameContainer gc, Airspace airspace) {
		Input input = gc.getInput();
		int posX=Mouse.getX();
		int posY=Mouse.getY();
		posY = 600-Mouse.getY();

		if (this.selectedFlight != null ){
			
			// Only allow controls if user isn't changing a plan
			
			if(!(this.selectedFlight.getFlightPlan().getChangingPlan())){
				
				if(posX>10&&posX<150&&posY<65&&posY>45&&Mouse.isButtonDown(0)){
					this.selectedFlight.getFlightPlan().setChangingPlan(true);
				}
				
				if(Mouse.isButtonDown(1) && (this.difficultyValueOfGame != 3)){
					this.giveHeadingWithMouse(posX, posY, airspace);
				}

				this.updateHeadingTextBox(input);
				this.updateTurnLeftTextBox(input);
				this.updateTurnRightTextBox(input);
		
			
				
				// Handle and update Altitude Buttons
				
				this.handleAndUpdateAltitudeButtons();
				
				
				//ALTITUDE KEYS
				if(input.isKeyPressed(Input.KEY_UP)){
					if(this.selectedFlight.getTargetAltitude() < MAXIMUMALTITUDE) {
						this.selectedFlight.setTargetAltitude(this.selectedFlight.getTargetAltitude()+1000);
					}
				}
				if(input.isKeyPressed(Input.KEY_DOWN)){
					if(this.selectedFlight.getTargetAltitude() > MINIMUMALTITUDE) {
						this.selectedFlight.setTargetAltitude(this.selectedFlight.getTargetAltitude()-1000);
					}
				}
				
				
				if (!this.headingControlTextBox.hasFocus()) {
					this.getHeadingControlTB().setText(
							String.valueOf(Math.round(this.selectedFlight.getTargetHeading())));
				}
			
			}
			
			else{
				if(posX>10&&posX<150&&posY<95&&posY>75&&Mouse.isButtonDown(0)){
					this.selectedFlight.getFlightPlan().setChangingPlan(false);
				}
			}
		
		}
		
		if(Mouse.isButtonDown(0)){
			this.checkSelected(posX,posY,airspace);
			}
		
		if(!Mouse.isButtonDown(0)){
			this.mouseHeldDownOnFlight = false;
			this.mouseHeldDownOnAltitudeButton = false;
		}
		
		if (!Mouse.isButtonDown(1)){
			this.headingAlreadyChangedByMouse = false;
		}
		


	}
	
	//MUTATORS AND ACCESSORS

	

	public void setSelectedFlight(Flight flight1){
		this.selectedFlight = flight1;
	}


	
	public TextField getHeadingControlTB() {
		return headingControlTextBox;
	}

	
	public Flight getSelectedFlight(){
		return this.selectedFlight;
	}
	
	public void setDifficultyValueOfGame(int value){
		this.difficultyValueOfGame = value;
		
	}
}



