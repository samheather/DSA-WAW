package logicClasses;
import java.util.Random;

import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;

public class Flight {

	// FIELDS
	private double x, y, currentHeading, targetHeading;
	private int currentAltitude, targetAltitude, flightNumber;
	private boolean turningRight, turningLeft;
	private String flightName;
	private FlightPlan flightPlan;
	private Image regularFlightImage, selectedFlightInformationBackgroundImage, slowFlightImage, fastFlightImage, shadowImage;
	private boolean selected;
	private Airspace airspace;

	
	

	// CONSTRUCTOR
	public Flight(Airspace airspace) {
		this.x = 0;
		this.y = 0;
		this.targetAltitude = 0;
		this.currentAltitude = generateAltitude();
		this.targetHeading = 0;
		this.currentHeading = 0;
		this.turningRight = false;
		this.turningLeft = false;
		this.airspace = airspace;
		this.flightPlan = new FlightPlan(airspace, this);
		this.selected = false;
		

	}

	// METHODS
	
	
	/**
	 * generateAltitude: Randomly assigns one of three different altitudes to a flight
	 * @return A random altitude (either 28000, 29000 or 30000)
	 */

	public int generateAltitude() {
		Random rand = new Random();
		int check = rand.nextInt(3);
		switch(check) {
		case 0:
			return 28000;
		case 1:
			return 29000;
		case 2:
			return 30000;
		}
		return 27000;
	}

/**
 * calculateHeadingToFirstWaypoint: calculates heading between flight's current position and the first waypoint
 * in the flight's plan. The flight's current position will always be it's entrypoint because this method
 * is only called within the newFlight() function in airspace.
 * @param desX - The X coordinate of the waypoint
 * @param dexY - The Y coordinate of the waypoint
 * @return The heading between the flight and first waypoint.
 */
	
	public double calculateHeadingToFirstWaypoint(double desX, double desY) {
		double deltaX;
		double deltaY;
		deltaY = desY - this.y;
		deltaX = desX - this.x;
		double angle = Math.toDegrees(Math.atan2(deltaY, deltaX));
		angle += 90;
		if (angle < 0) {
			angle += 360;
		}
		return angle;
	}
	
	/**
	 * turnFlightLeft: sets the target heading to certain amount to to the left of the flight's current heading. 
	 * When an angle is entered in the textfields found in Controls, the value is passed to turnFlightLeft. The
	 * turningLeft boolean is set in order to tell the updateCurrentHeading() method that the flight should turn left 
	 * towards it's target heading.
	 * @param degreeTurnedBy - The amount of degrees you want to turn left by.
	 */
	
	public void turnFlightLeft(int degreeTurnedBy) {

		this.turningRight = false;
		this.turningLeft = true;

		this.targetHeading = Math.round(this.currentHeading) - degreeTurnedBy;
		if(this.targetHeading < 0){
			this.targetHeading = 360 +this.targetHeading;
		}
	}
	
	/**
	 * turnFlightRight: sets the target heading to certain amount to to the right of the flight's current heading. 
	 * When an angle is entered in the textfields found in Controls, the value is passed to turnFlightRight. The
	 * turningRight boolean is set in order to tell the updateCurrentHeading() method that the flight should turn right
	 * towards it's target heading.
	 * @param degreeTurnedBy - The amount of degrees you want to turn right by.
	 */
	
	public void turnFlightRight(int degreeTurnedBy) {

		this.turningLeft = false;
		this.turningRight = true;
		
		this.targetHeading = Math.round(this.currentHeading) + degreeTurnedBy;
		if(this.targetHeading >= 360){
			this.targetHeading = this.targetHeading - 360;
		}


	}
	
	/**
	 * giveHeading: Changes the target heading to newHeading. Whenever a command is issued by the user to change the heading,
	 * the method is passed the value of that command. The heading is always adjusted to a value between 0 and 359. This is 
	 * done using newHeading % 360.
	 * @param newHeading - The heading the flight has been commmanded to fly at.
	 */

	public void giveHeading(int newHeading) {
		this.turningRight = false;
		this.turningLeft = false;
		newHeading = newHeading % 360;
		this.targetHeading = newHeading;
	}
	
	/**
	 * checkIfFlightAtWaypoint: checks whether a flight is close enough to the next waypoint in it's plan
	 * for it to be considered at that waypoint.
	 * @param Waypoint - The next waypoint in the flight's plan.
	 * @return True if flight is at it's next waypoint.
	 */
	
	public boolean checkIfFlightAtWaypoint(Point waypoint) {
		
		if (((Math.abs(Math.round(this.x) - Math.round(waypoint.getX()))) <= 15)
				&& (Math.abs(Math.round(this.y) - Math.round(waypoint.getY()))) <= 15) {
			return true;
		}

		return false;
	}

	
	// DRAWING METHODS
	
	/**
	 * drawFlight: draws the flight at it's current x,y and draws its information around within a circle.
	 * Different images for the flight are used depending on how fast the plane is.
	 * @param g - Graphics libraries required by slick2d.
	 * @param gc - GameContainer required by slick2d.
	 */
	
	public void drawFlight(Graphics g, GameContainer gc ){

				g.setColor(Color.white);
				g.setWorldClip(150, 0, 1200, 600);

				//Scale the shadow in accordance to the altitude of the flight
				float shadowScale = (float) (36 - (this.currentAltitude / 1000))/10;
				shadowImage.setRotation((int) currentHeading);
				shadowImage.draw((int) this.x-35, (int) this.y, shadowScale);
				
				//Depending on a plane's speed, different images for the plane are drawn
					
				if(this.flightPlan.getVelocity() <= 275){
					
					slowFlightImage.setRotation((int) currentHeading);
					slowFlightImage.draw((int) this.x-10, (int) this.y-10);
					
				}
				
				else if(this.flightPlan.getVelocity() > 270 && this.flightPlan.getVelocity() < 340){
					
					regularFlightImage.setRotation((int) currentHeading);
					regularFlightImage.draw((int) this.x-10, (int) this.y-10);
			
				}
				
				else{
					fastFlightImage.setRotation((int) currentHeading);
					fastFlightImage.draw((int) this.x-10, (int) this.y-10);
					
				}
				
				// Drawing Separation Circle
				
				g.drawOval((int) this.x - 50, (int) this.y - 50, 100, 100);
				
				
				// Drawing information around flight
				
				// If flight is selected then also display current heading
				
				if (this.selected){
					g.setColor(Color.white);
					g.drawString(this.flightName, (int) this.x-24, (int) this.y-44);
					g.drawString(Math.round(this.currentAltitude) + " ft",(int) this.x-30, (int) this.y + 10);
					g.drawString(Math.round(this.currentHeading) + "°",(int) this.x - 13, (int) this.y + 25);//-15,20
					
					if (this.flightPlan.getWaypoints().size() > 0) {
						g.drawString("Aim: "+this.flightPlan.getPointByIndex(0).getPointRef(),(int) this.x -22, (int)this.y-28);
						
					}
	
				}
				
				// If flight isn't selected then don't display current heading
				else{
					g.setColor(Color.lightGray);
					g.drawString(this.flightName, (int) this.x-24, (int) this.y-44);
					g.drawString(Math.round(this.currentAltitude) + " ft",(int) this.x-30, (int) this.y + 10);
					
					if (this.flightPlan.getWaypoints().size() > 0) {
						g.drawString("Aim: "+this.flightPlan.getPointByIndex(0).getPointRef(),(int) this.x -22, (int)this.y-28);
					}
					g.drawOval((int) this.x - 50, (int) this.y - 50, 100, 100);
				}
				
				g.setWorldClip(0, 0, 1200, 600);
		
	}
	
	/**
	 * drawSelectedFlightInformation: draws the selected flight's information in the bottom left hand corner.
	 * @param g - Graphics libraries required by slick2d.
	 * @param gc - GameContainer required by slick2d.
	 */
	
	public void drawSelectedFlightInformation(Graphics g, GameContainer gc) {
		
		this.selectedFlightInformationBackgroundImage.draw(0,450);
		g.setColor(Color.white);
		g.drawString(this.flightName,  10, 460);
		g.drawString("Plan: ",  10, 480);
		String plan = "";
		
		for(int i=0; i<this.flightPlan.getWaypoints().size(); i++) {
			plan += this.flightPlan.getWaypoints().get(i).getPointRef()+", ";
		}
		
		
		g.setColor(Color.white);
		g.drawString(plan, 10, 500);
		g.drawString(Math.round(this.currentAltitude) + " Ft",
			 10, 520);
		g.drawString(Math.round(this.currentHeading) + " DEG",
			10, 540);
		g.drawString(Math.round(this.getFlightPlan().getVelocity()) + " MPH",
			10, 560);
		
	}
	
	// UPDATE METHODS
	
	/**
	 * updateXYCoordinates: updates the x and y values of the plane depending on it's velocity 
	 * and it's current heading. The velocity of the plane is scaled so that it can be used for 
	 * movement in terms of pixels.
	 */

	public void updateXYCoordinates() {
		double velocity = (this.flightPlan.getVelocity()) / 1000;

		this.x += velocity * Math.sin(Math.toRadians(this.currentHeading));

		this.y -= velocity * Math.cos(Math.toRadians(this.currentHeading));

	}

	/**
	 * updateAltitude(): If the target altitude is higher than the current altitude, increase current altitude.
	 * If target altitude is less than current altitude, decrease current altitude. If current altitude and
	 * target altitude are the same, do nothing.
	 */
	
	public void updateAltitude() {
		if (this.currentAltitude > this.targetAltitude) {
			this.currentAltitude -= 1;
		}

		else if (this.currentAltitude < this.targetAltitude) {
			this.currentAltitude += 1;
		}
	}
	
	/**
	 * updateCurrentHeading(): Moves the current heading towards the target heading. If a user has issued
	 * a heading but not specified what way to turn, this method will determine what way it would be quicker 
	 * to turn towards it's target heading.
	 */

	public void updateCurrentHeading() {
	
		double rate = 0.5;
		if (Math.round(this.targetHeading) != Math.round(this.currentHeading)) {
			

			// If plane has been given a heading so no turning direction specified
			// Below works out whether it should turn left or right to that heading.
			if(this.turningRight == false && this.turningLeft == false){

				if (Math.abs(this.targetHeading - this.currentHeading) == 180) {
					this.turningRight = true;
				} 
				
				else if (this.currentHeading + 180 <= 359){
					
					if (this.targetHeading < this.currentHeading + 180 && this.targetHeading > this.currentHeading){
						this.turningRight = true;
					}
					else {
						this.turningLeft = true;
					}
				}
				
				else {
					
					if (this.targetHeading > this.currentHeading - 180 && this.targetHeading < this.currentHeading){
						this.turningLeft = true;
					}
					else {
						this.turningRight = true;
					}
				}

			}
			
			// If plane is already turning right or user has told it to turn right
			
			if (this.turningRight == true) {
				this.currentHeading += rate;
				if (Math.round(this.currentHeading) >= 360 && this.targetHeading != 360) {
					this.currentHeading = 0;
				}
			}

			// if plane is already turning left or user has told it to turn left
			if (this.turningLeft == true) {
				this.currentHeading -= rate;
				if (Math.round(this.currentHeading) <= 0 && this.targetHeading != 0) {
					this.currentHeading = 360;
				}
			}
		}
	}
	
	


	// UPDATE, RENDER, INIT
	
	/**
	 * init: initialises resources such as images.
	 * @param gc - GameContainer required by slick2d.
	 */
	
	public void init(GameContainer gc) throws SlickException {
		this.regularFlightImage = new Image("res/graphics/flight.png");
		this.shadowImage = new Image("res/graphics/flight_shadow.png");
		this.slowFlightImage = new Image("res/graphics/flight_slow.png");
		this.fastFlightImage = new Image("res/graphics/flight_fast.png");
		this.selectedFlightInformationBackgroundImage = new Image("res/graphics/selected_flight2.jpg");

	}
	
	
/**
 * Update: calls all the update functions.
 */

	public void update() {

		this.updateCurrentHeading();
		this.updateXYCoordinates();
		this.updateAltitude();
		this.flightPlan.update();
	}
	
/**
 * render: draw's all elements of the flight and it's information.
 * @param g - Graphics libraries required by slick2d.
 * @param gc - GameContainer required by slick2d.
 */
	

	public void render(Graphics g, GameContainer gc) throws SlickException {
		
		this.drawFlight(g,  gc);
		this.flightPlan.render(g,gc);

		if(this.selected) {
			this.drawSelectedFlightInformation(g, gc);
			

		}
		
	}
	

	// MUTATORS AND ACCESSORS
	

	public double getX() {
		return this.x;
	}

	public void setX(double x) {
		this.x = x;
	}

	public double getY() {
		return this.y;
	}

	public void setY(double y) {
		this.y = y;
	}

	public double getCurrentHeading() {
		return this.currentHeading;
	}

	public void setCurrentHeading(double currentHeading) {
		this.currentHeading = currentHeading;
	}

	public double getTargetHeading() {
		return this.targetHeading;
	}

	public void setTargetHeading(double targetHeading) {
		this.targetHeading = targetHeading;
	}

	public int getTargetAltitude() {
		return this.targetAltitude;
	}

	public void setTargetAltitude(int targetAltitude) {
		this.targetAltitude = targetAltitude;
	}

	public int getAltitude() {
		return this.currentAltitude;
	}

	public void setAltitude(int altitude) {
		this.currentAltitude = altitude;
	}

	public boolean getTurningRight() {
		return this.turningRight;
	}

	public void setTurningRight(boolean turningRight) {
		this.turningRight = turningRight;
	}

	public boolean getTurningLeft() {
		return this.turningLeft;
	}

	public void setTurningLeft(boolean turningLeft) {
		this.turningLeft = turningLeft;
	}

	public void setFlightNum(int i) {
		this.flightNumber = i;
	}

	public int getFlightNum() {
		return flightNumber;
	}

	public String getFlightName() {
		return flightName;
	}

	public void setFlightName(String flightName) {
		this.flightName = flightName;
	}

	public boolean isSelected() {
		return selected;
	}

	public void setSelected(boolean selected) {
		this.selected = selected;
	}

	// tostring function to display a flight object so we can read it
	@Override
	public String toString() {
		return "X: " + this.x + " Y: " + this.y + " Flight Number: "
				+ this.flightNumber;
	}

	public int getCurrentAltitude() {
		return currentAltitude;
	}

	public void setCurrentAltitude(int currentAltitude) {
		this.currentAltitude = currentAltitude;
	}

	public FlightPlan getFlightPlan() {
		return flightPlan;
	}
	

	
	public Airspace getAirspace(){
		return airspace;
	}
	
	public boolean getSelected(){
		return this.selected;
	}



}
