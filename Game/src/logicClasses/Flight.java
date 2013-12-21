package logicClasses;
import java.util.Random;

import org.lwjgl.input.Mouse;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;

public class Flight {

	// FIELDS
	private double x, y, targetAltitude, currentHeading, targetHeading;
	private int currentAltitude, flightNumber;
	private boolean turningRight, turningLeft;
	private String flightName;
	private FlightPlan currentFlightPlan;
	private Image regularFlightImage, selectedFlightInformationBackgroundImage, slowFlightImage, fastFlightImage, shadowImage;
	private Color color;
	private boolean selected;
	private Point waypointMouseIsOver;
	private Point waypointClicked;
	private EntryPoint entryPoint;
	private Airspace airspace;
	private boolean changingPlan;
	private boolean draggingWaypoint;
	
	

	// CONSTRUCTOR
	public Flight(Airspace airspace) {
		this.x = 0;
		this.y = 0;
		this.targetAltitude = 0;
		this.currentAltitude = generate_altitude();
		this.targetHeading = 0;
		this.currentHeading = 0;
		this.turningRight = false;
		this.turningLeft = false;
		this.airspace = airspace;
		this.entryPoint = generate_entry_point();
		this.currentFlightPlan = new FlightPlan(airspace, this.entryPoint);
		this.color = Color.white;
		this.selected = false;
		this.changingPlan = false;
		this.draggingWaypoint = false;
		

	}

	// METHODS
	
	public EntryPoint generate_entry_point(){
		
		Random rand = new Random();
		int random_number = rand.nextInt(3);
		
		
		this.airspace.getList_of_entrypoints().get(random_number);
			
		// Setting flights x and y to the coordinates of it's entrypoint
		this.x = this.airspace.getList_of_entrypoints().get(random_number).getX();// choose one a get the x and y values
		this.y = this.airspace.getList_of_entrypoints().get(random_number).getY();
		
		return this.airspace.getList_of_entrypoints().get(random_number);
		
	}

	public int generate_altitude() {
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

//	public boolean check_other_flight_selection(Airspace a) {
//		for (int i = 0; i < a.getList_of_flights().size(); i++) {
//			if (a.getList_of_flights().get(i) != this) {
//				if (a.getList_of_flights().get(i).isSelected()) {
//					return true;
//				}
//			}
//		}
//		return false;
//	}

	public double calculate_heading_to_first_waypoint(double destination_x, double destination_y) {
		double deltaX;
		double deltaY;
		deltaY = destination_y - this.y;
		deltaX = destination_x - this.x;
		double angle = Math.toDegrees(Math.atan2(deltaY, deltaX));
		angle += 90;
		if (angle < 0) {
			angle += 360;
		}
		return angle;
	}
	
	public void turn_flight_left(int degree_turned_by) {

		this.turningRight = false;
		this.turningLeft = true;

		this.targetHeading = Math.round(this.currentHeading) - degree_turned_by;
		if(this.targetHeading < 0){
			this.targetHeading = 360 +this.targetHeading;
		}
	}
	
	
	public void turn_flight_right(int degree_turned_by) {

		this.turningLeft = false;
		this.turningRight = true;
		
		this.targetHeading = Math.round(this.currentHeading) + degree_turned_by;
		if(this.targetHeading >= 360){
			this.targetHeading = this.targetHeading - 360;
		}


	}

	public void give_heading(int new_heading) {
		this.turningRight = false;
		this.turningLeft = false;
		new_heading = new_heading % 360;
		this.targetHeading = new_heading;
	}
	
	
	public boolean check_if_flight_at_waypoint(Point waypoint) {
		
		if (((Math.abs(Math.round(this.x) - Math.round(waypoint.getX()))) <= 15)
				&& (Math.abs(Math.round(this.y) - Math.round(waypoint.getY()))) <= 15) {
			return true;
		}

		return false;
	}
	
	public static boolean isBetween(int x, int lower, int upper) {
		  return lower <= x && x < upper;
		}
	
	private boolean isMouseOnWaypoint() {
		int mouseX = Mouse.getX();
		int mouseY = Mouse.getY();
		mouseY=600-mouseY;
		if(this.getFlight_plan().getWaypoints().isEmpty()) {
			return false;
		}
		for(int i=0; i<this.airspace.getList_of_way_points().size();i++) {
			if (((Math.abs(Math.round(mouseX) - Math.round(this.airspace.getList_of_way_points().get(i).getX()))) <= 15)
					&& (Math.abs(Math.round(mouseY) - Math.round(this.airspace.getList_of_way_points().get(i).getY()))) <= 15) {
				
					this.waypointMouseIsOver=this.airspace.getList_of_way_points().get(i);
					return true;
					
			}
		}
		this.waypointMouseIsOver=null;
		return false;
	}
	
	public void change_flight_plan(){
		if (this.selected && this.currentFlightPlan.getWaypoints().size() > 0 ){
			boolean mouseOverWaypoint = this.isMouseOnWaypoint();

				// Checks if user is not currently dragging a waypoint
				if (!draggingWaypoint){
					//Checks if user has clicked on a waypoint
					if(mouseOverWaypoint && Mouse.isButtonDown(0)) {
						this.waypointClicked=this.waypointMouseIsOver;
						this.draggingWaypoint=true;
					}
				}
				
				// Checks if user is currently dragging a waypoint
				else if(draggingWaypoint){
					// Checks if user has released mouse from drag over empty airspace
					if((!Mouse.isButtonDown(0)) && !mouseOverWaypoint){
						this.waypointClicked=null;
						this.draggingWaypoint=false;
							
					}
					
					// Checks if user has released mouse from drag over another waypoint
					else if((!Mouse.isButtonDown(0)) && mouseOverWaypoint){
						
						//Finding waypoint that mouse is over
						for(int i=0; i<this.currentFlightPlan.getWaypoints().size();i++) {
							
							// Checks if new waypoint is not already in the plan and adds if not in plan
							if (this.waypointClicked == this.getFlight_plan().getWaypoints().get(i)&& (!this.getFlight_plan().getWaypoints().contains(this.waypointMouseIsOver)) ){
								this.getFlight_plan().getWaypoints().remove(i);
								this.getFlight_plan().getWaypoints().add(i,this.waypointMouseIsOver);
								this.waypointClicked=null;
								this.draggingWaypoint=false;
								
							}
							
							// Checks if waypoint already in plan and doesn't add if not
							else if(this.waypointClicked == this.getFlight_plan().getWaypoints().get(i)&& (this.getFlight_plan().getWaypoints().contains(this.waypointMouseIsOver))){
								this.waypointClicked=null;
								this.draggingWaypoint=false;
								break;
								
							}
						}
					}
				}
		}
	}
	


	
	
	// DRAWING METHODS
	
	public void draw_flight(Graphics g, GameContainer gc ){

				g.setColor(color);
				g.setWorldClip(150, 0, 1200, 600);

				//Scale the shadow in accordance to the altitude of the flight
				float shadow_scale = (float) (36 - (this.currentAltitude / 1000))/10;
				shadowImage.setRotation((int) currentHeading);
				shadowImage.draw((int) this.x-35, (int) this.y, shadow_scale);
				
				//Depending on a plane's speed, different images for the plane are drawn
					
				if(this.currentFlightPlan.getVelocity() <= 275){
					
					slowFlightImage.setRotation((int) currentHeading);
					slowFlightImage.draw((int) this.x-10, (int) this.y-10);
					
				}
				
				else if(this.currentFlightPlan.getVelocity() > 270 && this.currentFlightPlan.getVelocity() < 340){
					
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
				
				if (this.selected){
					g.setColor(Color.white);
					g.drawString(this.flightName, (int) this.x-24, (int) this.y-44);
					g.drawString(Math.round(this.currentAltitude) + " ft",(int) this.x-30, (int) this.y + 10);
					g.drawString(Math.round(this.currentHeading) + "°",(int) this.x - 13, (int) this.y + 25);//-15,20
					
					if (this.currentFlightPlan.getWaypoints().size() > 0) {
						g.drawString("Aim: "+this.currentFlightPlan.getPointByIndex(0).getPointRef(),(int) this.x -22, (int)this.y-28);
						
					}
	
				}
				
				else{
					g.setColor(Color.lightGray);
					g.drawString(this.flightName, (int) this.x-24, (int) this.y-44);
					g.drawString(Math.round(this.currentAltitude) + " ft",(int) this.x-30, (int) this.y + 10);
					
					if (this.currentFlightPlan.getWaypoints().size() > 0) {
						g.drawString("Aim: "+this.currentFlightPlan.getPointByIndex(0).getPointRef(),(int) this.x -22, (int)this.y-28);
					}
					g.drawOval((int) this.x - 50, (int) this.y - 50, 100, 100);
				}
				
				g.setWorldClip(0, 0, 1200, 600);
		
	}
	

	
	public void draw_selected_flight_information(Graphics g, GameContainer gc) {
		
		this.selectedFlightInformationBackgroundImage.draw(0,450);
		g.setColor(Color.white);
		g.drawString(this.flightName,  10, 460);
		g.drawString("Plan: ",  10, 480);
		String plan = "";
		
		for(int i=0; i<this.currentFlightPlan.getWaypoints().size(); i++) {
			plan += this.currentFlightPlan.getWaypoints().get(i).getPointRef()+", ";
		}
		
		
		g.setColor(Color.white);
		g.drawString(plan, 10, 500);
		g.drawString(Math.round(this.currentAltitude) + " Ft",
			 10, 520);
		g.drawString(Math.round(this.currentHeading) + " DEG",
			10, 540);
		g.drawString(Math.round(this.getFlight_plan().getVelocity()) + " MPH",
			10, 560);
		
	}
	
	// UPDATE METHODS
	

	public void update_x_y_coordinates() {
		double velocity = (this.currentFlightPlan.getVelocity()) / 1000;

		this.x += velocity * Math.sin(Math.toRadians(this.currentHeading));

		this.y -= velocity * Math.cos(Math.toRadians(this.currentHeading));

	}

	public void update_altitude() {
		if (this.currentAltitude > this.targetAltitude) {
			this.currentAltitude -= 1;
		}

		else if (this.currentAltitude < this.targetAltitude) {
			this.currentAltitude += 1;
		}
	}

	public void update_current_heading() {
	
		double rate = 0.5;
		if (Math.round(this.targetHeading) != Math.round(this.currentHeading)) {
			if (this.turningRight == true) {// If plane is already turning
												// right or user has told it to
												// turn right
				this.currentHeading += rate;
				if (Math.round(this.currentHeading) == 360
						&& this.targetHeading != 360) {
					this.currentHeading = 0;
				}
			}

			// if plane is already turning left or user has told it to turn left
			else if (this.turningLeft == true) {
				this.currentHeading -= rate;
				if (Math.round(this.currentHeading) == 0
						&& this.targetHeading != 0) {
					this.currentHeading = 360;
				}
			}

			// If plane has been given a heading so no turning direction specified
			// Below works out whether it should turn left or right to that heading.
			else {

				if (this.targetHeading - this.currentHeading == 180) {
					this.turningRight = true;
					this.currentHeading += rate;
				} else if ((this.currentHeading + 180) >= 359) {

					if (this.targetHeading > this.currentHeading) {
						this.turningRight = true;
						this.currentHeading += rate;
						if (Math.round(this.currentHeading) == 360) {
							this.currentHeading = 0;
						}
					} else if ((180 - (360 - this.currentHeading)) > this.targetHeading) {
						this.turningRight = true;
						this.currentHeading += rate;
						if (Math.round(this.currentHeading) == 360) {
							this.currentHeading = 0;
						}
					} else {
						this.turningLeft = true;
						this.currentHeading -= rate;
						if (Math.round(this.currentHeading) == 0) {
							this.currentHeading = 360;
						}
					}
				} else {
					if ((this.targetHeading > this.currentHeading)
							&& (this.targetHeading < this.currentHeading + 180)) {
						this.turningRight = true;
						this.currentHeading += rate;
						if (Math.round(this.currentHeading) == 360) {
							this.currentHeading = 0;
						}
					} else {
						this.turningLeft = true;
						this.currentHeading -= rate;
						if (Math.round(this.currentHeading) == 0) {
							this.currentHeading = 360;
						}
					}
				}
			}
		}
	}
	
	public void update_flight_plan(){

		if (this.currentFlightPlan.getWaypoints().size() > 0) {
			if (this.check_if_flight_at_waypoint(currentFlightPlan.getWaypoints()
					.get(0))) {
				this.currentFlightPlan.getWaypoints().remove(0);
			}
		}

	}
	
	
	

	public void draw_flights_plan(Graphics g, GameContainer gc){

		if (this.currentFlightPlan.getWaypoints().size() > 0){
			
			g.setColor(Color.cyan);
			
			// If not dragging waypoints, just draw lines between all waypoints in plan
			if(!draggingWaypoint){
				for(int i=1; i<this.currentFlightPlan.getWaypoints().size();i++) {
					g.drawLine((float)this.getFlight_plan().getWaypoints().get(i).getX(), (float)this.getFlight_plan().getWaypoints().get(i).getY(), (float)this.getFlight_plan().getWaypoints().get(i-1).getX(), (float)this.getFlight_plan().getWaypoints().get(i-1).getY());
				}
			}
			
			else if(draggingWaypoint){
				for(int i=1; i<this.currentFlightPlan.getWaypoints().size();i++) {
					
					// This is needed as i=1 behavours differently to other values of i when first waypoint is being dragged.
					if(i==1){
						if(this.waypointClicked == this.getFlight_plan().getWaypoints().get(0) ) {
							g.drawLine(Mouse.getX(),600-Mouse.getY() , (float)this.getFlight_plan().getWaypoints().get(1).getX(),(float)this.getFlight_plan().getWaypoints().get(1).getY());
						}
						
						else if (this.waypointClicked == this.getFlight_plan().getWaypoints().get(1)){
							g.drawLine((float)this.currentFlightPlan.getWaypoints().get(i+1).getX(), (float)this.getFlight_plan().getWaypoints().get(i+1).getY(),Mouse.getX(),600-Mouse.getY());
							g.drawLine((float)this.currentFlightPlan.getWaypoints().get(i-1).getX(), (float)this.getFlight_plan().getWaypoints().get(i-1).getY(),Mouse.getX(),600-Mouse.getY());
							i++;
							
						}
						
						else{
							g.drawLine((float)this.getFlight_plan().getWaypoints().get(i).getX(), (float)this.getFlight_plan().getWaypoints().get(i).getY(), (float)this.getFlight_plan().getWaypoints().get(i-1).getX(), (float)this.getFlight_plan().getWaypoints().get(i-1).getY());
						}
						

					}
					
					else{
						// If Waypoint is being changed draw lines between mouse and waypoint before and after the waypoint being changed. 
						if (this.waypointClicked == this.getFlight_plan().getWaypoints().get(i)){
							g.drawLine((float)this.currentFlightPlan.getWaypoints().get(i+1).getX(), (float)this.getFlight_plan().getWaypoints().get(i+1).getY(),Mouse.getX(),600-Mouse.getY());
							g.drawLine((float)this.currentFlightPlan.getWaypoints().get(i-1).getX(), (float)this.getFlight_plan().getWaypoints().get(i-1).getY(),Mouse.getX(),600-Mouse.getY());
							i++;
						}
						
						else{
							g.drawLine((float)this.getFlight_plan().getWaypoints().get(i).getX(), (float)this.getFlight_plan().getWaypoints().get(i).getY(), (float)this.getFlight_plan().getWaypoints().get(i-1).getX(), (float)this.getFlight_plan().getWaypoints().get(i-1).getY());
						}
						
					}
				}
			}
				
		}
		
	}


	// UPDATE, RENDER, INIT
	
	
	public void init(GameContainer gc) throws SlickException {
		this.regularFlightImage = new Image("/res/graphics/graphics/flight.png");
		this.shadowImage = new Image("/res/graphics/graphics/flight_shadow.png");
		this.slowFlightImage = new Image("/res/graphics/graphics/flight_slow.png");
		this.fastFlightImage = new Image("/res/graphics/graphics/flight_fast.png");
		this.selectedFlightInformationBackgroundImage = new Image("res/graphics/graphics/selected_flight2.jpg");

	}
	
	


	public void update() {


		this.update_current_heading();
		this.update_x_y_coordinates();
		this.update_altitude();
		this.update_flight_plan();
		
		if(this.changingPlan == true){
			this.change_flight_plan();
		}
		

	}
	

	public void render(Graphics g, GameContainer gc) throws SlickException {
		
		
		this.draw_flight(g,  gc);
		
		

		if(this.selected) {
			this.draw_selected_flight_information(g, gc);
			if(this.changingPlan == true){
				this.draw_flights_plan(g, gc);
			}
			

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

	public double getCurrent_heading() {
		return this.currentHeading;
	}

	public void setCurrent_heading(double current_heading) {
		this.currentHeading = current_heading;
	}

	public double getTarget_heading() {
		return this.targetHeading;
	}

	public void setTarget_heading(double target_heading) {
		this.targetHeading = target_heading;
	}

	public double getTarget_altitude() {
		return this.targetAltitude;
	}

	public void setTarget_altitude(double target_altitude) {
		this.targetAltitude = target_altitude;
	}

	public int getAltitude() {
		return this.currentAltitude;
	}

	public void setAltitude(int altitude) {
		this.currentAltitude = altitude;
	}

	public boolean isTurning_right() {
		return this.turningRight;
	}

	public void setTurning_right(boolean turning_right) {
		this.turningRight = turning_right;
	}

	public boolean isTurning_left() {
		return this.turningLeft;
	}

	public void setTurning_left(boolean turning_left) {
		this.turningLeft = turning_left;
	}

	public void setFlight_num(int i) {
		this.flightNumber = i;
	}

	public int getFlight_num() {
		return flightNumber;
	}

	public String getFlight_name() {
		return flightName;
	}

	public void setFlight_name(String flight_name) {
		this.flightName = flight_name;
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

	public int getCurrent_altitude() {
		return currentAltitude;
	}

	public void setCurrent_altitude(int current_altitude) {
		this.currentAltitude = current_altitude;
	}

	public FlightPlan getFlight_plan() {
		return currentFlightPlan;
	}
	
	public EntryPoint getEntryPoint(){
		return this.entryPoint;
	}
	
	public boolean getChangingPlan(){
		return this.changingPlan;
	}
	
	public void setChangingPlan(boolean bool){
		this.changingPlan = bool;
	}
	
	public boolean getDraggingWaypoint(){
		return this.draggingWaypoint;
	}



}
