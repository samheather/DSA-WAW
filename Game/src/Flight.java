import java.util.Random;

import org.lwjgl.input.Mouse;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.Input;
import org.newdawn.slick.SlickException;

public class Flight {

	//FIELDS
	private double x, y, target_altitude, current_heading, target_heading;
	private int current_altitude, flight_num, flight_button_x; 
	private boolean turning_right, turning_left;
	private String flight_name;
	private FlightPlan flight_plan;
	private int MAXIMUM_ALTITUDE = 30000;
	private int MINIMUM_ALTITUDE = 27000;
	private Image img;
	private Color color;
	private boolean selected;
	private Controls controls;

	


	

	//CONSTRUCTOR
	Flight(Airspace airspace){
		this.x = 0;
		this.y = 0;
		this.target_altitude = 0;
		this.current_altitude = generate_altitude();
		this.target_heading = 0;
		this.current_heading = 0;
		this.turning_right = false;
		this.turning_left = false;
		this.flight_plan = new FlightPlan(airspace);
		this.flight_button_x = airspace.getFlight_button_x();
		this.color=Color.white;
		this.selected=false;
		

		//current_heading=calc.calculate_heading_to_first_waypoint(this, this.flight_plan.getPointByIndex(0).getXCoOrd(), this.flight_plan.getPointByIndex(0).getXCoOrd());
		
	}
	
	//METHODS
	
	
	public int generate_altitude(){
		Random rand = new Random();
		return rand.nextInt((MAXIMUM_ALTITUDE-MINIMUM_ALTITUDE)+1) + MINIMUM_ALTITUDE;	
	}
	
	public boolean check_other_flight_selection(Airspace a) {
		for(int i=0;i<a.getList_of_flights().size();i++) {
			if(a.getList_of_flights().get(i)!=this) {
				if(a.getList_of_flights().get(i).isSelected()) {
					return true;
				}
			} 
		}
		return false;
	}
	
	public void turn_flight_left(int degree_turned_by){
		
		this.turning_right = false;
		this.turning_left = true;
		
		if (((int)this.target_heading - degree_turned_by) < 0){
			this.target_heading = 360 - (degree_turned_by-(int)this.target_heading);
		}
		else{
			this.target_heading -=  degree_turned_by;
		}
	}
	
	public void turn_flight_right(int degree_turned_by){
		
		this.turning_left = false;
		this.turning_right = true;
		
		if (((int)this.target_heading + degree_turned_by) >= 360){
			this.target_heading = (degree_turned_by-(360-(int)this.target_heading));
		}
		else{
			this.target_heading += degree_turned_by;
		}
	}
	
	public void give_heading(int new_heading){
		this.turning_right = false;
		this.turning_left = false;
		new_heading = new_heading % 360;
		this.target_heading = new_heading;
	}
	
	public void set_altitude_lower(){
		if ((this.current_altitude-1000)<MINIMUM_ALTITUDE){
			this.target_altitude = MINIMUM_ALTITUDE;
		}
		else{
			this.target_altitude -= 1000;
		}
	}
	
	public void set_altitude_higher(){
		if ((this.current_altitude+1000)<MAXIMUM_ALTITUDE){
			this.target_altitude = MAXIMUM_ALTITUDE;
		}
		else{
			this.target_altitude += 1000;
		}
	}
	
	public void update_x_y_coordinates(){
		double velocity= (this.flight_plan.getVelocity())/500;

		this.x += velocity * Math.sin(Math.toRadians(this.current_heading)) ;

		this.y -= velocity * Math.cos(Math.toRadians(this.current_heading)) ; 

	}
	
	public void update_altitude(){
		if (this.current_altitude > this.target_altitude){
			this.current_altitude -= 1;
		}
		
		else if (this.current_altitude < this.target_altitude){
			this.current_altitude += 1;
		}
	}
	
	public void update_current_heading(){
		// NOTE TO RORY FROM RORY: DONT FORGET TO FIX THIS FOR EVERY CASE
		double rate = 0.2;
		if (Math.round(this.target_heading)!=Math.round(this.current_heading)){		
			if (this.turning_right == true){// If plane is already turning right or user has told it to turn right
				this.current_heading += rate;
				if (Math.round(this.current_heading) == 360&&this.target_heading!=360){
					this.current_heading = 0;
				}	
			}
			
			//if plane is already turning left or user has told it to turn left
			else if (this.turning_left == true){
				this.current_heading -= rate;
					if (Math.round(this.current_heading) == 0&&this.target_heading!=0){
						this.current_heading = 360;
					}	
			}
			
			// If plane has been given a heading so no turning direction specified
			// Below works out whether it should turn left or right to that heading.
			else{
				
				if (this.target_heading-this.current_heading==180){
					this.turning_right = true;
					this.current_heading +=rate;
				}
				else if ((this.current_heading+180)>= 359){
					
					if (this.target_heading > this.current_heading ){
						this.turning_right = true;
						this.current_heading +=rate;
						if (Math.round(this.current_heading) == 360){
							this.current_heading = 0;
						}
					}
					else if((180 - (360 - this.current_heading))>this.target_heading){
						this.turning_right = true;
						this.current_heading +=rate;
						if (Math.round(this.current_heading) == 360){
							this.current_heading = 0;
						}
					}
					else{
						this.turning_left = true;
						this.current_heading -=rate;
						if (Math.round(this.current_heading) == 0){
							this.current_heading = 360;
						}
					}
				}
				else{
					if( (this.target_heading > this.current_heading) && (this.target_heading < this.current_heading+180) ){
						this.turning_right = true;
						this.current_heading +=rate;
						if (Math.round(this.current_heading) == 360){
							this.current_heading = 0;
						}
					}
					else{
						this.turning_left = true;
						this.current_heading -=rate;
						if (Math.round(this.current_heading) == 0){
							this.current_heading = 360;
						}
					}
				}
			}
		}
	}
	
	public boolean check_if_flight_at_waypoint(Point waypoint)
	{
		// The line below is just so there are no errors
			if (((Math.abs(Math.round(this.x) - Math.round(waypoint.getX()))) <= 20)
					&& (Math.abs(Math.round(this.y) - Math.round(waypoint.getY()))) <= 20){
				return true;
			}

		return false;
	}
		
	
	// UPDATE, RENDER, INIT
	
	public void update(GameContainer gc, Airspace a){
		
		boolean init=false;
		Input input = gc.getInput();
		this.update_current_heading();
		this.update_x_y_coordinates();
		this.update_altitude();
		if(this.flight_plan.getWaypoints().size()>0) {
			if(this.check_if_flight_at_waypoint(flight_plan.getWaypoints().get(0))){
				this.flight_plan.getWaypoints().remove(0);
			}
		}
		int posX=Mouse.getX();
		int posY=Mouse.getY();
		
		//Update controls
		if(this.selected==true) {
			this.controls.update(gc);
			if (!this.controls.headingHasFocus()) {
				this.controls.getHeadingControlTB().setText(String.valueOf(Math.round(this.target_heading)));
			}
			if (!this.controls.altHasFocus()){
				this.controls.getAltControlTB().setText(String.valueOf(Math.round(this.target_altitude)));
			}
			this.controls.allow_all();
			
			
			this.color=Color.yellow;
			if(input.isKeyDown(Input.KEY_UP)) {
				this.give_heading(0);
			}
			if(input.isKeyDown(Input.KEY_LEFT)) {
				this.give_heading(270);
			}
			if(input.isKeyDown(Input.KEY_DOWN)) {
				this.give_heading(180);
			}
			if(input.isKeyDown(Input.KEY_RIGHT)) {
				this.give_heading(90);
			}
			if((((posX<this.flight_button_x||posX>this.flight_button_x+100)||(posY<0||posY>100))&&Mouse.isButtonDown(0)) || this.flight_plan.getWaypoints().isEmpty()) {
				if((((posX>0&&posX<100)&&(posY>0&&posY<600))&&Mouse.isButtonDown(0))&&!this.flight_plan.getWaypoints().isEmpty()) {
					this.selected=true;
					a.set_selected_flight(this);
				}
			if(this.check_other_flight_selection(a)) {
				this.selected=false;
			}
			}
		}
		else {
				this.color=Color.white;

					this.controls.clear_all();

				if((posX>this.flight_button_x&&posX<this.flight_button_x+100)&&(posY>0&&posY<100)&&Mouse.isButtonDown(0)) {

					this.selected=true;
					a.set_selected_flight(this);

				}
			}


	}

	public void init(GameContainer gc) throws SlickException{
		img = new Image("plane.png");
		this.controls=new Controls(gc,this);
		controls.init(gc);
		
	}
	
	public void render(Graphics g, GameContainer gc) throws SlickException{
		g.setColor(color);
		g.setWorldClip(100, 0, 1100, 500);
		g.drawString(this.flight_name, (int)this.x-15, (int)this.y-20);


		if(this.flight_plan.getWaypoints().size()>0) {
			g.drawString(this.flight_plan.getPointByIndex(0).getPointRef(),(int)this.x+5,(int)this.y+20);
		}
		
		
		img.setRotation((int)current_heading);
		
		g.drawOval((int)this.x-40, (int)this.y-40, 100, 100);
		
		img.draw((int)this.x, (int)this.y);
		
		g.setWorldClip(0, 0, 1200, 600);
		g.fillRect((int)this.flight_button_x, 500, 99, 100);
		g.setColor(Color.black);
		g.drawString(this.flight_name, (int)this.flight_button_x+13, 510);
		g.drawString(Math.round(this.current_altitude) + " ft", (int)this.flight_button_x+13, 525);
		g.drawString(Math.round(this.current_heading) + " deg", (int)this.flight_button_x+13, 555);
		g.drawString(Math.round(this.getFlight_plan().getVelocity()) + " MPH", (int)this.flight_button_x+13, 540);
		if(this.selected) {
			this.controls.render(gc, g);
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
		return this.current_heading;
	}

	public void setCurrent_heading(double current_heading) {
		this.current_heading = current_heading;
	}

	public double getTarget_heading() {
		return this.target_heading;
	}

	public void setTarget_heading(double target_heading) {
		this.target_heading = target_heading;
	}

	public double getTarget_altitude() {
		return this.target_altitude;
	}

	public void setTarget_altitude(double target_altitude) {
		this.target_altitude = target_altitude;
	}

	public int getAltitude() {
		return this.current_altitude;
	}

	public void setAltitude(int altitude) {
		this.current_altitude = altitude;
	}

	public boolean isTurning_right() {
		return this.turning_right;
	}

	public void setTurning_right(boolean turning_right) {
		this.turning_right = turning_right;
	}

	public boolean isTurning_left() {
		return this.turning_left;
	}

	public void setTurning_left(boolean turning_left) {
		this.turning_left = turning_left;
	}
	public void setFlight_num(int i) {
		this.flight_num=i;
	}
	public int getFlight_button_x() {
		return flight_button_x;
	}

	public void setFlight_button_x(int flight_button_x) {
		this.flight_button_x = flight_button_x;
	}

	public int getFlight_num() {
		return flight_num;
	}

	public String getFlight_name() {
		return flight_name;
	}

	public void setFlight_name(String flight_name) {
		this.flight_name = flight_name;
	}

	public boolean isSelected() {
		return selected;
	}

	public void setSelected(boolean selected) {
		this.selected = selected;
	}

	//tostring function to display a flight object so we can read it
	@Override
	public String toString() {
		return "X: "+this.x+" Y: "+this.y+" Flight Number: "+this.flight_num;
	}
	

	public int getCurrent_altitude() {
		return current_altitude;
	}

	public void setCurrent_altitude(int current_altitude) {
		this.current_altitude = current_altitude;
	}

	public Controls getControls() {
		return controls;
	}

	public FlightPlan getFlight_plan() {
		return flight_plan;
	}

}
