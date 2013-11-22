import java.util.Random;

import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;

public class Flight {

	//FIELDS
	private double x, y, current_heading, weight, target_heading, target_altitude;
	private int current_altitude;
	private boolean at_waypoint, turning_right, turning_left;
	private FlightPlan flight_plan;
	private int MAXIMUM_ALTITUDE = 30000;
	private int MINIMUM_ALTITUDE = 27000;
	int flight_num;
	Image img;

	//CONSTRUCTOR
	Flight(){
		this.x = 0;
		this.y = 0;
		this.target_altitude = 0;
		this.current_altitude = generate_altitude();
		this.target_heading = 0;
		this.current_heading = 0;
		this.turning_right = false;
		this.turning_left = false;
		
	}
	
	//METHODS
	
	
	public int generate_altitude(){
		Random rand = new Random();
		return rand.nextInt((MAXIMUM_ALTITUDE-MINIMUM_ALTITUDE)+1) + MINIMUM_ALTITUDE;	
	}
	
	public void turn_flight_left(int degree_turned_by){
		
		this.turning_right = false;
		this.turning_left = true;
		
		if ((this.current_heading - degree_turned_by) <= 0){
			this.target_heading = 360 - (degree_turned_by-this.current_heading);
		}
		else{
			this.target_heading -=  degree_turned_by;
		}
	}
	
	public void turn_flight_right(int degree_turned_by){
		
		this.turning_left = false;
		this.turning_right = true;
		
		if ((this.current_heading + degree_turned_by) >= 360){
			this.target_heading = (degree_turned_by-(360-this.current_heading));
		}
		else{
			this.target_heading += degree_turned_by;
		}
	}
	
	public void give_heading(int new_heading){
		this.turning_right = false;
		this.turning_left = false;
		//new_heading = new_heading % 360;
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
		int velocity= 1; // This is merely a placeholder until the Flight Plan class is made.
		
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
		if (this.target_heading != this.current_heading){
			
			// If plane is already turning right or user has told it to turn right
			if (this.turning_right == true){
				this.current_heading += 0.2;
				if (this.current_heading == 360){
					this.current_heading = 0;
				}	
			}
			
			//if plane is already turning left or user has told it to turn left
			else if (this.turning_left == true){
				this.current_heading -= 0.2;
					if (this.current_heading == 0){
						this.current_heading = 360;
					}	
			}
			
			// If plane has been given a heading so no turning direction specified
			// Below works out whether it should turn left or right to that heading.
			else{
				if (Math.abs(this.target_heading-this.current_heading)==180){
					this.turning_right = true;
					this.current_heading +=0.2;
				}
				else if ((this.current_heading+180)>= 360){
					
					if (this.target_heading > this.current_heading){
						this.turning_right = true;
						this.current_heading +=0.2;
						if (this.current_heading == 360){
							this.current_heading = 0;
						}
					}
					else if((180 - (360 - this.current_heading))>this.target_heading){
						this.turning_right = true;
						this.current_heading +=0.2;
						if (this.current_heading == 360){
							this.current_heading = 0;
						}
					}
					else{
						this.turning_left = true;
						this.current_heading -=0.2;
						if (this.current_heading == 0){
							this.current_heading = 360;
						}
					}
				}
				else{
					if((this.current_heading +180) >this.target_heading ){
						this.turning_right = true;
						this.current_heading +=0.2;
						if (this.current_heading == 360){
							this.current_heading = 0;
						}
					}
					else{
						this.turning_left = true;
						this.current_heading -=0.2;
						if (this.current_heading == 0){
							this.current_heading = 360;
						}
					}
				}
			}
		}
	}
	
	public boolean check_if_flight_at_waypoint()
	{
		// The line below is just so there are no errors
		return true;
	}
		
	
	// UPDATE, RENDER, DRAW
	
	public void update(){
		this.update_current_heading();
		this.update_x_y_coordinates();
	}
	
	public void render(Graphics g){
		
		img.draw((int)this.x, (int)this.y);
		
		img.setRotation((int)current_heading);
	}
	
	public void init() throws SlickException{
		img = new Image("res/plane.png");
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
	

}
