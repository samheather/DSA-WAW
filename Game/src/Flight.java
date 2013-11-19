import java.util.Random;

public class Flight {

	//FIELDS
	public double x, y, current_heading, weight;
	public int altitude;
	public boolean on_course, at_waypoint;
	public int MAXIMUM_ALTITUDE = 30000;
	public int MINIMUM_ALTITUDE = 27000;

	//CONSTRUCTOR
	Flight(){
		x = 0;
		y = 0;
		altitude = generate_altitude();
		current_heading = 0;
		weight = 0;
		on_course = true;
	}
	
	//METHODS
	
	public void set_on_course(){
		if (on_course == true){
			on_course = false;
		}
		else{
			on_course = true;
		}
	}
	
	public int generate_altitude(){
		Random rand = new Random();
		return rand.nextInt((MAXIMUM_ALTITUDE-MINIMUM_ALTITUDE)+1) + MINIMUM_ALTITUDE;	
	}
	
	public void update_x_y_coordinates(){
		int speed = 10; // This is merely a placeholder until the Flight Plan class is made.
		x += speed * Math.sin(current_heading) ;
		y -= speed * Math.cos(current_heading) ; 
	}
	
	public void turn_flight_left(){
		this.set_on_course();
		if ((current_heading - 10) <0){
			current_heading = 360 - (10-current_heading);
		}
		else{
			current_heading -= 10;
		}
	}
	
	public void turn_flight_right(){
		this.set_on_course();
		if ((current_heading + 10) > 359){
			current_heading = 0 + (10-(359-current_heading));
		}
		else{
			current_heading += 10;
		}
	}
	
	public void give_heading(int new_heading){
		this.set_on_course();
		new_heading = new_heading % 360;
		current_heading = new_heading;
	}
	
	public void set_altitude_lower(){
		if ((altitude-100)<MINIMUM_ALTITUDE){
			altitude = MINIMUM_ALTITUDE;
		}
		else{
			altitude -= 100;
		}
	}
	
	public void set_altitude_higher(){
		if ((altitude+100)<MAXIMUM_ALTITUDE){
			altitude = MAXIMUM_ALTITUDE;
		}
		else{
			altitude += 100;
		}
	}
	
	public void update_current_heading(){
	}
	
	public boolean check_if_flight_at_waypoint()
	{
		// The line below is just so there are no errors
		return true;
	}
	
	// Update, Render, Draw
	
	public void update(){
		if (on_course == false){
			this.update_x_y_coordinates();
		}
		else{
			at_waypoint = this.check_if_flight_at_waypoint();
			if (at_waypoint = true){
				update_current_heading();
			}
			this.update_x_y_coordinates();
		}
	}
	
	public void render(){
	}
	
	public void draw(){
	}
	
	//MUTATORS AND ACCESSORS
	
	public void set_x(double new_x){
		x = new_x;
	}
	
	public void set_y(double new_y){
		y = new_y;
	}
	
	public void set_altitude(int new_altitude){
		altitude = new_altitude;
	}
	
	public void set_weight(double new_weight){
		weight = new_weight;
	}
	
	
	public static void main(String[] args) {
		Flight flight1 = new Flight();
		flight1.set_x(10);
		flight1.set_y(10);
		
		
		Flight flight2  = new Flight();
		flight2.set_x(15);
		flight2.set_y(15);
	
		
		
		System.out.print(Calculations.lateral_distance_between_flights(flight1, flight2));
		System.out.println();
		System.out.print(Calculations.vertical_distance_between_flights(flight1, flight2));
		System.out.println();
		System.out.print(Calculations.calculate_heading(flight1, 10, 15));
		
		System.out.println();
		System.out.println(flight1.altitude);
		System.out.println(flight2.altitude);
		
		
		
		
		

	}

}
