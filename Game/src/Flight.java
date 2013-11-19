import java.util.Random;

public class Flight {

	//FIELDS
	public double x, y, current_heading, weight, target_heading, target_altitude;
	public int altitude;
	public boolean at_waypoint, turning_right, turning_left;
	public int MAXIMUM_ALTITUDE = 30000;
	public int MINIMUM_ALTITUDE = 27000;

	//CONSTRUCTOR
	Flight(){
		x = 0;
		y = 0;
		target_altitude = 0;
		altitude = generate_altitude();
		target_heading = 0;
		current_heading = 0;
		weight = 0;
		turning_right = false;
		turning_left = false;
		
	}
	
	//METHODS
	
	
	public int generate_altitude(){
		Random rand = new Random();
		return rand.nextInt((MAXIMUM_ALTITUDE-MINIMUM_ALTITUDE)+1) + MINIMUM_ALTITUDE;	
	}
	
	public void update_x_y_coordinates(){
		int velocity= 10; // This is merely a placeholder until the Flight Plan class is made.
		x += velocity * Math.sin(current_heading) ;
		y -= velocity * Math.cos(current_heading) ; 
	}
	
	public void turn_flight_left(int degree_turned_by){
		
		turning_right = false;
		turning_left = true;
		
		if ((current_heading - degree_turned_by) <= 0){
			target_heading = 360 - (degree_turned_by-current_heading);
		}
		else{
			target_heading -=  degree_turned_by;
		}
	}
	
	public void turn_flight_right(int degree_turned_by){
		
		turning_left = false;
		turning_right = true;
		
		if ((current_heading + degree_turned_by) >= 360){
			target_heading = (degree_turned_by-(360-current_heading));
		}
		else{
			target_heading += degree_turned_by;
		}
	}
	
	public void give_heading(int new_heading){
		turning_right = false;
		turning_left = false;
		new_heading = new_heading % 360;
		target_heading = new_heading;
	}
	
	public void set_altitude_lower(){
		if ((altitude-1000)<MINIMUM_ALTITUDE){
			altitude = MINIMUM_ALTITUDE;
		}
		else{
			altitude -= 1000;
		}
	}
	
	public void set_altitude_higher(){
		if ((altitude+1000)<MAXIMUM_ALTITUDE){
			altitude = MAXIMUM_ALTITUDE;
		}
		else{
			altitude += 1000;
		}
	}
	
	public void update_current_heading(){
		if (target_heading != current_heading){
			
			// If plane is already turning right or user has told it to turn right
			if (turning_right == true){
				current_heading += 1;
				if (current_heading == 360){
					current_heading = 0;
				}	
			}
			
			//if plane is already turning left or user has told it to turn left
			else if (turning_left == true){
				current_heading -= 1;
					if (current_heading == 0){
						current_heading = 360;
					}	
			}
			
			// If plane has been given a heading so no turning direction specified
			else{
				if (Math.abs(target_heading-current_heading)==180){
					turning_right = true;
					current_heading +=1;
				}
				else if ((current_heading+180)>= 360){
					if((180 - (360 - current_heading))>target_heading){
						turning_right = true;
						current_heading +=1;
						if (current_heading == 360){
							current_heading = 0;
						}
						
					}
					else{
						turning_left = true;
						current_heading -=1;
						if (current_heading == 0){
							current_heading = 360;
						}
					}
				
				}
				
				else{
					if((current_heading +180) >target_heading ){
						turning_right = true;
						current_heading +=1;
						if (current_heading == 360){
							current_heading = 0;
						}
					}
					else{
						turning_left = true;
						current_heading -=1;
						if (current_heading == 0){
							current_heading = 360;
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
	}
	
	public void render(){
	}
	
	public void draw(){
	}
	
	
	
	public static void main(String[] args) {
		Flight flight1 = new Flight();
		flight1.x = 10;
		flight1.y = 10;
		
		
		Flight flight2  = new Flight();
		flight2.x = 15;
		flight2.y = 15;
	
		
		
		System.out.print(Calculations.lateral_distance_between_flights(flight1, flight2));
		System.out.println();
		System.out.print(Calculations.vertical_distance_between_flights(flight1, flight2));
		System.out.println();
		System.out.print(Calculations.calculate_heading_to_first_waypoint(flight1, 10, 15));
		System.out.println();
		System.out.println(flight1.altitude);
		System.out.println(flight2.altitude);
		
		
		
		
		

	}

}
