
public class Calculations {

	
	Calculations(){
	}
	
	// Put in Separations class
	public static double lateral_distance_between_flights(Flight flight1, Flight flight2){
		
		return Math.sqrt(Math.pow((flight1.x - flight2.x), 2) + Math.pow(( flight1.y - flight2.y),2));
		
	}
	
	// Put in Separations class
	public static int vertical_distance_between_flights(Flight flight1, Flight flight2){
		
		return Math.abs(flight1.altitude - flight2.altitude);
		
	}
	
	public static void lateral_distance_between_flight_and_waypoint(){
	}
	
	
	
	public static double calculate_heading(Flight flight1, double destination_x, double destination_y ){
		
		// DIAGONALS
		if ((flight1.x > destination_x) && (flight1.y < destination_y)){
			return  360 - Math.toDegrees(Math.atan((Math.abs(flight1.x-destination_x) / Math.abs(flight1.y-destination_y))));
		}
		
		else if ((flight1.x < destination_x) && (flight1.y < destination_y)){
			return  Math.toDegrees(Math.atan((Math.abs(flight1.x-destination_x) / Math.abs(flight1.y-destination_y))));
		}
		
		else if ((flight1.x < destination_x) && (flight1.y > destination_y )){
			return 180 - Math.toDegrees(Math.atan((Math.abs(flight1.x-destination_x) / Math.abs(flight1.y-destination_y))));                           
		}
		
		else if ((flight1.x > destination_x) && (flight1.y > destination_y )){
			return 180 + Math.toDegrees(Math.atan((Math.abs(flight1.x-destination_x) / Math.abs(flight1.y-destination_y))));                           
		}
		
		// HORIZONTALS
		
		else if ((flight1.x < destination_x) && (flight1.y == destination_y)){
			return  90;
		}
		
		else if ((flight1.x > destination_x) && (flight1.y == destination_y)){
			return  270;
		}
		
		//VERTICALS
		
		else if ((flight1.x == destination_x) && (flight1.y < destination_y)){
			return  0;
		}
		
		// Below = ((flight1.x == destination_x) && (flight1.y > destination_y))
		
		else{
			return  180;
		}

		
		
	}
	
	public static void main(String[] args) {
		
	}

}
