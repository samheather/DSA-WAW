
public class Game {
	
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
