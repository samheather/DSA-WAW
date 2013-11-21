
public class Game {
	
	public static void main(String[] args) {
		Flight flight1 = new Flight();
		flight1.setX(10);
		flight1.setY(10);
		
		
		Flight flight2  = new Flight();
		flight2.setX(15);
		flight2.setY(15);
	
		
		
		System.out.print(Calculations.lateral_distance_between_flights(flight1, flight2));
		System.out.println();
		System.out.print(Calculations.vertical_distance_between_flights(flight1, flight2));
		System.out.println();
		System.out.print(Calculations.calculate_heading_to_first_waypoint(flight1, 10, 15));
		System.out.println();
		System.out.println(flight1.getAltitude());
		System.out.println(flight2.getAltitude());
		
		
		
		
		

	}
}
