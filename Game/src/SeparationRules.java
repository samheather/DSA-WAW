
public class SeparationRules {
	
	//Fields

	private int warningLateralSeparation, warningVerticalSeparation; 
	private int gameOverLateralSeparation, gameOverVerticalSeparation;
	private boolean warningViolation; 
	private boolean gameOverViolation; 
		
	//Methods
	
	private static double lateralDistanceBetweenFlights(Flight flight1, Flight flight2){
		return Math.sqrt(Math.pow((flight1.x - flight2.x), 2) + Math.pow(( flight1.y - flight2.y),2));
		}
	
	private static int verticalDistanceBetweenFlights(Flight flight1, Flight flight2){
		return Math.abs(flight1.altitude - flight2.altitude);	
	}
	
	private void checkViolation(Airspace airspace1){
		for (int i = 0; i < airspace1.list_of_flights_in_airspace.length(); i++){
			for (int j = i+1; j < airspace1.list_of_flights_in_airspace.length(); j++){ // j = i + 1 : stops double checking
				if ((lateralDistanceBetweenFlights(airspace.list_of_flights_in_airspace[i], airspace.list_of_flights_in_airspace[j]) < warningLateralSeparation)
					|| (VerticalDistanceBetweenFlights(airspace.list_of_flights_in_airspace[i], airspace.list_of_flights_in_airspace[j]) < warningVerticalSeparation)) { 
					warningViolation = true; //Further action needed 
					}
				else {
					continue;
				}	
			}
		}
	}

	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
