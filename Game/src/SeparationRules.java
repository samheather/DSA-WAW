
public class SeparationRules {
	
	//Fields

	private int warningLateralSeparation, warningVerticalSeparation; 
	private int gameOverLateralSeparation, gameOverVerticalSeparation;
	private boolean warningViolation; 
	private boolean gameOverViolation; 
		
	//Methods
	
	private static double lateralDistanceBetweenFlights(Flight flight1, Flight flight2){
		return Math.sqrt(Math.pow((flight1.getX() - flight2.getX()), 2) + Math.pow(( flight1.getY() - flight2.getY()),2));
		}
	
	private static int verticalDistanceBetweenFlights(Flight flight1, Flight flight2){
		return Math.abs(flight1.getAltitude() - flight2.getAltitude());	
	}
	
	private void checkViolation(Airspace airspace){
		for (int i = 0; i < airspace.list_of_flights_in_airspace.size(); i++){
			for (int j = i+1; j < airspace.list_of_flights_in_airspace.size(); j++){ // j = i + 1 : stops double checking
				
				if ((lateralDistanceBetweenFlights(airspace.list_of_flights_in_airspace.get(i), airspace.list_of_flights_in_airspace.get(j)) < warningLateralSeparation)
					|| (verticalDistanceBetweenFlights(airspace.list_of_flights_in_airspace.get(i), airspace.list_of_flights_in_airspace.get(j)) < warningVerticalSeparation)) { 
					warningViolation = true; //Further action needed 
						
					if ((lateralDistanceBetweenFlights(airspace.list_of_flights_in_airspace.get(i), airspace.list_of_flights_in_airspace.get(j)) < gameOverLateralSeparation)
							|| (verticalDistanceBetweenFlights(airspace.list_of_flights_in_airspace.get(i), airspace.list_of_flights_in_airspace.get(j)) < gameOVerVerticalSeparation)){
							gameOverViolation = true;
						}
						
						else {
							continue;
						}
					}
				}
				else {
					continue;
				}	
			}
		}
	}



}
