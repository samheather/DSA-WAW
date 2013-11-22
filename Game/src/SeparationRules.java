
public class SeparationRules {
	
	//FIELDS

	private int warningLateralSeparation, warningVerticalSeparation; 
	private int gameOverLateralSeparation, gameOverVerticalSeparation;
	private boolean warningViolation; 
	private boolean gameOverViolation; 
	
	// CONSTRUCTOR
	 SeparationRules(){
		 this.warningLateralSeparation = 0; //NEED VALUES FOR THIS
		 this.warningVerticalSeparation = 0; //NEED VALUES FOR THIS
		 this.gameOverLateralSeparation = 0;
		 this.gameOverVerticalSeparation = 0;
		 this.warningViolation = false;
		 this.gameOverViolation = false;
	 }
		
	//METHODS
	
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
							|| (verticalDistanceBetweenFlights(airspace.list_of_flights_in_airspace.get(i), airspace.list_of_flights_in_airspace.get(j)) < gameOverVerticalSeparation)){
							gameOverViolation = true;
						}
						
						else {
							continue;
						}
					}
				}
				
			}
		}
	
	//MUTATORS AND ACCESSORS
	
	public void setGameOverLateralSeparation(int lateral_separation){
		this.gameOverLateralSeparation = lateral_separation;
	}
	
	public void setGameOverVerticalSeparation(int vertical_separation){
		this.gameOverVerticalSeparation = vertical_separation;
	}
	
	
	
	
	}




