
public class SeparationRules {
	
	//FIELDS

	private int warningLateralSeparation, warningVerticalSeparation; 
	private int gameOverLateralSeparation, gameOverVerticalSeparation;
	private boolean warningViolation;
	private boolean gameOverViolation; 
	
	// CONSTRUCTOR
	 SeparationRules(){
		 this.warningLateralSeparation = 50; //NEED VALUES FOR THIS
		 this.warningVerticalSeparation = 50; //NEED VALUES FOR THIS
		 this.gameOverLateralSeparation = 30;
		 this.gameOverVerticalSeparation = 30;
		 this.gameOverViolation = false;
		 
	 }
		
	//METHODS
	
	private static double lateralDistanceBetweenFlights(Flight flight1, Flight flight2){
		return Math.sqrt(Math.pow((flight1.getX() - flight2.getX()), 2) + Math.pow(( flight1.getY() - flight2.getY()),2));
		}
	
	private static int verticalDistanceBetweenFlights(Flight flight1, Flight flight2){
		return Math.abs(flight1.getAltitude() - flight2.getAltitude());	
		}
	
	public void checkViolation(Airspace airspace){
		
		// resetting all the flights to false
		
		for (int i = 0; i < airspace.getList_of_flights().size(); i++){
			airspace.getList_of_flights().get(i).set_warning_violation(false);
		}
		
		
		for (int i = 0; i < airspace.getList_of_flights().size(); i++){
			
			for (int j = i+1; j < airspace.getList_of_flights().size(); j++){ // j = i + 1 : stops double checking
				
				if ((lateralDistanceBetweenFlights(airspace.getList_of_flights().get(i), airspace.getList_of_flights().get(j)) < this.warningLateralSeparation)){
					airspace.getList_of_flights().get(i).set_warning_violation(true);
					airspace.getList_of_flights().get(j).set_warning_violation(true);
				}
				
				else if ((verticalDistanceBetweenFlights(airspace.getList_of_flights().get(i), airspace.getList_of_flights().get(j)) < this.warningVerticalSeparation)){
					airspace.getList_of_flights().get(i).set_warning_violation(true);
					airspace.getList_of_flights().get(j).set_warning_violation(true);
				}
						
				if ((lateralDistanceBetweenFlights(airspace.getList_of_flights().get(i), airspace.getList_of_flights().get(j)) < this.gameOverLateralSeparation)){
					this.gameOverViolation = true;
						}
				
				else if ((verticalDistanceBetweenFlights(airspace.getList_of_flights().get(i), airspace.getList_of_flights().get(j)) < this.gameOverVerticalSeparation)){
					this.gameOverViolation = true;
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
	
	public int getGameOverLateralSeparation(){
		return this.gameOverLateralSeparation;
	}
	
	public int getGameOverVerticalSeparation(){
		return this.gameOverVerticalSeparation;
	}
	
	public boolean getWarningViolation(){
		return this.warningViolation;
	}
	
	public boolean getGameOverViolation(){
		return this.gameOverViolation;
	}

}

