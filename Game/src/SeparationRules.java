
public class SeparationRules {
	
	//FIELDS

	private int warningLateralSeparation, warningVerticalSeparation; 
	private int gameOverLateralSeparation, gameOverVerticalSeparation;
	private boolean warningLateralViolation;
	private boolean warningVerticalViolation;
	private boolean gameOverViolation; 
	
	// CONSTRUCTOR
	 SeparationRules(){
		 this.warningLateralSeparation = 50; //NEED VALUES FOR THIS
		 this.warningVerticalSeparation = 50; //NEED VALUES FOR THIS
		 this.gameOverLateralSeparation = 30;
		 this.gameOverVerticalSeparation = 30;
		 this.warningLateralViolation = false;
		 this.warningVerticalViolation = false;
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
		for (int i = 0; i < airspace.getList_of_flights().size(); i++){
			for (int j = i+1; j < airspace.getList_of_flights().size(); j++){ // j = i + 1 : stops double checking
				
				if ((lateralDistanceBetweenFlights(airspace.getList_of_flights().get(i), airspace.getList_of_flights().get(j)) < this.warningLateralSeparation)){
					this.warningLateralViolation = true;
				}
				
				else if ((verticalDistanceBetweenFlights(airspace.getList_of_flights().get(i), airspace.getList_of_flights().get(j)) < this.warningVerticalSeparation)){
					this.warningVerticalViolation = true;
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
	
	public boolean getWarningLateralSeparation(){
		return this.warningLateralViolation;
	}
	
	public boolean getWarningVerticalSeparation(){
		return this.warningVerticalViolation;
	}
	
	public boolean getGameOverViolation(){
		return this.gameOverViolation;
	}

}

