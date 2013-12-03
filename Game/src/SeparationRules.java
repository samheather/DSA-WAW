
public class SeparationRules {
	
	//FIELDS

	private int warningLateralSeparation, warningVerticalSeparation; 
	private int gameOverLateralSeparation, gameOverVerticalSeparation;
	private boolean warningViolation;
	private boolean gameOverViolation; 
	
	// CONSTRUCTOR
	 SeparationRules(int difficultyVal){//Ram - Value 1 = Easy, Value 2 = Med, Value 3 = Hard
		this.warningLateralSeparation = 100; //Ram - WiP (work in progress) value.
		this.warningVerticalSeparation = 1000; //Ram - Changed to 1000ft to mirror Air Regulators
	 	this.gameOverViolation = false;
	 	
		if (difficultyVal == 1) { // Easy: Only a Crash will cause a Game Over
			this.gameOverLateralSeparation = 30;
			this.gameOverVerticalSeparation = 750;
		}
		if (difficultyVal == 2) { // Medium: Can Violate, but not too closely
			this.gameOverLateralSeparation = 30;
			this.gameOverVerticalSeparation = 750;
		 }
		 if (difficultyVal == 3) { // Hard: Warning Violation = Game Over
			this.gameOverLateralSeparation = 100;
			this.gameOverVerticalSeparation = 1000;
		 }
		 
	 }
		
	//METHODS
		
	public double lateralDistanceBetweenFlights(Flight flight1, Flight flight2){
		return Math.sqrt(Math.pow((flight1.getX() - flight2.getX()), 2) + Math.pow(( flight1.getY() - flight2.getY()),2));
		}
	
	public int verticalDistanceBetweenFlights(Flight flight1, Flight flight2){
		return Math.abs(flight1.getAltitude() - flight2.getAltitude());	
		}
	
	public void checkViolation(Airspace airspace){
		
		// resetting all the flights to false
		for (int i = 0; i < airspace.getList_of_flights().size(); i++){
			airspace.getList_of_flights().get(i).set_warning_violation(false);
		}
		
		// checks if flights are in violation with each other.
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
						
				// Ram - Changed Game Over cond. to require BOTH vertical and lateral screw up.
				if ((lateralDistanceBetweenFlights(airspace.getList_of_flights().get(i), airspace.getList_of_flights().get(j)) < this.gameOverLateralSeparation)){
					if ((verticalDistanceBetweenFlights(airspace.getList_of_flights().get(i), airspace.getList_of_flights().get(j)) < this.gameOverVerticalSeparation)){
					this.gameOverViolation = true;
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
	
	public int getGameOverLateralSeparation(){
		return this.gameOverLateralSeparation;
	}
	
	public int getGameOverVerticalSeparation(){
		return this.gameOverVerticalSeparation;
	}
	
	public int getWarningLateralSeparation() {
		return this.warningLateralSeparation;
	}
	
	public int getWarningVerticalSeparation(){
		return this.warningVerticalSeparation;
	}
	
	public boolean getWarningViolation(){
		return this.warningViolation;
	}
	
	public boolean getGameOverViolation(){
		return this.gameOverViolation;
	}

}

