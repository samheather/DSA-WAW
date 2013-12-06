package logicClasses;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;


public class SeparationRules {
	
	//FIELDS

	private int warningLateralSeparation, warningVerticalSeparation; 
	private int gameOverLateralSeparation, gameOverVerticalSeparation;
	private boolean gameOverViolation; 
	
	// CONSTRUCTOR
	 SeparationRules(int difficultyVal){//Ram - Value 1 = Easy, Value 2 = Med, Value 3 = Hard
		this.warningLateralSeparation = 100; //Ram - WiP (work in progress) value.
		this.warningVerticalSeparation = 1000; //Ram - Changed to 1000ft to mirror Air Regulators
	 	this.gameOverViolation = false;
	 	
		if (difficultyVal == 1) { // Easy: Only a Crash will cause a Game Over
			this.gameOverLateralSeparation = 30;
			this.gameOverVerticalSeparation = 200;
		}
		if (difficultyVal == 2) { // Medium: Can Violate, but not too closely
			this.gameOverLateralSeparation = 30;
			this.gameOverVerticalSeparation = 350;
		 }
		 if (difficultyVal == 3) { // Hard: Warning Violation = Game Over
			this.gameOverLateralSeparation = 100;
			this.gameOverVerticalSeparation = 500;
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
		
		
		// checks if flights are in violation with each other.
		for (int i = 0; i < airspace.getList_of_flights().size(); i++){
			
			for (int j = i+1; j < airspace.getList_of_flights().size(); j++){ // j = i + 1 : stops double checking
				
						
				// Ram - Changed Game Over cond. to require BOTH vertical and lateral screw up.
				if ((lateralDistanceBetweenFlights(airspace.getList_of_flights().get(i), airspace.getList_of_flights().get(j)) < this.gameOverLateralSeparation)){
					if ((verticalDistanceBetweenFlights(airspace.getList_of_flights().get(i), airspace.getList_of_flights().get(j)) < this.gameOverVerticalSeparation)){
					this.gameOverViolation = true;
					}
				}
			}
		}
	}
	
	public void render(Graphics g, GameContainer gc, Airspace airspace){
		for (int i = 0; i < airspace.getList_of_flights().size(); i++) {
			
			for (int j = i + 1; j < airspace.getList_of_flights().size(); j++ ) {	
				
				if (this.lateralDistanceBetweenFlights(airspace.getList_of_flights().get(i), 
						airspace.getList_of_flights().get(j)) <= this.getWarningLateralSeparation()) {
					
					if (this.verticalDistanceBetweenFlights(airspace.getList_of_flights().get(i), 
							airspace.getList_of_flights().get(j)) <= this.getWarningVerticalSeparation()) {
						
						float f1x = (float) airspace.getList_of_flights().get(i).getX();
						float f1y = (float) airspace.getList_of_flights().get(i).getY();
						float f2x = (float) airspace.getList_of_flights().get(j).getX();
						float f2y = (float) airspace.getList_of_flights().get(j).getY();
						g.setColor(Color.red);
						g.setLineWidth(2);
						g.drawLine(f1x, f1y, f2x, f2y);
						g.setLineWidth(1);
						
				}}
			}
		}
		
	}
	
	public void update(GameContainer gc, Airspace airspace) {
		
		this.checkViolation(airspace);
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
	
	
	public boolean getGameOverViolation(){
		return this.gameOverViolation;
	}

}

