package logicClasses;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;


public class SeparationRules {
	
	//FIELDS

	private int warningLateralSeparation, warningVerticalSeparation; 
	private int gameOverLateralSeparation, gameOverVerticalSeparation;
	private boolean gameOverViolation; 
	
	//CONSTRUCTOR
	public SeparationRules(int difficultyVal){
		this.warningLateralSeparation = 100; 
		this.warningVerticalSeparation = 999; 
	 	this.gameOverViolation = false;
	 	
		if (difficultyVal == 1) { // Easy: Only a Crash will cause a Game Over
			this.gameOverLateralSeparation = 30;
			this.gameOverVerticalSeparation = 200;
		}
		if (difficultyVal == 2) { // Medium: Can Violate, but not too closely
			this.gameOverLateralSeparation = 60;
			this.gameOverVerticalSeparation = 350;
		 }
		 if (difficultyVal == 3) { // Hard: Minimal Warning Violation allowed before end game achieved.
			this.gameOverLateralSeparation = 90;
			this.gameOverVerticalSeparation = 500;
		 }
		 
	 }
		
	//METHODS
	
	/**
	 * lateralDistanceBetweenFlights: Calculates the lateral distance between two flights.
	 * @param flight1 - A flight from the airspace.
	 * @param flight2 - A flight from the airspace.
	 * @return A double representing the lateral distance between the two flights passed as parameters.
	 */
	
	public double lateralDistanceBetweenFlights(Flight flight1, Flight flight2){
		return Math.sqrt(Math.pow((flight1.getX() - flight2.getX()), 2) + Math.pow(( flight1.getY() - flight2.getY()),2));
		}
	
	/**
	 * verticalDistanceBetweenFlights: Calculates the vertical distance between two flights.
	 * @param flight1 - A flight from the airspace.
	 * @param flight2 - A flight from the airspace.
	 * @return An int representing the vertical distance between the two flights passed as parameters.
	 */
	
	public int verticalDistanceBetweenFlights(Flight flight1, Flight flight2){
		return Math.abs(flight1.getAltitude() - flight2.getAltitude());	
		}
	
	/**
	 * checkViolation: Calculates whether two flights have breached the game over separation rules.
	 * @param airspace - The airspace object is passed as the checkViolation() method requires knowledge of
	 * flights in the airspace, which is stored within the airspace.
	 */
	
	public void checkViolation(Airspace airspace){
		
		
		for (int i = 0; i < airspace.getListOfFlights().size(); i++){
			
			for (int j = i+1; j < airspace.getListOfFlights().size(); j++){
				
				if ((lateralDistanceBetweenFlights(airspace.getListOfFlights().get(i), airspace.getListOfFlights().get(j)) < this.gameOverLateralSeparation)){
					if ((verticalDistanceBetweenFlights(airspace.getListOfFlights().get(i), airspace.getListOfFlights().get(j)) < this.gameOverVerticalSeparation)){
					this.gameOverViolation = true;
					}
				}
			}
		}
	}
	
	/**
	 * render: This calculates whether any flights in the airspace are breaking warning separation rules
	 * If two flight are breaking warning separation rules, a line is drawn between them.
	 * @param g - Graphics libraries required by slick2d.
	 * @param gc - GameContainer required by slick2d.
	 * @param airspace - The airspace object is passed as the render method requires knowledge of
	 * flights in the airspace, which is stored within the airspace. 
	 */
	
	public void render(Graphics g, GameContainer gc, Airspace airspace){
		
		for (int i = 0; i < airspace.getListOfFlights().size(); i++) {
			
			for (int j = i + 1; j < airspace.getListOfFlights().size(); j++ ) {	
				
				if (this.lateralDistanceBetweenFlights(airspace.getListOfFlights().get(i), 
						airspace.getListOfFlights().get(j)) <= this.getWarningLateralSeparation()) {
					
					if (this.verticalDistanceBetweenFlights(airspace.getListOfFlights().get(i), 
							airspace.getListOfFlights().get(j)) <= this.getWarningVerticalSeparation()) {
						
						float f1x = (float) airspace.getListOfFlights().get(i).getX();
						float f1y = (float) airspace.getListOfFlights().get(i).getY();
						float f2x = (float) airspace.getListOfFlights().get(j).getX();
						float f2y = (float) airspace.getListOfFlights().get(j).getY();
						g.setColor(Color.red);
						g.setLineWidth(2);
						g.drawLine(f1x, f1y, f2x, f2y);
						g.setLineWidth(1);
						
				}}
			}
		}
		
	}
	
	/**
	 * update: This calls the checkViolation method to detect whether the game over separation rules
	 * have been breached.
	 * @param airspace - The airspace object is passed as the checkViolation method requires knowledge of
	 * flights in the airspace, which is stored within the airspace.
	 */
	
	public void update(Airspace airspace) {
		
		this.checkViolation(airspace);
	}
	
	
	//MUTATORS AND ACCESSORS
	
	public void setGameOverLateralSeparation(int lateralSeparation){
		this.gameOverLateralSeparation = lateralSeparation;
	}
	
	public void setGameOverVerticalSeparation(int verticalSeparation){
		this.gameOverVerticalSeparation = verticalSeparation;
	}
	
	public int getGameOverLateralSeparation(){
		return this.gameOverLateralSeparation;
	}
	
	public int getGameOverVerticalSeparation(){
		return this.gameOverVerticalSeparation;
	}
	
	public void setGameOverViolation(boolean gameOverViolation) {
		this.gameOverViolation = gameOverViolation;
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

