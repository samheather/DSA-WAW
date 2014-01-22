package logicClasses;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.SlickException;



public class Airspace {

	// FIELDS

	private int maximumNumberOfFlightsInAirspace;
	private int score, numberOfGameLoopsSinceLastFlightAdded, numberOfGameLoops,
				numberOfGameLoopsWhenDifficultyIncreases, randomNumberForFlightGeneration;
	private List<Flight> listOfFlightsInAirspace;
	private List<Waypoint> listOfWayppoints;
	private List<EntryPoint> listofEntrypoints;
	private List<ExitPoint> listOfExitPoints;
	private SeparationRules separationRules;
	private Airport airport;
	private int difficultyValueOfGame; 
	private Controls controls;
	
	// CONSTRUCTOR

	public Airspace() {
		this.maximumNumberOfFlightsInAirspace = 5;
		this.score = 0;
		this.listOfFlightsInAirspace = new ArrayList<Flight>();
		this.listOfWayppoints = new ArrayList<Waypoint>();
		this.listofEntrypoints = new ArrayList<EntryPoint>();
		this.listOfExitPoints = new ArrayList<ExitPoint>();
		this.airport = new Airport();
		this.numberOfGameLoopsSinceLastFlightAdded = 0; // Stores how many loops since the last flight was spawned before another flight can enter
		this.numberOfGameLoops = 0; // Stores how many loops there have been in total
		this.numberOfGameLoopsWhenDifficultyIncreases = 3600; // this is how many loops until planes come more quickly, difficulty increase once a minute
		this.randomNumberForFlightGeneration = 500;
		this.controls = new Controls();
		this.difficultyValueOfGame = 0; // This value will be changed when the user selects a difficulty in the playstate
		
		
	}

	// METHODS
	
	/**
	 * resetAirspace: Reset all of the attributes in airspace back to default
	 */
	
	public void resetAirspace() {
		
		this.listOfFlightsInAirspace = new ArrayList<Flight>();
		
		this.numberOfGameLoopsSinceLastFlightAdded = 0; 
		this.numberOfGameLoops = 0; 
		this.numberOfGameLoopsWhenDifficultyIncreases = 3600;
		this.separationRules.setGameOverViolation(false); // Prevents user immediately entering game over state upon replay
		this.controls.setSelectedFlight(null); // Prevents information about flight from previous game being displayed 
		
		
	}
	
	/**
	 * createAndSetSeperationRules: Create and set the separation rules for the airpsace based on the difficulty value of the game
	 */
	
	public void createAndSetSeparationRules(){
		this.separationRules = new SeparationRules(difficultyValueOfGame); 
	}
	
	/**
	 * newWaypoint: Add a new waypoint to the list of waypoints in the airspace
	 * @param x The x coordinate of the waypoint
	 * @param y The y coordinate of the waypoint
	 * @param name The name used to reference the waypoint
	 */
	
	public boolean newWaypoint(int x, int y, String name)  {
		if (x < 1250 && x > 150 && y < 650 && y > -50){ // x and y must be within these bounds to be within screen space
			
			Waypoint tmpWp = new Waypoint(x, y, name);
			
			if (this.addWaypoint(tmpWp)) {
				return true;
			}
		} return false;
	}
	
	/**
	 * newExitPoint: Add a new exitpoint to the list in the airspace
	 * @param x The x coordinate of the exitpoint
	 * @param y The y coordinate of the exitpoint
	 * @param name The name used to reference the exitpoint
	 */
	public boolean newExitPoint(int x, int y, String name) {
		if (x < 1250 && x > 100 && y < 650 && y > -50){ // x and y must be within these bounds to be within screen space
			
			ExitPoint tmpEp = new ExitPoint(x, y, name);
			
			tmpEp.setPointRef("EXP" + name);
			if (this.addExitPoint(tmpEp)) {
				return true;
			}
		} return false;
	}
	
	/**
	 * newEntryPoint: Add a new entrypoint to the the list in the airspace
	 * @param x The x coordinate of the entry point
	 * @param y The y coordinate of the entrypoint
	 */
	
	public boolean newEntryPoint(int x, int y)  {
		if (x < 1250 && x > 100 && y < 650 && y > -50){
			
			EntryPoint tmpEp = new EntryPoint(x, y);
			
			if (this.addEntryPoint(tmpEp)) {
				return true;
			}
		} return false;
	}
	
	/**
	 * newFlight: Add a new flight to the list in the airspace if it has been long enough since the last flight was added and if random number is right
	 * The flight is also given a name 
	 * @param gc - Game Container
	 * @throws SlickException
	 */
	public boolean newFlight(GameContainer gc) throws SlickException {

		if (this.listOfFlightsInAirspace.size() < this.maximumNumberOfFlightsInAirspace) {
			
			if ((this.numberOfGameLoopsSinceLastFlightAdded >= 850  || this.listOfFlightsInAirspace.isEmpty())) {
					
				Random rand = new Random();
				int checkNumber;
					
				if (this.listOfFlightsInAirspace.isEmpty()) {
						checkNumber = rand.nextInt(100); // A random number (checkNumber) is generated in the range [0, 100) 
				} 
					
				else {
					checkNumber = rand.nextInt(this.randomNumberForFlightGeneration); // A random number (checkNumber) is generated in range [0, randomNumberForFlightGeneration)
				}
				
				/* 
				 * The random number is generated in the range [0, 100) if the airspace is empty, as this increases 
				 * the likelihood of a value of 1 being returned, and therefore a flight being generated; this stops the user
				 * having to potentially wait a long period of time for a flight to be generated. 
				 * If the airspace is not empty, the random number generated is in the range [0, randomNumberForFlight Generation)
				 * which is > 100. This decreases the likelihood of a flight being generated.
				 */
		
				if (checkNumber == 1) {
			
					Flight tempFlight = new Flight(this);
					tempFlight.setFlightName(this.generateFlightName());
					tempFlight.setTargetAltitude(tempFlight.getAltitude());
					double heading = tempFlight.calculateHeadingToFirstWaypoint(
										tempFlight.getFlightPlan().getPointByIndex(0).getX() ,
										tempFlight.getFlightPlan().getPointByIndex(0).getY());
					tempFlight.setTargetHeading(heading);
					tempFlight.setCurrentHeading(heading);
					this.numberOfGameLoopsSinceLastFlightAdded = 0;
					if (this.listOfFlightsInAirspace.add(tempFlight)) {
						this.listOfFlightsInAirspace.get(
								this.listOfFlightsInAirspace.size() - 1)
								.init(gc);
						return true;
					}
				}
			}
		}
		return false;
	}
	
	
	/**
	 * generateFlightName: Generate a random name for a flight, based on UK flight tail numbers
	 * @return Returns a random string that can be used to identify a flight.
	 */
	
	public String generateFlightName() {
		String name = "G-";
		Random rand = new Random();
		for (int i = 0; i < 4; i++) {
			int thisChar = rand.nextInt(10) + 65; // Generates int in range [65, 74]
			name += (char) thisChar; // Generate corresponding ascii character for int 
		}
		return name;
	}
	
	/**
	 * checkIfFlightHasLeftAirspace: Check if a flight is outside the area of the game, and if it is removed the object so it is not
	 * using unnecessary resources.
	 * @param flight The flight being checked.
	 */

	public boolean checkIfFlightHasLeftAirspace(Flight flight) {

		if (flight.getX() > 1250 || flight.getX() < 100 || flight.getY() > 650 || flight.getY() < -50) { // x and y must be within these bounds to be within screen space
			return true;
		} else {
			return false;
		}

	}

	/**
	 * changeScore: Add a value to the current score
	 * @param value the amount the score is increased by. 
	 */
	
	public void changeScore(int value) {
		this.score += value;
	}
	
	/**
	 * increaseDifficulty 
	 */
	
	public void increaseDifficulty(){
		this.numberOfGameLoopsWhenDifficultyIncreases += 3600;
		if (this.randomNumberForFlightGeneration - 50 > 0) {
			this.randomNumberForFlightGeneration -= 50;
		}
	}
	


	// INIT, RENDER, UPDATE
	
	/**
	 * init: Initialises all the resources required for the airspace class, and any other classes that are rendered within it
	 * @param gc GameContainer
	 * @throws SlickException
	 */
	
	public void init(GameContainer gc) throws SlickException {
		
		this.controls.init(gc);
		this.airport.init(gc);
		
		for (int i = 0; i < this.listOfWayppoints.size(); i++) { // Initialising waypoints
			this.listOfWayppoints.get(i).init(gc);
		}
		
		for (int i = 0; i < this.listOfExitPoints.size(); i++) { // Initailising exit points
			this.listOfExitPoints.get(i).init(gc);
		}
		
		for (int i = 0; i < this.listofEntrypoints.size(); i++) { // Initialising entry point
			this.listofEntrypoints.get(i).init(gc);
		}
		
	}
	
	/**
	 * update: Update all logic in the airspace class
	 * @param gc GameContainer
	 */
	
	public void update(GameContainer gc) {
		
		this.numberOfGameLoopsSinceLastFlightAdded++;
		this.numberOfGameLoops++;
		if (this.numberOfGameLoops >= this.numberOfGameLoopsWhenDifficultyIncreases) {
			this.increaseDifficulty();

		}
		
		
		for (int i = 0; i < this.listOfFlightsInAirspace.size(); i++) {
			this.listOfFlightsInAirspace.get(i).update();
			if(this.listOfFlightsInAirspace.get(i).getFlightPlan().getCurrentRoute().size()==0) {
				this.removeSpecificFlight(i);
			}
			else if (this.checkIfFlightHasLeftAirspace(this.getListOfFlights().get(i))) {
				this.removeSpecificFlight(i);
			}
			
		}
		
		this.separationRules.update(this);
		this.controls.update(gc, this);
		
	}
	/**
	 * render: Render all of the graphics in the airspace
	 * @param g Graphics
	 * @param gc GameContainer
	 * 
	 * @throws SlickException
	 */
	public void render(Graphics g, GameContainer gc) throws SlickException { 
		
		this.airport.render(g, gc);

		for (int i = 0; i < this.listOfWayppoints.size(); i++) { // Draws waypoints
			this.listOfWayppoints.get(i).render(g, this);
		}
		for (int i = 0; i < this.listOfExitPoints.size(); i++) { // Draws exit points
			this.listOfExitPoints.get(i).render(g, this);
		}
		for (int i = 0; i < this.listofEntrypoints.size(); i++) { // Draws entry points
			this.listofEntrypoints.get(i).render(g);
		}
		for (int i = 0; i < this.listOfFlightsInAirspace.size(); i++) { // Draws flights in airspace
			this.listOfFlightsInAirspace.get(i).render(g, gc);
		}
		
		
		this.separationRules.render(g, gc, this);
		this.controls.render(gc,g);

	}



	// MUTATORS AND ACCESSORS

	public int getMaxNumberOfFlights() {
		return this.maximumNumberOfFlightsInAirspace;
	}

	public int getScore() {
		return this.score;
	}

	public List<Flight> getListOfFlights() {
		return this.listOfFlightsInAirspace;
	}

	public List<Waypoint> getListOfWaypoints() {
		return this.listOfWayppoints;
	}

	public List<EntryPoint> getListOfEntryPoints() {
		return this.listofEntrypoints;
	}

	public List<ExitPoint> getListOfExitPoints() {
		return this.listOfExitPoints;
	}

	public void setMaxNumberOfFlights(int maxNumberOfFlights) {
		this.maximumNumberOfFlightsInAirspace = maxNumberOfFlights;
	}

	public boolean addWaypoint(Waypoint waypoint) {
		if (this.listOfWayppoints.contains(waypoint)) {
			return false;
		} else {
			this.listOfWayppoints.add(waypoint);
			return true;
		}
	}

	public boolean addEntryPoint(EntryPoint entrypoint) {
		if (this.listofEntrypoints.contains(entrypoint)) {
			return false;
		} else {
			this.listofEntrypoints.add(entrypoint);
			return true;
		}
	}

	public boolean addExitPoint(ExitPoint exitpoint) {
		if (this.listOfExitPoints.contains(exitpoint)) {
			return false;
		} else {
			this.listOfExitPoints.add(exitpoint);
			return true;
		}
	}
	public boolean addFlight(Flight flight) {

		// Checks whether the flight was already added before, and if it won't pass the maximum number of flights allowed
		if ((this.listOfFlightsInAirspace.contains(flight))
				&& (this.listOfFlightsInAirspace.size() > this.maximumNumberOfFlightsInAirspace - 1)) {
			return false;
		} else {
			this.listOfFlightsInAirspace.add(flight);
			return true;
		}
	}
	
	public void removeSpecificFlight(int flight) {
		this.listOfFlightsInAirspace.remove(flight);
		
		// If flight was selected, de-select it
		if (!(this.listOfFlightsInAirspace.contains(this.controls.getSelectedFlight()))) {
			this.controls.setSelectedFlight(null);

		}
	}

	public void removeWaypoint(Waypoint waypoint) {
		this.listOfWayppoints.remove(waypoint);
	}

	public void removeEntryPoint(EntryPoint entrypoint) {
		this.listofEntrypoints.remove(entrypoint);
	}

	public void removeExitPoint(ExitPoint exitpoint) {
		this.listOfExitPoints.remove(exitpoint);
	}

	public SeparationRules getSeparationRules(){
		return this.separationRules;
	}

	
	public void setListOfEntryPoints(List<EntryPoint> listOfEntryPoints) {
		this.listofEntrypoints = listOfEntryPoints;
	}
	
	public Controls getControls(){
		return this.controls;
	}
	
	public void setDifficultyValueOfGame(int i){
		this.difficultyValueOfGame = i;
		
	}
	
	public int getDifficultyValueOfGame(){
		return this.difficultyValueOfGame;
	}
	
	public int getNumberOfGameLoops(){
		return this.numberOfGameLoops;
	}
	
	public int getNumberOfGameLoopsWhenDifficultyIncreases(){
		return this.numberOfGameLoopsWhenDifficultyIncreases;
	}
}
