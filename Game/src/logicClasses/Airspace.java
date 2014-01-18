package logicClasses;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.SlickException;


//@SuppressWarnings("unused")
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
		this.numberOfGameLoopsSinceLastFlightAdded = 0; // how many loops to wait before another flight can enter
		this.numberOfGameLoops = 0; // stores how many loops there have been in total
		this.numberOfGameLoopsWhenDifficultyIncreases = 3600; // this is how many loops until planes come more quickly, difficulty increase once a minute
		this.randomNumberForFlightGeneration = 500;
		this.controls = new Controls();
		this.difficultyValueOfGame = 0; // This value will be changed when the user selects a difficulty in the playstate
		
		
	}

	// METHODS
	
	public void resetAirspace() {
		
		this.listOfFlightsInAirspace = new ArrayList<Flight>();
		
		this.numberOfGameLoopsSinceLastFlightAdded = 0; // how many loops to wait before another flight can enter
		this.numberOfGameLoops = 0; // stores how many loops there have been in total
		this.numberOfGameLoopsWhenDifficultyIncreases = 3600; // this is how many loops until planes come more quickly, divide by 60 for seconds
		this.separationRules.setGameOverViolation(false);
		this.controls.setSelectedFlight(null);
		
		
	}
	
	public void createAndSetSeparationRules(){
		this.separationRules = new SeparationRules(difficultyValueOfGame); 
	}
	
	public boolean newWaypoint(int x, int y, String name)  {
		if (x < 1250 && x > 150 && y < 650
				&& y > -50){
			
			Waypoint tmpWp = new Waypoint(x, y, name);
			
			if (this.addWaypoint(tmpWp)) {
				return true;
			}
		} return false;
	}
	public boolean newExitPoint(int x, int y, String name) {
		if (x < 1250 && x > 100 && y < 650
				&& y > -50){
			ExitPoint tmpEp = new ExitPoint(x, y, name);
			
			tmpEp.setPointRef("EXP" + name);
			if (this.addExitPoint(tmpEp)) {
				return true;
			}
		} return false;
	}

	public boolean newEntryPoint(int x, int y)  {
		if (x < 1250 && x > 100 && y < 650
				&& y > -50){
			EntryPoint tmpEp = new EntryPoint(x, y);
			if (this.addEntryPoint(tmpEp)) {
				return true;
			}
		} return false;
	}
	
	
	public boolean newFlight(GameContainer gc) throws SlickException {

		if (this.listOfFlightsInAirspace.size() < this.maximumNumberOfFlightsInAirspace) {
			
			if ((this.numberOfGameLoopsSinceLastFlightAdded >= 850  || this.listOfFlightsInAirspace.isEmpty())) {
					
				Random rand = new Random();
				int checkNumber;
					
				// A random number is generated, if that number is 1, a flight is added.
					
				if (this.listOfFlightsInAirspace.isEmpty()) {
						checkNumber = rand.nextInt(100);
				} 
					
				else {
					checkNumber = rand.nextInt(this.randomNumberForFlightGeneration);
				}
		
				if (checkNumber == 1) {
			
					Flight tempFlight = new Flight(this);
					//boolean isInViolation = this.separationRules.lateralDistanceBetweenFlights(flight1, flight2)
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
	
	
	public String generateFlightName() {
		String name = "G-";
		Random rand = new Random();
		for (int i = 0; i < 4; i++) {
			int thisChar = rand.nextInt(10) + 65;
			name += (char) thisChar;
		}
		return name;
	}

	public boolean checkIfFlightHasLeftAirspace(Flight flight) {

		if (flight.getX() > 1250 || flight.getX() < 100 || flight.getY() > 650
				|| flight.getY() < -50) {
			return true;
		} else {
			return false;
		}

	}


	public void changeScore(int value) {
		this.score += value;
	}
	
	public void increaseDifficulty(){
		this.numberOfGameLoopsWhenDifficultyIncreases += 3600;
		if (this.randomNumberForFlightGeneration - 50 > 0) {
			this.randomNumberForFlightGeneration -= 50;
		}
	}
	


	// INIT, RENDER, UPDATE

	public void init(GameContainer gc) throws SlickException {
		
		this.controls.init(gc);
		this.airport.init(gc);
		
		for (int i = 0; i < this.listOfWayppoints.size(); i++) {
			this.listOfWayppoints.get(i).init(gc);
		}
		
		for (int i = 0; i < this.listOfExitPoints.size(); i++) {
			this.listOfExitPoints.get(i).init(gc);
		}
		
		for (int i = 0; i < this.listofEntrypoints.size(); i++) {
			this.listofEntrypoints.get(i).init(gc);
		}
		
	}
	

	
	public void update(GameContainer gc) {
		
		this.numberOfGameLoopsSinceLastFlightAdded++;
		this.numberOfGameLoops++;
		if (this.numberOfGameLoops >= this.numberOfGameLoopsWhenDifficultyIncreases) {
			this.increaseDifficulty();

		}
		
		
		for (int i = 0; i < this.listOfFlightsInAirspace.size(); i++) {
			this.listOfFlightsInAirspace.get(i).update();
			if(this.listOfFlightsInAirspace.get(i).getFlightPlan().getWaypoints().size()==0) {
				this.removeSpecificFlight(i);
			}
			else if (this.checkIfFlightHasLeftAirspace(this.getListOfFlights().get(i))) { // if a flight has left the airspace
				this.removeSpecificFlight(i);
			}
			
		}
		
		this.separationRules.update(this);
		this.controls.update(gc, this);
		
	}

	public void render(Graphics g, GameContainer gc) throws SlickException { 
		
		this.airport.render(g, gc);

		for (int i = 0; i < this.listOfWayppoints.size(); i++) {
			this.listOfWayppoints.get(i).render(g, this);
		}
		for (int i = 0; i < this.listOfExitPoints.size(); i++) {
			this.listOfExitPoints.get(i).render(g, this);
		}
		for (int i = 0; i < this.listofEntrypoints.size(); i++) {
			this.listofEntrypoints.get(i).render(g);
		}
		for (int i = 0; i < this.listOfFlightsInAirspace.size(); i++) {
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
		 // I made them boolean so we can check if the plane was added successfully (we can change them later on)
	}
	
	public void removeSpecificFlight(int flight) {
		this.listOfFlightsInAirspace.remove(flight); // remove that flight from the list
		
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
}
