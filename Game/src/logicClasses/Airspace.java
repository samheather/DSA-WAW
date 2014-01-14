package logicClasses;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.SlickException;
import org.lwjgl.input.Mouse;


//@SuppressWarnings("unused")
public class Airspace {

	// FIELDS

	private int maximumNumberOfFlightsInAirspace;
	private int score, numberOfLoopsSinceLastFlightAdded, numberOfTimesGameHasLooped,
			numberOfGameLoopsWhenDifficultyIncreases, maxRand, difficultyLevels, waypointCounter,
			exitpointCounter;
	private List<Flight> listOfFlightsInAirspace;
	private List<Waypoint> listOfWayppoints;
	private List<EntryPoint> listofEntrypoints;
	private List<ExitPoint> listOfExitPoints;
	private SeparationRules separationRules;
	private Airport airport;
	private int difficultyValueOfGame; 
	private boolean warningViolation = false; // bool outlining whether a violation is present
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
		this.difficultyValueOfGame = 2;
		this.separationRules = new SeparationRules(difficultyValueOfGame); 
		this.numberOfLoopsSinceLastFlightAdded = 0; // how many loops to wait before another flight can enter
		this.numberOfTimesGameHasLooped = 0; // stores how many loops there have been in total
		this.numberOfGameLoopsWhenDifficultyIncreases = 10000; // this is how many loops until planes come more quickly, divide by 60 for seconds
		this.maxRand = 1000;
		this.waypointCounter = 64; // Starts at 64 as this is ASCII value for A
		this.exitpointCounter = 0;
		this.controls = new Controls();
		
		
	}

	// METHODS
	
	public void resetAirspace() {
		for(int i = 0; i<this.listOfFlightsInAirspace.size();i++) {
			this.listOfFlightsInAirspace.remove(i);
		}
	}
	
	public boolean newWaypoint(int x, int y)  {
		if (x < 1250 && x > 150 && y < 650
				&& y > -50){
			this.waypointCounter++;
			Waypoint tmpWp = new Waypoint(x, y);
			
			tmpWp.setPointRef(String.valueOf((char) this.waypointCounter));
			if (this.addWaypoint(tmpWp)) {
				return true;
			}
		} return false;
	}
	public boolean newExitPoint(int x, int y) {
		if (x < 1250 && x > 100 && y < 650
				&& y > -50){
			ExitPoint tmpEp = new ExitPoint(x, y);
			this.exitpointCounter++;
			
			tmpEp.setPointRef("EXP" + this.exitpointCounter);
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
			
			if ((this.numberOfLoopsSinceLastFlightAdded >= 850  || this.listOfFlightsInAirspace.isEmpty())) {
					
				Random rand = new Random();
				int checkNumber;
					
				// A random number is generated, if that number is 1, a flight is added.
					
				if (this.listOfFlightsInAirspace.isEmpty()) {
						checkNumber = rand.nextInt(100);
				} 
					
				else {
					checkNumber = rand.nextInt(this.maxRand);
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
					this.numberOfLoopsSinceLastFlightAdded = 0;
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
		this.numberOfGameLoopsWhenDifficultyIncreases += 10000;
		if (this.maxRand - 200 > 0) {
			this.maxRand -= 200;
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
		
		this.numberOfLoopsSinceLastFlightAdded++;
		this.numberOfTimesGameHasLooped++;
		if (this.numberOfTimesGameHasLooped >= this.numberOfGameLoopsWhenDifficultyIncreases) {
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
		
		// If flight was selected, deselect it
		if (!(this.listOfFlightsInAirspace.contains(this.controls.getFlight()))) {
			this.controls.setFlight(null);

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
	public boolean getTheWarningViolation() {
		return this.warningViolation;
	}
	
	public void setTheWarningViolation(boolean updatedbool) {
		this.warningViolation = updatedbool;
	}

	/*
	public List<EntryPoint> getList_of_entrypoints() {
		return listofEntrypoints;
	}
	*/
	
	public void setListOfEntryPoints(List<EntryPoint> listOfEntryPoints) {
		this.listofEntrypoints = listOfEntryPoints;
	}
	
	public Controls getControls(){
		return this.controls;
	}
	
	public void setDifficultyValueOfGame(int i){
		this.difficultyValueOfGame = i;
		
	}
}
