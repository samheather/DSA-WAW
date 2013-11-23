import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.SlickException;

public class Airspace {

	//FIELDS
	
	int max_number_of_flights;
	int score, flight_counter, wp_counter, exp_counter, ep_counter, entry_rate;
	List<Flight> list_of_flights_in_airspace, list_of_incoming_flights;
	List<Waypoint> list_of_waypoints;	
	List<EntryPoint> list_of_entrypoints;
	List<ExitPoint> list_of_exitpoints;
	SeparationRules separationRules; 
	int flight_button_x = 30;

	
	//CONSTRUCTOR
	
	
	Airspace(){
		this.max_number_of_flights = 20; //just a value
		this.score = 0;
		this.list_of_flights_in_airspace = new ArrayList<Flight>();
		this.list_of_incoming_flights = new ArrayList<Flight>();
		this.list_of_waypoints = new ArrayList<Waypoint>();
		this.list_of_entrypoints = new ArrayList<EntryPoint>();
		this.list_of_exitpoints = new ArrayList<ExitPoint>();
		this.separationRules = new SeparationRules();
		this.flight_counter=0;
		this.wp_counter=0;
		this.ep_counter=0;
		this.exp_counter=0;
		this.entry_rate=200;
	}
	
	//METHODS
	
	public void new_flight(int num) {
		Random rand=new Random();
		Flight tempFlight = new Flight(this);
		tempFlight.setX(-100);
		tempFlight.setY(-100);
		tempFlight.setFlight_num(num);
		int entryNum=rand.nextInt(this.entry_rate)+this.entry_rate;
		if(this.flight_counter<4) {
			this.entry_rate+=500;
		}
		else if(this.flight_counter>=4&&this.flight_counter<=10) {
			this.entry_rate+=500;
		}
		tempFlight.setEntryNum(entryNum);
		add_flight(tempFlight);
			this.flight_counter++;
			this.flight_button_x+=130;


		
	}
	
	public boolean new_waypoint(int x, int y) {
		Waypoint tmpWp = new Waypoint(x,y);
		if(this.addWaypoint(tmpWp)){
			this.wp_counter++;
			return true;
		}
		else {
			return false;
		}
	}
	public boolean check_if_flight_has_left_airspace(Flight f) {
		
		if(f.getX()>1250||f.getX()<-50||f.getY()>650||f.getY()<-50) {
			return true;
		}
		else {
			return false;
		}
		
	}
	public boolean new_flight2(int num) throws SlickException {

	    if (this.list_of_flights_in_airspace.size() < this.max_number_of_flights){
	        Random rand=new Random();
	        int check_number = rand.nextInt(500);

	        if(check_number == 150){
	            Flight tempFlight = new Flight(this);
	            tempFlight.setFlight_num(num);
	            tempFlight.setX(600);
	            tempFlight.setY(600);
	            

	            if(this.list_of_flights_in_airspace.add(tempFlight)) {
	            	this.list_of_flights_in_airspace.get(this.list_of_flights_in_airspace.size()-1).init();
	            	this.flight_button_x+=130;
	            	return true;
	            }
	         }

	     }
	    return false;
		

	}
	public boolean new_exit_point(int x, int y) {
		ExitPoint tmpEp = new ExitPoint(x,y);
		if(this.addExitPoint(tmpEp)){
			this.exp_counter++;
			return true;
		}
		else {
			return false;
		}
	}
	public boolean new_entry_point(int x, int y) {
		EntryPoint tmpEp = new EntryPoint(x,y);
		if(this.addEntryPoint(tmpEp)){
			this.ep_counter++;
			return true;
		}
		else {
			return false;
		}
	}


	
	public boolean add_flight(Flight flight){ 	
	
		//Checks whether the flight was already added before, and if it won't pass the maximum number of flights allowed	
		if ((this.list_of_flights_in_airspace.contains(flight)) 
				&& (this.list_of_flights_in_airspace.size() > this.max_number_of_flights-1)){    
			return false;
		} else {
			this.list_of_flights_in_airspace.add(flight);
			return true;
		}/* I made them boolean so we can check if the plane was added successfully 
	    (we can change them later on) */
	}
	public boolean check_flight(Flight flight){
		return this.list_of_flights_in_airspace.contains(flight);
	}
	
	public boolean remove_flight(Flight flight){
		if (this.list_of_flights_in_airspace.contains(flight)){
			this.list_of_flights_in_airspace.remove(flight);
			return true;
		} else {
			return false;
			}
		}
	
	public void setDifficulty(int difficulty){
        switch (difficulty) {
        case 1: separationRules.setGameOverLateralSeparation(50);
			    separationRules.setGameOverVerticalSeparation(50);
			    break;
        case 2: separationRules.setGameOverLateralSeparation(25);
	    		separationRules.setGameOverVerticalSeparation(25);
	    		break;
        case 3:	separationRules.setGameOverLateralSeparation(10);
	    		separationRules.setGameOverVerticalSeparation(10);
	    		break;
		}
	}
	
	public void changeScore(int value){
		this.score += value;
	}
	
	public void setMax_number_of_flights(int max_number_of_flights) {
		this.max_number_of_flights = max_number_of_flights;
	}

	public boolean addWaypoint(Waypoint waypoint) {
		if (this.list_of_waypoints.contains(waypoint)){
			return false;
		} else {
			this.list_of_waypoints.add(waypoint);
			return true;
			}
	}

	public boolean addEntryPoint(EntryPoint entrypoint) {
		if (this.list_of_entrypoints.contains(entrypoint)){
			return false;
		} else {
			this.list_of_entrypoints.add(entrypoint);
			return true;
			}
	}

	public boolean addExitPoint(ExitPoint exitpoint) {
		if (this.list_of_exitpoints.contains(exitpoint)){
			return false;
		} else {
			this.list_of_exitpoints.add(exitpoint);
			return true;
		}
	}
	
	public void removeWaypoint(Waypoint waypoint) {
		this.list_of_waypoints.remove(waypoint);
	}

	public void removeEntryPoint(EntryPoint entrypoint) {
		this.list_of_entrypoints.remove(entrypoint);
	}

	public void removeExitPoint(ExitPoint exitpoint) {
		this.list_of_exitpoints.remove(exitpoint);
	}
	// render, update and init
	public void init() throws SlickException {

		for(int i=0; i<this.list_of_flights_in_airspace.size();i++) {
			this.list_of_flights_in_airspace.get(i).init();
		}
	}
	public void render(Graphics g) { //I added this so we can draw things in the airspace, for example a radar like background or terrain
		for(int i=0; i<this.list_of_flights_in_airspace.size();i++) {
			this.list_of_flights_in_airspace.get(i).render(g);
		}
		for(int i=0; i<wp_counter;i++) {
			this.list_of_waypoints.get(i).render(g);
		}
		for(int i=0; i<exp_counter;i++) {
			this.list_of_exitpoints.get(i).render(g);
		}
		//this.list_of_flights_in_airspace.get(0).render(g);
	}
	public void update(GameContainer gc) {

		for(int i=0; i<this.list_of_flights_in_airspace.size();i++) {
			this.list_of_flights_in_airspace.get(i).update(gc);
			if(this.check_if_flight_has_left_airspace(this.getList_of_flights().get(i))) {
				this.list_of_flights_in_airspace.remove(i);	
			}

		}
	}

	//GETTERS & SETTERS
	
	public int getMax_number_of_flights() {
		return this.max_number_of_flights;
	}

	public int getScore() {
		return this.score;
	}

	public List<Flight> getList_of_flights() {
		return this.list_of_flights_in_airspace;
	}

	public List<Waypoint> getList_of_way_points() {
		return this.list_of_waypoints;
	}

	public List<EntryPoint> getList_of_entry_points() {
		return this.list_of_entrypoints;
	}

	public List<ExitPoint> getList_of_exit_points() {
		return this.list_of_exitpoints;
	}
	
	public void add_to_list_of_incoming_flights(Flight flight){
		this.list_of_incoming_flights.add(flight);
	}

	public int getFlight_button_x() {
		return flight_button_x;
	}

	public void setFlight_button_x(int flight_button_x) {
		this.flight_button_x = flight_button_x;
	}

}

