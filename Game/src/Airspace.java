import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.SlickException;

public class Airspace {

	//FIELDS
	
	private int max_number_of_flights;
	private int score, flight_counter, loops_since_last_flight_entry, overall_loops, next_difficulty_loops,max_rand;
	private List<Flight> list_of_flights_in_airspace, list_of_incoming_flights;
	private List<Waypoint> list_of_waypoints;	
	private List<EntryPoint> list_of_entrypoints;
	private List<ExitPoint> list_of_exitpoints;
	private SeparationRules separationRules; 
	private int flight_button_x = 30;



	
	//CONSTRUCTOR
	
	
	
	Airspace(){
		this.max_number_of_flights = 9; //just a value
		this.score = 0;
		this.list_of_flights_in_airspace = new ArrayList<Flight>();
		this.list_of_incoming_flights = new ArrayList<Flight>();
		this.list_of_waypoints = new ArrayList<Waypoint>();
		this.list_of_entrypoints = new ArrayList<EntryPoint>();
		this.list_of_exitpoints = new ArrayList<ExitPoint>();
		this.separationRules = new SeparationRules();
		this.flight_counter=0;
		this.loops_since_last_flight_entry=400;
		this.overall_loops=0;
		this.next_difficulty_loops=10000;
		this.max_rand=800;
	}
	
	//METHODS
	
	
	public boolean new_waypoint(int x, int y) {
		Waypoint tmpWp = new Waypoint(x,y);
		if(this.addWaypoint(tmpWp)){
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
	        double x;
	        double y;
	        int check_number;
	        int entryPoint=rand.nextInt(3);
	        
    		if(this.list_of_entrypoints.size()==3) { //if we have all three entrypoints
    			x = this.list_of_entrypoints.get(entryPoint).getX();//choose one a get the x and y values
    			y = this.list_of_entrypoints.get(entryPoint).getY();

    		}
    		else { //if all entrypoints are not there then just assign some values
    			x = 1250;
    			y = 300;
    			
    		}
	        
	        if(this.overall_loops>=this.next_difficulty_loops ) {
	        	this.next_difficulty_loops+=5000;
	        	if(this.max_rand>25) {
	        		this.max_rand=this.max_rand/2;
	        	}
	        }
	        System.out.println(this.max_rand);
	        if(this.list_of_flights_in_airspace.size()==0) {
	        	check_number = rand.nextInt(200);
	        }
	        else {
	        	check_number = rand.nextInt(this.max_rand);
	        }

	        if(check_number == 1){

	        	if(this.loops_since_last_flight_entry>=350) {
	        		
	        		Flight tempFlight = new Flight(this);
	        		tempFlight.setFlight_num(num);
	        		tempFlight.setX(x);
	        		tempFlight.setY(y);
	        		if(y==0) {
	        			tempFlight.setTarget_heading(180);
	        			tempFlight.setCurrent_heading(180);
	        		}else {
	        			tempFlight.setTarget_heading(270);
	        			tempFlight.setCurrent_heading(270);
	        		}
	        		this.loops_since_last_flight_entry=0;
	        		if(this.list_of_flights_in_airspace.add(tempFlight)) {
	        			this.list_of_flights_in_airspace.get(this.list_of_flights_in_airspace.size()-1).init();
	        			this.flight_button_x+=130;
	        			return true;
	        		}
	        	}

	         }

	     }
	    this.loops_since_last_flight_entry++;
	    
	    this.overall_loops++;
	    System.out.println(this.overall_loops);
	    return false;
		

	}
	public boolean new_exit_point(int x, int y) {
		ExitPoint tmpEp = new ExitPoint(x,y);
		if(this.addExitPoint(tmpEp)){
			return true;
		}
		else {
			return false;
		}
	}
	public boolean new_entry_point(int x, int y) {
		EntryPoint tmpEp = new EntryPoint(x,y);
		if(this.addEntryPoint(tmpEp)){
			return true;
		}
		else {
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
	
	
	
	// INIT, RENDER, UPDATE
	
	
	public void init() throws SlickException {

		for(int i=0; i<this.list_of_flights_in_airspace.size();i++) {
			this.list_of_flights_in_airspace.get(i).init();
		}
	}
	
	
	public void render(Graphics g) { //I added this so we can draw things in the airspace, for example a radar like background or terrain
		for(int i=0; i<this.list_of_flights_in_airspace.size();i++) {
			this.list_of_flights_in_airspace.get(i).render(g);
		}
		for(int i=0; i<this.list_of_waypoints.size();i++) {
			this.list_of_waypoints.get(i).render(g);
		}
		for(int i=0; i<this.list_of_exitpoints.size();i++) {
			this.list_of_exitpoints.get(i).render(g);
		}
		for(int i=0; i<this.list_of_entrypoints.size();i++) {
			this.list_of_entrypoints.get(i).render(g);
		}
		//this.list_of_flights_in_airspace.get(0).render(g);
	}
	
	
	public void update(GameContainer gc) {
		
		for(int i=0; i<this.list_of_flights_in_airspace.size();i++) {
			this.list_of_flights_in_airspace.get(i).update(gc);
			if(this.check_if_flight_has_left_airspace(this.getList_of_flights().get(i))) {
				this.list_of_flights_in_airspace.remove(i);
			}
			/*if(this.removed_last_loop.size()>0) {
				for(int j = 0; j<this.removed_last_loop.size();j++) {
					this.list_of_flights_in_airspace.get(i).setFlight_button_x(this.removed_last_loop.get(j));
				}
			}
			else {
				this.list_of_flights_in_airspace.get(i).setFlight_button_x(this.getList_of_flights().get(i).getFlight_button_x());
			}*/
			

		}
	}
	

	//MUTATORS AND ACCESSORS
	
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


	

}

