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

	private int max_number_of_flights;
	private int score, loops_since_last_flight_entry, overall_loops,
			next_difficulty_loops, max_rand, difficulty_levels, wp_counter,
			exp_counter;
	private List<Flight> list_of_flights_in_airspace, list_of_incoming_flights;
	private List<Waypoint> list_of_waypoints;
	private List<EntryPoint> list_of_entrypoints;
	private List<ExitPoint> list_of_exitpoints;
	private SeparationRules separationRules;
	private Flight selected_flight;
	private Airport airport;
	private int game_difficulty_value; // !! Should be fetched by Difficulty Screen, currently fails to do so
	private boolean warningViolation = false; // bool outlining whether a violation is present
	private Controls controls;
	
	// CONSTRUCTOR

	public Airspace() {
		this.max_number_of_flights = 5;
		this.score = 0;
		this.list_of_flights_in_airspace = new ArrayList<Flight>();
		this.list_of_waypoints = new ArrayList<Waypoint>();
		this.list_of_entrypoints = new ArrayList<EntryPoint>();
		this.list_of_exitpoints = new ArrayList<ExitPoint>();
		this.airport = new Airport();
		this.game_difficulty_value = 2;
		this.separationRules = new SeparationRules(game_difficulty_value); 
		this.loops_since_last_flight_entry = 400; // how many loops to wait before another flight can enter
		this.overall_loops = 0; // stores how many loops there have been in total
		this.next_difficulty_loops = 10000; // this is how many loops until planes come more quickly, divide by 60 for seconds
		this.difficulty_levels = 10;// number of times difficulty changes
		this.max_rand = (int) Math.pow(2, this.difficulty_levels);
		this.wp_counter = 64;
		this.exp_counter = 0;
		this.selected_flight = null;
		this.controls = new Controls();
		
		
	}

	// METHODS
	
	public void reset_airspace() {
		this.selected_flight=null;
		for(int i = 0; i<this.list_of_flights_in_airspace.size();i++) {
			this.list_of_flights_in_airspace.remove(i);
		}
	}
	
	public boolean new_waypoint(int x, int y)  {
		if (x < 1250 && x > 150 && y < 650
				&& y > -50){
			this.wp_counter++;
			Waypoint tmpWp = new Waypoint(x, y);
			
			tmpWp.setPointRef(String.valueOf((char) this.wp_counter));
			if (this.addWaypoint(tmpWp)) {
				return true;
			}
		} return false;
	}
	public boolean new_exit_point(int x, int y) {
		if (x < 1250 && x > 100 && y < 650
				&& y > -50){
			ExitPoint tmpEp = new ExitPoint(x, y);
			this.exp_counter++;
			
			tmpEp.setPointRef("EXP" + this.exp_counter);
			if (this.addExitPoint(tmpEp)) {
				return true;
			}
		} return false;
	}

	public boolean new_entry_point(int x, int y)  {
		if (x < 1250 && x > 100 && y < 650
				&& y > -50){
			EntryPoint tmpEp = new EntryPoint(x, y);
			if (this.addEntryPoint(tmpEp)) {
				return true;
			}
		} return false;
	}
	
	
	public boolean new_flight(GameContainer gc) throws SlickException {

		if (this.list_of_flights_in_airspace.size() < this.max_number_of_flights) {
			Random rand = new Random();

			int check_number;
			
			if (this.list_of_flights_in_airspace.isEmpty()) {
				check_number = rand.nextInt(100);
			} else {
				check_number = rand.nextInt(this.max_rand);
			}

			if (check_number == 1) {
				
				if ((this.loops_since_last_flight_entry >= 1000  || this.list_of_flights_in_airspace.isEmpty())) {

					Flight tempFlight = new Flight(this);
					//boolean isInViolation = this.separationRules.lateralDistanceBetweenFlights(flight1, flight2)
					tempFlight.setFlight_name(this.generate_flight_name());
					tempFlight.setTarget_altitude(tempFlight.getCurrent_altitude());
					double heading = tempFlight.calculate_heading_to_first_waypoint(
									tempFlight.getFlight_plan().getPointByIndex(0).getX() ,
									tempFlight.getFlight_plan().getPointByIndex(0).getY());
					tempFlight.setTarget_heading(heading);
					tempFlight.setCurrent_heading(heading);
					this.loops_since_last_flight_entry = 0;
					if (this.list_of_flights_in_airspace.add(tempFlight)) {
						this.list_of_flights_in_airspace.get(
								this.list_of_flights_in_airspace.size() - 1)
								.init(gc);
						
						return true;
					}
				}

			}

		}

		return false;

	}
	
	
	public String generate_flight_name() {
		String name = "G-";
		Random rand = new Random();
		for (int i = 0; i < 4; i++) {
			int thisChar = rand.nextInt(10) + 65;
			name += (char) thisChar;
		}
		return name;
	}

	public boolean check_if_flight_has_left_airspace(Flight flight) {

		if (flight.getX() > 1250 || flight.getX() < 100 || flight.getY() > 650
				|| flight.getY() < -50) {
			return true;
		} else {
			return false;
		}

	}

	public void check_selected(int pointX, int pointY, Airspace airspace ){
		double min_distance;
		Flight nearest_flight;
		int index_of_nearest_flight;
		
		pointY = 600-pointY; // Translating Mouse coordinates onto object coordinates
		
		// Working out nearest flight to click
		
		if(airspace.getList_of_flights().size()>=1){
			min_distance = Math.sqrt(Math.pow(pointX-airspace.getList_of_flights().get(0).getX(), 2)+Math.pow(pointY-airspace.getList_of_flights().get(0).getY(), 2));
			nearest_flight = airspace.getList_of_flights().get(0);
			index_of_nearest_flight = 0;
			
			for (int i =0; i< airspace.getList_of_flights().size(); i++){
				if(Math.sqrt(Math.pow(pointX-airspace.getList_of_flights().get(i).getX(), 2)+Math.pow(pointY-airspace.getList_of_flights().get(i).getY(), 2)) < min_distance){
					min_distance = Math.sqrt(Math.pow(pointX-airspace.getList_of_flights().get(i).getX(), 2)+Math.pow(pointY-airspace.getList_of_flights().get(i).getY(), 2));
					nearest_flight = airspace.getList_of_flights().get(i);
					index_of_nearest_flight = i;
				}
			}
			
			// Working out whether the nearest flight to click is close enough
			// to be selected.
			
			if (min_distance <= 50){
				nearest_flight.setSelected(true);
				airspace.set_selected_flight(nearest_flight);
				this.controls.setFlight(nearest_flight);
				for (int i =0; i< airspace.getList_of_flights().size(); i++){
					if(i != index_of_nearest_flight){
						airspace.getList_of_flights().get(i).setSelected(false);
					}
				}
			}
		}
	}
	
	public void give_heading_with_mouse(int pointX, int pointY, Airspace airspace){
		
		double deltaX, deltaY;
		double distance_between_mouse_and_plane;
		pointY = 600-pointY;
		
		distance_between_mouse_and_plane = Math.sqrt(Math.pow(pointX-airspace.get_selected_flight().getX(), 2)+Math.pow(pointY-airspace.get_selected_flight().getY(), 2));
		System.out.println(distance_between_mouse_and_plane);
		
		if (distance_between_mouse_and_plane < 50)
		{
			deltaY = pointY - airspace.get_selected_flight().getY();
			deltaX = pointX - airspace.get_selected_flight().getX();
			double angle = Math.toDegrees(Math.atan2(deltaY, deltaX));
			angle+=90;
			if (angle < 0) {
				angle += 360;
			}
			airspace.get_selected_flight().give_heading((int)angle);
		
		}
		
	}

	

	public void changeScore(int value) {
		this.score += value;
	}
	
	public void update_controls(Airspace airspace, GameContainer gc){
		
		
		// Update controls
		this.controls.update(gc);
		this.controls.setIncrease_alt((int)Math.round(this.selected_flight.getTarget_altitude())+1000);
		this.controls.setDecrease_alt((int)Math.round(this.selected_flight.getTarget_altitude())-1000);
		this.controls.setTarget_alt((int)Math.round(this.selected_flight.getTarget_altitude()));
		if (!this.controls.headingHasFocus()) {
			this.controls.getHeadingControlTB().setText(
					String.valueOf(Math.round(this.selected_flight.getTarget_heading())));
		}
		if(this.controls.isIncrease_alt_clicked()) {
			this.selected_flight.setTarget_altitude(this.selected_flight.getTarget_altitude()+1000);
			System.out.println(this.selected_flight.getTarget_altitude());
		}
		if(this.controls.isDecrease_alt_clicked()) {
			this.selected_flight.setTarget_altitude(this.selected_flight.getTarget_altitude()-1000);
			System.out.println(this.selected_flight.getTarget_altitude());
		}
			
			this.controls.allow_all();
		
	}		
			


	// INIT, RENDER, UPDATE

	public void init(GameContainer gc) throws SlickException {
		this.controls.init(gc);
		
	}
	
	public void update(GameContainer gc) {
		this.loops_since_last_flight_entry++;
		this.overall_loops++;
		if (this.overall_loops >= this.next_difficulty_loops) {
			this.next_difficulty_loops += 10000;
			if (this.max_rand > 2) {
				this.max_rand = this.max_rand / 2;
			}

		}
		
		int posX=Mouse.getX();
		int posY=Mouse.getY();
		
		if(Mouse.isButtonDown(0)){
			this.check_selected(posX,posY,this);
		}
		
		if(Mouse.isButtonDown(1)){
			if (this.get_selected_flight()!= null){
			this.give_heading_with_mouse(posX, posY,this );
			}
		}
		
		
		for (int i = 0; i < this.list_of_flights_in_airspace.size(); i++) {
			this.list_of_flights_in_airspace.get(i).update();
			if(this.list_of_flights_in_airspace.get(i).getFlight_plan().getWaypoints().size()==0) {
				this.remove_specific_flight(i);
			}
			else if (this.check_if_flight_has_left_airspace(this.getList_of_flights().get(i))) { // if a flight has left the airspace
				this.remove_specific_flight(i);
			}
			
		}
		
		this.separationRules.update(this);
		
		if (this.selected_flight != null){
			this.update_controls(this, gc);
		}
	}

	public void render(Graphics g, GameContainer gc) throws SlickException { 
		
		this.airport.render(g, gc);

		for (int i = 0; i < this.list_of_waypoints.size(); i++) {
			this.list_of_waypoints.get(i).render(g, this);
		}
		for (int i = 0; i < this.list_of_exitpoints.size(); i++) {
			this.list_of_exitpoints.get(i).render(g, this);
		}
		for (int i = 0; i < this.list_of_entrypoints.size(); i++) {
			this.list_of_entrypoints.get(i).render(g);
		}
		for (int i = 0; i < this.list_of_flights_in_airspace.size(); i++) {
			this.list_of_flights_in_airspace.get(i).render(g, gc);
		}
		
		
		this.separationRules.render(g, gc, this);
		this.controls.render(gc,g);

	}



	// MUTATORS AND ACCESSORS

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

	public void add_to_list_of_incoming_flights(Flight flight) {
		this.list_of_incoming_flights.add(flight);
	}


	public void setMax_number_of_flights(int max_number_of_flights) {
		this.max_number_of_flights = max_number_of_flights;
	}

	public boolean addWaypoint(Waypoint waypoint) {
		if (this.list_of_waypoints.contains(waypoint)) {
			return false;
		} else {
			this.list_of_waypoints.add(waypoint);
			return true;
		}
	}

	public boolean addEntryPoint(EntryPoint entrypoint) {
		if (this.list_of_entrypoints.contains(entrypoint)) {
			return false;
		} else {
			this.list_of_entrypoints.add(entrypoint);
			return true;
		}
	}

	public boolean addExitPoint(ExitPoint exitpoint) {
		if (this.list_of_exitpoints.contains(exitpoint)) {
			return false;
		} else {
			this.list_of_exitpoints.add(exitpoint);
			return true;
		}
	}
	public boolean add_flight(Flight flight) {

		// Checks whether the flight was already added before, and if it won't pass the maximum number of flights allowed
		if ((this.list_of_flights_in_airspace.contains(flight))
				&& (this.list_of_flights_in_airspace.size() > this.max_number_of_flights - 1)) {
			return false;
		} else {
			this.list_of_flights_in_airspace.add(flight);
			return true;
		}
		 // I made them boolean so we can check if the plane was added successfully (we can change them later on)
	}
	
	public void remove_specific_flight(int flight) {
		this.list_of_flights_in_airspace.remove(flight); // remove that flight from the list
		
		this.controls.setIncrease_alt(0);
		this.controls.setDecrease_alt(0);
		this.controls.setTarget_alt(0);
	
		this.controls.getHeadingControlTB().setText("");
													
		
		
		if (!(this.list_of_flights_in_airspace.contains(this.selected_flight))) {
			this.selected_flight = null;

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


	public void set_selected_flight(Flight flight) {
		this.selected_flight = flight;
	}

	public Flight get_selected_flight() {
		return this.selected_flight;

	}
	
	public SeparationRules get_separation_rules(){
		return this.separationRules;
	}
	public boolean get_the_warning_violation() {
		return this.warningViolation;
	}
	
	public void set_the_warning_violation(boolean updatedbool) {
		this.warningViolation = updatedbool;
	}

	public List<EntryPoint> getList_of_entrypoints() {
		return list_of_entrypoints;
	}

	public void setList_of_entrypoints(List<EntryPoint> list_of_entrypoints) {
		this.list_of_entrypoints = list_of_entrypoints;
	}
}
