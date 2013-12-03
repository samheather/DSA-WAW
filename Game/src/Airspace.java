import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.Color;
import org.lwjgl.input.Mouse;
import org.newdawn.slick.Input;

@SuppressWarnings("unused")
public class Airspace {

	// FIELDS

	private int max_number_of_flights;
	private int score, loops_since_last_flight_entry, overall_loops,
			next_difficulty_loops, max_rand, difficulty_levels, wp_counter,
			exp_counter, flight_button_x, flight_button_width;
	private List<Flight> list_of_flights_in_airspace, list_of_incoming_flights;
	private List<Waypoint> list_of_waypoints;
	private List<EntryPoint> list_of_entrypoints;
	private List<ExitPoint> list_of_exitpoints;
	private SeparationRules separationRules;
	private boolean previous_removed;
	private Flight selected_flight;

	private int game_difficulty_value = 2;  // !! Should be fetched by Difficulty Screen, currently fails to do so
	private boolean warningViolation = false; // bool outlining whether a violation is present
	
	// CONSTRUCTOR

	Airspace() {
		this.max_number_of_flights = 5; // just a value
		this.score = 0;
		this.list_of_flights_in_airspace = new ArrayList<Flight>();
		this.list_of_incoming_flights = new ArrayList<Flight>();
		this.list_of_waypoints = new ArrayList<Waypoint>();
		this.list_of_entrypoints = new ArrayList<EntryPoint>();
		this.list_of_exitpoints = new ArrayList<ExitPoint>();
		this.separationRules = new SeparationRules(game_difficulty_value); 
		this.loops_since_last_flight_entry = 400; // how many loops to wait before another flight can enter
		this.overall_loops = 0; // stores how many loops there have been in total
		this.next_difficulty_loops = 5000; // this is how many loops until planes come more quickly, divide by 60 for seconds
		this.difficulty_levels = 1;// number of times difficulty changes
		this.max_rand = (int) Math.pow(2, this.difficulty_levels);
		this.previous_removed = false; // variable for storing whether a flight was removed on each loop
		this.wp_counter = 64;
		this.exp_counter = 0;
		this.flight_button_x = 0;
		this.selected_flight = null;
		this.flight_button_width=100;
	}

	// METHODS

	public boolean new_waypoint(int x, int y) {
		this.wp_counter++;
		Waypoint tmpWp = new Waypoint(x, y);
		tmpWp.setPointRef(String.valueOf((char) this.wp_counter));
		if (this.addWaypoint(tmpWp)) {
			return true;
		} else {
			return false;
		}
	}

	public boolean check_if_flight_has_left_airspace(Flight f) {

		if (f.getX() > 1250 || f.getX() < -50 || f.getY() > 500
				|| f.getY() < -50) {
			return true;
		} else {
			return false;
		}

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

	public boolean new_flight2(int num, GameContainer gc) throws SlickException {

		if (this.list_of_flights_in_airspace.size() < this.max_number_of_flights) {
			Random rand = new Random();
			boolean firstFlightCreated = false;
			double x;
			double y;
			int check_number;
			int entryPoint = rand.nextInt(3);

			if (this.list_of_entrypoints.size() == 3) { // if we have all three entrypoints
				x = this.list_of_entrypoints.get(entryPoint).getX();// choose one a get the x and y values
				y = this.list_of_entrypoints.get(entryPoint).getY();

			} else { // if all entrypoints are not there then just assign some values
				x = 1250;
				y = 300;

			}
			if (this.overall_loops >= this.next_difficulty_loops) {
				this.next_difficulty_loops += 5000;
				if (this.max_rand > 2) {
					this.max_rand = this.max_rand / 2;
				}

			}
			if (this.list_of_flights_in_airspace.isEmpty()) {
				check_number = rand.nextInt(100);
			} else {
				check_number = rand.nextInt(this.max_rand);
			}

			if (check_number == 1) {

				if (this.loops_since_last_flight_entry >= 700  || this.list_of_flights_in_airspace.isEmpty()) {

					Flight tempFlight = new Flight(this);
					tempFlight.setFlight_name(this.generate_flight_name());
					tempFlight.setX(x);
					tempFlight.setY(y);
					tempFlight.setTarget_altitude(tempFlight.getCurrent_altitude());
					double heading = Calculations.calculate_heading_to_first_waypoint(tempFlight,
									tempFlight.getFlight_plan().getPointByIndex(0).getX() - 5,
									tempFlight.getFlight_plan().getPointByIndex(0).getY() - 5);
					tempFlight.setTarget_heading(heading);
					tempFlight.setCurrent_heading(heading);
					this.loops_since_last_flight_entry = 0;
					if (this.list_of_flights_in_airspace.add(tempFlight)) {
						if (!firstFlightCreated) {
							firstFlightCreated = true;
						}
						this.list_of_flights_in_airspace.get(
								this.list_of_flights_in_airspace.size() - 1)
								.init(gc);
						this.flight_button_x += this.flight_button_width;
						return true;
					}
				}

			}

		}

		return false;

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
				System.out.println(nearest_flight);
				for (int i =0; i< airspace.getList_of_flights().size(); i++){
					if(i != index_of_nearest_flight){
						airspace.getList_of_flights().get(i).setSelected(false);
					}
				}
				
				
			}
			
			
			
		}
	}

	public boolean new_exit_point(int x, int y) {
		ExitPoint tmpEp = new ExitPoint(x, y);
		this.exp_counter++;
		tmpEp.setPointRef("EXP" + this.exp_counter);
		if (this.addExitPoint(tmpEp)) {
			return true;
		} else {
			return false;
		}
	}

	public boolean new_entry_point(int x, int y) {
		EntryPoint tmpEp = new EntryPoint(x, y);
		if (this.addEntryPoint(tmpEp)) {
			return true;
		} else {
			return false;
		}
	}

	//public void setDifficulty(int difficulty) {
	//	switch (difficulty) {
	//	case 1:
	//		separationRules.setGameOverLateralSeparation(50);
	//		separationRules.setGameOverVerticalSeparation(50);
	//		break;
	//	case 2:
	//		separationRules.setGameOverLateralSeparation(25);
	//		separationRules.setGameOverVerticalSeparation(25);
	//		break;
	//	case 3:
	//		separationRules.setGameOverLateralSeparation(10);
	//		separationRules.setGameOverVerticalSeparation(10);
	//		break;
	//	}
	//}

	public void changeScore(int value) {
		this.score += value;
	}

	// INIT, RENDER, UPDATE

	public void init(GameContainer gc) throws SlickException {
		
		//for (int i = 0; i < this.list_of_waypoints.size(); i++) {
		//	this.list_of_waypoints.get(i).init(gc);
		//}

		for (int i = 0; i < this.list_of_flights_in_airspace.size(); i++) {
			this.list_of_flights_in_airspace.get(i).init(gc);
		}

	}

	public void render(Graphics g, GameContainer gc) throws SlickException { // I added this so we can draw things in the airspace, for example a
																				// radar like background or terrain

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
// Ram - Draw lines between flights in airspace violation.
// Logic checks each pair of planes currently in airspace. If too close (as defined by Separation Rules Object, 
// red line is drawn.
		for (int flightno = 0; flightno < this.getList_of_flights().size(); flightno++) {
			
			for (int temptemp = flightno + 1; temptemp < this.getList_of_flights().size(); temptemp++ ) {	
				
				if (this.get_separation_rules().lateralDistanceBetweenFlights(this.getList_of_flights().get(flightno), 
						this.getList_of_flights().get(temptemp)) <= this.get_separation_rules().getWarningLateralSeparation()) {
					
					if (this.get_separation_rules().verticalDistanceBetweenFlights(this.getList_of_flights().get(flightno), 
							this.getList_of_flights().get(temptemp)) <= this.get_separation_rules().getWarningVerticalSeparation()) {
						
						float f1x = (float) list_of_flights_in_airspace.get(flightno).getX();
						float f1y = (float) list_of_flights_in_airspace.get(flightno).getY();
						float f2x = (float) list_of_flights_in_airspace.get(temptemp).getX();
						float f2y = (float) list_of_flights_in_airspace.get(temptemp).getY();
						g.setColor(Color.red);
						g.setLineWidth(2);
						g.drawLine(f1x, f1y, f2x, f2y);
						g.setLineWidth(1);
						
				}}
			}
		}

	}
	public void remove_specific_flight(int flight) {
		this.list_of_flights_in_airspace.get(flight).getControls()
		.clear_all();
		this.list_of_flights_in_airspace.remove(flight); // remove that flight from the list
		this.previous_removed = true; // tell the program a flight has been removed on this loop
		if (!(this.list_of_flights_in_airspace.contains(this.selected_flight))) {
			this.selected_flight = null;

		}
	}

	public void update(GameContainer gc) {
		this.loops_since_last_flight_entry++;
		this.overall_loops++;
		int posX=Mouse.getX();
		int posY=Mouse.getY();
		if(Mouse.isButtonDown(0)){
			this.check_selected(posX,posY,this);
		}
		for (int i = 0; i < this.list_of_flights_in_airspace.size(); i++) {
			this.list_of_flights_in_airspace.get(i).update(gc, this);
			if(this.list_of_flights_in_airspace.get(i).getFlight_plan().getWaypoints().size()==0) {
				this.remove_specific_flight(i);
			}
			else if (this.check_if_flight_has_left_airspace(this.getList_of_flights().get(i))) { // if a flight has left the airspace
				this.remove_specific_flight(i);
			}
			
			

			// the code to shift all the buttons up if there is space
			if (this.list_of_flights_in_airspace.size() > 0) { // if the list is not empty
				if (i <= this.list_of_flights_in_airspace.size() - 1) { // if i is not greater than the size of the list
					if (this.previous_removed) { // if an object was removed
						this.list_of_flights_in_airspace.get(i).setFlight_button_x(
						this.list_of_flights_in_airspace.get(i).getFlight_button_x() - this.flight_button_width);// take 100 off the current x value of the button

						if (i == this.list_of_flights_in_airspace.size() - 1) { // if we are at the end of the list
							this.flight_button_x -= this.flight_button_width; // take 130 of this.flight_button_x so that when the next flight is made is button is next to
															// the current last button
							this.previous_removed = false; // set to false so none of this runs until next time a flight is removed
						}

					}

				} else { // if i was greater than the size of the list, we must have removed the last element so
					this.flight_button_x -= this.flight_button_width; // just decrease this, but the x of any current buttons doesn't need to change
					this.previous_removed = false;
				}
			} else { // if the list was empty set flight_button_x back to its initial value
				this.flight_button_x -= this.flight_button_width;
				this.previous_removed = false;
			}
		}
		
		this.separationRules.checkViolation(this);
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

	public void removeWaypoint(Waypoint waypoint) {
		this.list_of_waypoints.remove(waypoint);
	}

	public void removeEntryPoint(EntryPoint entrypoint) {
		this.list_of_entrypoints.remove(entrypoint);
	}

	public void removeExitPoint(ExitPoint exitpoint) {
		this.list_of_exitpoints.remove(exitpoint);
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

	public boolean check_flight(Flight flight) {
		return this.list_of_flights_in_airspace.contains(flight);
	}

	public boolean remove_flight(Flight flight) {
		if (this.list_of_flights_in_airspace.contains(flight)) {
			this.list_of_flights_in_airspace.remove(flight);
			return true;
		} else {
			return false;
		}
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
}
