import java.util.ArrayList;
import java.util.List;

public class Airspace {

	//FIELDS
	
	int max_number_of_flights;
	int score;
	List<Flight> list_of_flights, list_of_incoming_flights;
	List<Waypoint> list_of_waypoints;	
	List<EntryPoint> list_of_entrypoints;
	List<ExitPoint> list_of_exitpoints;
	
	//CONSTRUCTOR
	
	Airspace(){
		this.max_number_of_flights = 20; //just a value
		this.score = 0;
		this.list_of_flights = new ArrayList<Flight>();
		this.list_of_incoming_flights = new ArrayList<Flight>();
		this.list_of_waypoints = new ArrayList<Waypoint>();
		this.list_of_entrypoints = new ArrayList<EntryPoint>();
		this.list_of_exitpoints = new ArrayList<ExitPoint>();
	}
	
	//METHODS
	
	public boolean add_flight(Flight flight){ 	
	
		//Checks whether the flight was already added before, and if it won't pass the maximum number of flights allowed	
		if ((this.list_of_flights.contains(flight)) && (this.list_of_flights.size() > this.max_number_of_flights-1)){    
			return false;
		} else {
			this.list_of_flights.add(flight);
			return true;
		}/* I made them boolean so we can check if the plane was added successfully 
	    (we can change them later on) */
	}
	public boolean check_flight(Flight flight){
		return this.list_of_flights.contains(flight);
	}
	
	public boolean remove_flight(Flight flight){
		if (this.list_of_flights.contains(flight)){
			this.list_of_flights.remove(flight);
			return true;
		} else {
			return false;
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

	//GETTERS & SETTERS
	
	public int getMax_number_of_flights() {
		return this.max_number_of_flights;
	}

	public int getScore() {
		return this.score;
	}

	public List<Flight> getList_of_flights() {
		return this.list_of_flights;
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
}

