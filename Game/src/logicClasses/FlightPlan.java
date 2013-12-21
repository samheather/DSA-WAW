package logicClasses;

import java.util.ArrayList;
import java.util.Random;

import org.lwjgl.input.Mouse;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.SlickException;

public class FlightPlan {
	
	// FIELDS
	
	
	private ArrayList<Point> currentRoute = new ArrayList<Point>();
	private ArrayList<Point> waypointsAlreadyVisited;
	private double velocity;
	private Flight flight;
	private Point waypointMouseIsOver;
	private Point waypointClicked;
	private boolean changingPlan;
	private boolean draggingWaypoint;
	

	// CONSTRUCTOR
	

	public FlightPlan(Airspace airspace, Flight flight) {
		this.flight = flight;
		this.velocity = generate_velocity();
		this.currentRoute = build_route(airspace, flight.getEntryPoint());
		this.waypointsAlreadyVisited = new ArrayList<Point>();
		this.changingPlan = false;
		this.draggingWaypoint = false;
	}
	

	// METHODS
	
	
	public ArrayList<Point> build_route(Airspace airspace, EntryPoint entryPoint) {
		ArrayList<Point> temp_route = new ArrayList<Point>();
		ArrayList<Point> temp_list_of_waypoints = new ArrayList<Point>();
		ArrayList<Point> temp_list_of_exitpoints = new ArrayList<Point>();
		Boolean exitpoint_added = false;
		
		if (!airspace.getList_of_way_points().isEmpty()&& !airspace.getList_of_exit_points().isEmpty()) {
				Random rand = new Random();
				
				// Initialising Temporary Lists
				
				for (int i = 0; i < airspace.getList_of_way_points().size(); i++) {
					temp_list_of_waypoints.add(airspace.getList_of_way_points().get(i));
				}
				
				for (int i = 0; i < airspace.getList_of_exit_points().size(); i++) {
					temp_list_of_exitpoints.add(airspace.getList_of_exit_points().get(i));
				}
				
				// Adding Waypoints to Plan
				
				int pointsInPlan = rand.nextInt(3) + 4;
				
				for (int i = 0; i < pointsInPlan - 1; i++) {
					int waypoint_index = rand.nextInt(temp_list_of_waypoints.size());
					temp_route.add(temp_list_of_waypoints.get(waypoint_index));
					temp_list_of_waypoints.remove(waypoint_index);
				}
				
				// Adding ExitPoint to Plan
				
				int ExitPointIndex = rand.nextInt(temp_list_of_exitpoints.size());
				
				while (exitpoint_added == false){
					
					if (entryPoint.getY() == temp_list_of_exitpoints.get(ExitPointIndex).getY()){
						temp_list_of_exitpoints.remove(ExitPointIndex);
						ExitPointIndex = rand.nextInt(temp_list_of_exitpoints.size());
					}
					
					else if (entryPoint.getX() == temp_list_of_exitpoints.get(ExitPointIndex).getX()){
						temp_list_of_exitpoints.remove(ExitPointIndex);
						ExitPointIndex = rand.nextInt(temp_list_of_exitpoints.size());
					}
					else{
						temp_route.add(temp_list_of_exitpoints.get(ExitPointIndex));
						exitpoint_added = true;
					}
				}
		}
		
		return temp_route;
	}


	public int generate_velocity() {
		Random rand = new Random();
		return (rand.nextInt(200) + 200);
	}
	
	private boolean isMouseOnWaypoint() {
		int mouseX = Mouse.getX();
		int mouseY = Mouse.getY();
		mouseY=600-mouseY;
		if(this.getWaypoints().isEmpty()) {
			return false;
		}
		for(int i=0; i<this.flight.getAirspace().getList_of_way_points().size();i++) {
			if (((Math.abs(Math.round(mouseX) - Math.round(this.flight.getAirspace().getList_of_way_points().get(i).getX()))) <= 15)
					&& (Math.abs(Math.round(mouseY) - Math.round(this.flight.getAirspace().getList_of_way_points().get(i).getY()))) <= 15) {
				
					this.waypointMouseIsOver=this.flight.getAirspace().getList_of_way_points().get(i);
					return true;
					
			}
		}
		this.waypointMouseIsOver=null;
		return false;
	}
	
	public void update_flight_plan(){

		if (this.currentRoute.size() > 0) {
			if (this.flight.check_if_flight_at_waypoint(currentRoute
					.get(0))) {
				this.waypointsAlreadyVisited.add(this.currentRoute.get(0));
				this.currentRoute.remove(0);
			}
		}

	}
	
	public void change_flight_plan(){
		if (this.flight.getSelected() && this.currentRoute.size() > 0 ){
			boolean mouseOverWaypoint = this.isMouseOnWaypoint();

				// Checks if user is not currently dragging a waypoint
				if (!draggingWaypoint){
					//Checks if user has clicked on a waypoint
					if(mouseOverWaypoint && Mouse.isButtonDown(0)) {
						this.waypointClicked=this.waypointMouseIsOver;
						this.draggingWaypoint=true;
					}
				}
				
				// Checks if user is currently dragging a waypoint
				else if(draggingWaypoint){
					// Checks if user has released mouse from drag over empty airspace
					if((!Mouse.isButtonDown(0)) && !mouseOverWaypoint){
						this.waypointClicked=null;
						this.draggingWaypoint=false;
							
					}
					
					// Checks if user has released mouse from drag over another waypoint
					else if((!Mouse.isButtonDown(0)) && mouseOverWaypoint){
						
						//Finding waypoint that mouse is over
						for(int i=0; i<this.currentRoute.size();i++) {
							
							// Checks if new waypoint is not already in the plan and adds if not in plan
							if (this.waypointClicked == this.currentRoute.get(i)&& (!this.currentRoute.contains(this.waypointMouseIsOver))&& (!this.waypointsAlreadyVisited.contains(this.waypointMouseIsOver))){
								this.currentRoute.remove(i);
								this.currentRoute.add(i,this.waypointMouseIsOver);
								this.waypointClicked=null;
								this.draggingWaypoint=false;
								
							}
							
							// Checks if waypoint already in plan and doesn't add if not
							else if(this.waypointClicked == this.currentRoute.get(i)&& ((this.currentRoute.contains(this.waypointMouseIsOver)) || (this.waypointsAlreadyVisited.contains(this.waypointMouseIsOver)))){
								this.waypointClicked=null;
								this.draggingWaypoint=false;
								break;
								
							}
						}
					}
				}
		}
	}
	
	public void draw_flights_plan(Graphics g, GameContainer gc){

		if (this.currentRoute.size() > 0){
			
			g.setColor(Color.cyan);
			
			// If not dragging waypoints, just draw lines between all waypoints in plan
			if(!draggingWaypoint){
				for(int i=1; i<this.currentRoute.size();i++) {
					g.drawLine((float)this.currentRoute.get(i).getX(), (float)this.currentRoute.get(i).getY(), (float)this.currentRoute.get(i-1).getX(), (float)this.currentRoute.get(i-1).getY());
				}
			}
			
			else if(draggingWaypoint){
				for(int i=1; i<this.currentRoute.size();i++) {
					
					// This is needed as i=1 behavours differently to other values of i when first waypoint is being dragged.
					if(i==1){
						if(this.waypointClicked == this.currentRoute.get(0) ) {
							g.drawLine(Mouse.getX(),600-Mouse.getY() , (float)this.currentRoute.get(1).getX(),(float)this.currentRoute.get(1).getY());
						}
						
						else if (this.waypointClicked == this.currentRoute.get(1)){
							g.drawLine((float)this.currentRoute.get(i+1).getX(), (float)this.currentRoute.get(i+1).getY(),Mouse.getX(),600-Mouse.getY());
							g.drawLine((float)this.currentRoute.get(i-1).getX(), (float)this.currentRoute.get(i-1).getY(),Mouse.getX(),600-Mouse.getY());
							i++;
							
						}
						
						else{
							g.drawLine((float)this.currentRoute.get(i).getX(), (float)this.currentRoute.get(i).getY(), (float)this.currentRoute.get(i-1).getX(), (float)this.currentRoute.get(i-1).getY());
						}
						

					}
					
					else{
						// If Waypoint is being changed draw lines between mouse and waypoint before and after the waypoint being changed. 
						if (this.waypointClicked == this.currentRoute.get(i)){
							g.drawLine((float)this.currentRoute.get(i+1).getX(), (float)this.currentRoute.get(i+1).getY(),Mouse.getX(),600-Mouse.getY());
							g.drawLine((float)this.currentRoute.get(i-1).getX(), (float)this.currentRoute.get(i-1).getY(),Mouse.getX(),600-Mouse.getY());
							i++;
						}
						
						else{
							g.drawLine((float)this.currentRoute.get(i).getX(), (float)this.currentRoute.get(i).getY(), (float)this.currentRoute.get(i-1).getX(), (float)this.currentRoute.get(i-1).getY());
						}
						
					}
				}
			}
				
		}
		
		
	}
	
	public void markUnavailableWaypoints(Graphics g, GameContainer gc){
		for (int i = 0; i < this.waypointsAlreadyVisited.size(); i++){
			g.drawLine((float) this.waypointsAlreadyVisited.get(i).getX()-14, (float) this.waypointsAlreadyVisited.get(i).getY()-14, (float) this.waypointsAlreadyVisited.get(i).getX()+14, (float) this.waypointsAlreadyVisited.get(i).getY()+14);
			g.drawLine((float) this.waypointsAlreadyVisited.get(i).getX()+14, (float) this.waypointsAlreadyVisited.get(i).getY()-14, (float) this.waypointsAlreadyVisited.get(i).getX()-14, (float) this.waypointsAlreadyVisited.get(i).getY()+14);
		}
	}
	
	public void update() {
		
		this.update_flight_plan();
		if(this.changingPlan == true){
			this.change_flight_plan();
		}

	}
	
	public void render(Graphics g, GameContainer gc) throws SlickException {


		if(this.flight.getSelected()) {
			if(this.changingPlan == true){
				this.draw_flights_plan(g, gc);
				this.markUnavailableWaypoints(g, gc);
			}
			

		}
		
	}
	
	
	
	
	

	// ACCESSORS AND MUTATORS

	public void setVelocity(double new_velocity){
		this.velocity = new_velocity;
		
	}
	
	public double getVelocity() {
		return this.velocity;
	}

	public ArrayList<Point> getWaypoints() {
		return currentRoute;
	}

	public Point getPointByIndex(int i) {
		return this.currentRoute.get(i);

	}
	
	public boolean getChangingPlan(){
		return this.changingPlan;
	}
	
	public void setChangingPlan(boolean bool){
		this.changingPlan = bool;
	}
	
	public boolean getDraggingWaypoint(){
		return this.draggingWaypoint;
	}

	@Override
	public String toString() {
		String returnString = "";
		for (int i = 0; i < this.currentRoute.size(); i++) {
			returnString += "Point " + i + ": ";
			returnString += this.currentRoute.get(i).getX();
			returnString += ", ";
			returnString += this.currentRoute.get(i).getY();
			returnString += " | ";
		}
		return returnString;
	}

}