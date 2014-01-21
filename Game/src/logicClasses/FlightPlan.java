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
	private EntryPoint entryPoint;
	

	// CONSTRUCTOR
	

	public FlightPlan(Airspace airspace, Flight flight) {
		this.flight = flight;
		this.velocity = generateVelocity();
		this.entryPoint = generateEntryPoint(airspace);
		this.currentRoute = buildRoute(airspace, this.entryPoint);
		this.waypointsAlreadyVisited = new ArrayList<Point>();
		this.changingPlan = false;
		this.draggingWaypoint = false;
		
	}
	

	// METHODS
	
	/**
	 * generateEntryPoint: Creates the entry point for the flight
	 * @param airspace
	 * @return airspace.getListofEntryPoints
	 */
	
	public EntryPoint generateEntryPoint(Airspace airspace){
		
		Random rand = new Random();
		int randomNumber = rand.nextInt(3);
			
		// Setting flights x and y to the coordinates of it's entrypoint
		flight.setX(airspace.getListOfEntryPoints().get(randomNumber).getX()); // choose one a get the x and y values
		flight.setY(airspace.getListOfEntryPoints().get(randomNumber).getY());
		
		return airspace.getListOfEntryPoints().get(randomNumber);
		
	}
	
	/**
	 * buildRoute: Creates an array of waypoints that the aircraft will be initially given  
	 * @param airspace
	 * @param entryPoint
	 * @return tempRoute
	 */
	
	public ArrayList<Point> buildRoute(Airspace airspace, EntryPoint entryPoint) {
		ArrayList<Point> tempRoute = new ArrayList<Point>();
		ArrayList<Point> tempListOfWaypoints = new ArrayList<Point>();
		ArrayList<Point> tempListOfExitPoints = new ArrayList<Point>();
		Boolean exitpointAdded = false;
		
		if (!airspace.getListOfWaypoints().isEmpty()&& !airspace.getListOfExitPoints().isEmpty()) {
				Random rand = new Random();
				
				// Initialising Temporary Lists
				
				for (int i = 0; i < airspace.getListOfWaypoints().size(); i++) {
					tempListOfWaypoints.add(airspace.getListOfWaypoints().get(i));
				}
				
				for (int i = 0; i < airspace.getListOfExitPoints().size(); i++) {
					tempListOfExitPoints.add(airspace.getListOfExitPoints().get(i));
				}
				
				// Adding Waypoints to Plan
				
				int pointsInPlan = rand.nextInt(3) + 3;
				
				for (int i = 0; i < pointsInPlan - 1; i++) {
					int waypointIndex = rand.nextInt(tempListOfWaypoints.size());
					tempRoute.add(tempListOfWaypoints.get(waypointIndex));
					tempListOfWaypoints.remove(waypointIndex);
				}
				
				// Adding ExitPoint to Plan
				
				int ExitPointIndex = rand.nextInt(tempListOfExitPoints.size());
				
				while (exitpointAdded == false){
					
					if (this.entryPoint.getY() == tempListOfExitPoints.get(ExitPointIndex).getY()){
						tempListOfExitPoints.remove(ExitPointIndex);
						ExitPointIndex = rand.nextInt(tempListOfExitPoints.size());
					}
					
					else if (this.entryPoint.getX() == tempListOfExitPoints.get(ExitPointIndex).getX()){
						tempListOfExitPoints.remove(ExitPointIndex);
						ExitPointIndex = rand.nextInt(tempListOfExitPoints.size());
					}
					else{
						tempRoute.add(tempListOfExitPoints.get(ExitPointIndex));
						exitpointAdded = true;
					}
				}
		}
		
		return tempRoute;
	}
	
	/**
	 * generateVelocity: Creates a velocity from a range of values
	 */

	public int generateVelocity() {
		Random rand = new Random();
		return (rand.nextInt(200) + 200);
	}
	
	/**
	 * isMouseOnWaypoint: Used to tell what waypoint the mouse is currently over
	 */
	
	private boolean isMouseOnWaypoint() {
		int mouseX = Mouse.getX();
		int mouseY = Mouse.getY();
		mouseY=600-mouseY;
		if(this.getCurrentRoute().isEmpty()) {
			return false;
		}
		for(int i=0; i<this.flight.getAirspace().getListOfWaypoints().size();i++) {
			if (((Math.abs(Math.round(mouseX) - Math.round(this.flight.getAirspace().getListOfWaypoints().get(i).getX()))) <= 15)
					&& (Math.abs(Math.round(mouseY) - Math.round(this.flight.getAirspace().getListOfWaypoints().get(i).getY()))) <= 15) {
				
					this.waypointMouseIsOver=this.flight.getAirspace().getListOfWaypoints().get(i);
					return true;
					
			}
		}
		this.waypointMouseIsOver=null;
		return false;
	}
	
	/**
	 * updateFlightPlan: Handles updating the flight plan when a flight passes through a waypoint
	 */
	
	public void updateFlightPlan(){

		if (this.currentRoute.size() > 0) {
			if (this.flight.checkIfFlightAtWaypoint(currentRoute
					.get(0))) {
				this.waypointsAlreadyVisited.add(this.currentRoute.get(0));
				this.currentRoute.remove(0);
			}
		}

	}
	
	/**
	 * changeFlightPlan: Handles the user changing the flightplan using the mouse in 
	 * plan mode
	 */
	
	public void changeFlightPlan(){
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
	
	/**
	 * drawFlightsPlan: Draws the graphics required for the flightplan
	 * @param g
	 * @param gs
	 */
	
	public void drawFlightsPlan(Graphics g, GameContainer gc){

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
	
	/**
	 * markUnavaliableWaypoints: Handles alerting the user to any waypoints that are
	 * invalid for selection
	 * @param g
	 * @param gc
	 */
	
	public void markUnavailableWaypoints(Graphics g, GameContainer gc){
		for (int i = 0; i < this.waypointsAlreadyVisited.size(); i++){
			g.drawLine((float) this.waypointsAlreadyVisited.get(i).getX()-14, (float) this.waypointsAlreadyVisited.get(i).getY()-14, (float) this.waypointsAlreadyVisited.get(i).getX()+14, (float) this.waypointsAlreadyVisited.get(i).getY()+14);
			g.drawLine((float) this.waypointsAlreadyVisited.get(i).getX()+14, (float) this.waypointsAlreadyVisited.get(i).getY()-14, (float) this.waypointsAlreadyVisited.get(i).getX()-14, (float) this.waypointsAlreadyVisited.get(i).getY()+14);
		}
	}
	
	public void update() {
		
		this.updateFlightPlan();
		if(this.changingPlan == true){
			this.changeFlightPlan();
		}

	}
	
	public void render(Graphics g, GameContainer gc) throws SlickException {


		if(this.flight.getSelected()) {
			if(this.changingPlan == true){
				this.drawFlightsPlan(g, gc);
				this.markUnavailableWaypoints(g, gc);
			}
			

		}
		
	}
	
	
	
	
	

	// ACCESSORS AND MUTATORS

	public void setVelocity(double newVelocity){
		this.velocity = newVelocity;
		
	}
	
	public double getVelocity() {
		return this.velocity;
	}

	public ArrayList<Point> getCurrentRoute() {
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
	
	public EntryPoint getEntryPoint(){
		return this.entryPoint;
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