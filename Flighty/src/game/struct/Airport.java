package game.struct;

import org.newdawn.slick.geom.*;


public class Airport extends Point {
	

	private int landingAltitude;
	private boolean planeLandingOrTakingOff;
	private Plane planeWaitingtoTakeoff;
	private Waypoint beginningOfRunway, endOfRunway;
	private Polygon approachPolygon;
	
	
	
    public Airport(double x, double y) {
    	super(x,y);
    	this.planeLandingOrTakingOff = false;
    	this.planeWaitingtoTakeoff = null;
    	this.landingAltitude = 20000;
    	this.beginningOfRunway = new Waypoint(720,460);
    	this.endOfRunway = new Waypoint(1180,460);
    	approachPolygon = new Polygon();
    	approachPolygon.addPoint(720, 460);
    	approachPolygon.addPoint(400, 344);
    	approachPolygon.addPoint(400, 576);
    	
    	
    	
    	
    }

    // ACCESSORS AND MODIFIERS

	public Waypoint getBeginningOfRunway() {
		return beginningOfRunway;
	}

	public void setBeginningOfRunway(Waypoint beginningOfRunway) {
		this.beginningOfRunway = beginningOfRunway;
	}

	public Waypoint getEndOfRunway() {
		return endOfRunway;
	}

	public void setEndOfRunway(Waypoint endOfRunway) {
		this.endOfRunway = endOfRunway;
	}

	public int getLandingAltitude() {
		return landingAltitude;
	}



	public void setLandingAltitude(int landingAltitude) {
		this.landingAltitude = landingAltitude;
	}



	public boolean isPlaneLandingOrTakingOff() {
		return planeLandingOrTakingOff;
	}



	public void setPlaneLandingOrTakingOff(boolean planeLandingOrTakingOff) {
		this.planeLandingOrTakingOff = planeLandingOrTakingOff;
	}



	public Plane getPlaneWaitingtoTakeoff() {
		return planeWaitingtoTakeoff;
	}



	public void setPlaneWaitingtoTakeoff(Plane planeWaitingtoTakeoff) {
		this.planeWaitingtoTakeoff = planeWaitingtoTakeoff;
	}

	public Polygon getApproachPolygon() {
		return approachPolygon;
	}

	public void setApproachPolygon(Polygon approachPolygon) {
		this.approachPolygon = approachPolygon;
	}

    
    

   

    
    
    
}
	


