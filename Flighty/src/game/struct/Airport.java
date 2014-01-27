package game.struct;

public class Airport extends Point {
	

	private int landingAltitude;
	private boolean planeLandingOrTakingOff;
	private Plane planeWaitingtoTakeoff;
	
	
	
    public Airport(double x, double y) {
    	super(x,y);
    	this.planeLandingOrTakingOff = false;
    	this.planeWaitingtoTakeoff = null;
    	this.landingAltitude = 20000;
    	
    	
    	
    }

    // ACCESSORS AND MODIFIERS

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

    
    

   

    
    
    
}
	


