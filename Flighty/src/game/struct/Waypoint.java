package game.struct;


/**
 * Waypoint class defines flight path nodes
 * <p>
 * Used to store target co-ordinates.
 * </p>
 */
public class Waypoint {
	
	/** Current X co-ordinate */
    protected double x;
    
    /** Current Y co-ordinate */
    protected double y;
    
    /** Current altitude */
    protected double altitude;
    
    /** Next Waypoint in list */
    protected Waypoint next;
    
    /** Previous Waypoint in list */
    protected Waypoint prev;
    
    /** Should the Waypoint be displayed */
    protected boolean visible;
    
    
    /**
     * Default constructor for Waypoint
     * <p>
     * Sets position, and sets visibility to false
     * </p>
     * 
     * @param x				the point's horizontal position
     * @param y				the point's vertical position
     */
    public Waypoint(double x, double y) {
    	this.x = x;
    	this.y = y;
    	this.altitude = 0;
    	this.next = null;
    	this.prev = null;
    	this.visible = false;
    }

	/**
     * Extended constructor for Waypoint
     * <p>
     * Sets position <b>and visibility</b>
     * </p>
     * 
     * @param x				the point's horizontal position
     * @param y				the point's vertical position
     * @param visible		<code>true</code> if the point should be shown,
     * 						<code>false</code> if the point should be hidden
     */
    public Waypoint(double x, double y, boolean visible) {
    	this.x = x;
    	this.y = y;
    	this.altitude = 0;
    	this.next = null;
    	this.prev = null;
    	this.visible = visible;
    }
    
    
    // Accessors
    /**
     * @return				the point's horizontal position
     */
    public double getX() {
    	return this.x;
    }
    
    /**
     * @return				the point's vertical position
     */
    public double getY() {
    	return this.y;
    }
    
    /**
     * @return				the point's altitude
     */
    public double getAltitude() {
    	return this.altitude;
    }
    
    /**
     * @return				the next Waypoint in the list
     */
    public Waypoint getNext() {
    	return this.next;
    }
    
    /**
     * @return				the previous Waypoint in the list
     */
    public Waypoint getPrev() {
    	return this.prev;
    }
    
    /**
     * @return				<code>true</code> if the point is visible,
     * 						<code>false</code> otherwise
     */
    public boolean isVisible() {
    	return this.visible;
    }

    
    // Mutators
    /**
     * @param x				the new horizontal position
     */
    public void setX(double x) {
    	this.x = x;
    }
    
    /**
     * @param y				the new vertical position
     */
    public void setY(double y) {
    	this.y = y;
    }
    
    /**
     * @param altitude		the altitude to set
     */
    public void setAltitude(double altitude) {
    	this.altitude = altitude;
    }
    
    /**
     * @param next			the waypoint to insert
     */
    public void setNext(Waypoint next) {
    	this.next = next;
    }
    
    /**
     * @param prev			the waypoint to insert
     */
    public void setPrev(Waypoint prev) {
    	this.prev = prev;
    }
    
    /**
     * @param visible		<code>true</code> if the point should be shown,
     * 						<code>false</code> if the point should be hidden
     */
    public void setVisible(boolean visible) {
    	this.visible = visible;
    }
    
    
    // Other methods
    /**
     * Creates a new hidden Waypoint
     * <p>
     * The new Waypoint will be hidden, and inserted before the current Waypoint
     * </p>
     * <p>
     * Note: this is preferred to {@link #setPrev(Waypoint)} as it will
     * maintain the state of the list
     * </p>
     * 
     * @param x				the point's horizontal position
     * @param y				the point's vertical position
     * @return				the new Waypoint
     */
    public Waypoint createWaypoint(double x, double y) {
    	return this.createWaypoint(x, y, false);
    }
    
    /**
     * Creates a new Waypoint
     * <p>
     * This is an alternative to {@link #createWaypoint(double, double)},
     * allowing the visibility of the new Waypoint to be set
     * <p>
     * Note: this is preferred to {@link #setPrev(Waypoint)} as it will
     * maintain the state of the list
     * </p>
     * 
     * @param x				the point's horizontal position
     * @param y				the point's vertical position
     * @param visible		<code>true</code> if the point should be shown,
     * 						<code>false</code> if the point should be hidden
     * @return				the new waypoint
     */
    public Waypoint createWaypoint(double x, double y, boolean visible) {
    	Waypoint newWaypoint = new Waypoint(x, y, visible);
    	newWaypoint.next = this;
    	newWaypoint.prev = this.prev;
    	this.prev = newWaypoint;
    	return newWaypoint;
    }
    
    /**
     * Removes a Waypoint
     * <p>
     * Causes the Waypoint to be removed from the list
     * </p>
     * <p>
     * Note: this will not actually delete references to the Waypoint
     * (i.e. the Waypoint will still exist) - it will only be <b>removed</b>
     * from the list of Waypoints.
     */
    public void deleteWaypoint() {
    	if(this.getPrev() != null) {
    		this.getPrev().setNext(this.next);
    	}
    	
    	if(this.getNext() != null) {
    		this.getNext().setPrev(this.prev);
    	}
    }
    
    /**
     * Moves the Waypoint
     * 
     * @param x				the point's horizontal position
     * @param y				the point's vertical position
     */
    public void moveWaypoint(double x, double y, double altitude) {
    	this.x = x;
    	this.y = y;
    	this.altitude = altitude;
    }


    // Overrides
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = (int) (prime * result + altitude);
		result = prime * result + ((next == null) ? 0 : next.hashCode());
		result = prime * result + ((prev == null) ? 0 : prev.hashCode());
		long temp;
		temp = Double.doubleToLongBits(x);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(y);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (!(obj instanceof Waypoint)) {
			return false;
		}
		Waypoint other = (Waypoint) obj;
		if (altitude != other.altitude) {
			return false;
		}
		if (next == null) {
			if (other.next != null) {
				return false;
			}
		} else if (!next.equals(other.next)) {
			return false;
		}
		if (prev == null) {
			if (other.prev != null) {
				return false;
			}
		} else if (!prev.equals(other.prev)) {
			return false;
		}
		if (Double.doubleToLongBits(x) != Double.doubleToLongBits(other.x)) {
			return false;
		}
		if (Double.doubleToLongBits(y) != Double.doubleToLongBits(other.y)) {
			return false;
		}
		return true;
	}
}