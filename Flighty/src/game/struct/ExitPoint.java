package game.struct;


/**
 * ExitPoint class defines flight path end nodes
 * <p>
 * Used to store target co-ordinates for the point at which a flight
 * terminates.
 * </p>
 */
public class ExitPoint extends Waypoint {

	// Constructors
    /**
     * Default constructor for ExitPoint
     * <p>
     * Sets position, and sets visibility to false
     * </p>
     * 
     * @param x				the point's horizontal position
     * @param y				the point's vertical position
     */
	public ExitPoint(double x, double y) {
		super(x, y);
	}
	
    /**
     * Extended constructor for ExitPoint
     * <p>
     * Sets position <b>and visibility</b>
     * </p>
     * 
     * @param x				the point's horizontal position
     * @param y				the point's vertical position
     * @param visible		<code>true</code> if the point should be shown,
     * 						<code>false</code> if the point should be hidden
     */
	public ExitPoint(double x, double y, boolean visible) {
		super(x, y, visible);
	}
	
	// Overrides
	/**
	 * <p>
	 * Will <b>not</b> trigger any errors if used to change value of next.
	 * Will cause next to remain null.
	 * </p>
     */
	@Override
	public void setNext(Waypoint newNext) {
    	this.next = null;
    }
}