package game.struct;

/**
 * Waypoint class defines flight path nodes
 * <p>
 * Used to store target co-ordinates.
 * </p>
 */
public class Waypoint extends Point {
	
	public Waypoint() {
		
	}
	protected double x;
	protected double y;
	protected boolean visible;

	/**
	 * Default constructor for Waypoint
	 * <p>
	 * Sets position.
	 * </p>
	 * 
	 * @param x
	 *            the point's horizontal position
	 * @param y
	 *            the point's vertical position
	 */
	public Waypoint(double x, double y) {
		super(x, y);
	}

}
