package game.struct;

/**
 * Waypoint class defines flight path nodes
 * <p>
 * Used to store target co-ordinates.
 * </p>
 */
public class Waypoint extends Point {

	/**
	 * This constructor is required for serialization, it should not be called
	 */
	public Waypoint() {

	}

	protected double x; // x position
	protected double y; // y position
	protected boolean visible; // is the waypoint visible by the player

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
