package game.struct;

/**
 * Waypoint class defines flight path nodes
 * <p>
 * Used to store target co-ordinates.
 * </p>
 */
public class EntryPoint extends Point {

	/**
	 * Empty constructor
	 */
	public EntryPoint() {

	}

	/** X position of Entry point */
	protected double x;

	/** Y position of Entry point */
	protected double y;

	/** Is this entry point visible? */
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
	public EntryPoint(double x, double y) {
		super(x, y);
	}
}
