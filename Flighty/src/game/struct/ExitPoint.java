package game.struct;

/**
 * Waypoint class defines flight path nodes
 * <p>
 * Used to store target co-ordinates.
 * </p>
 */
public class ExitPoint extends Point {

	/**
	 * Empty constructor.
	 */
	public ExitPoint() {

	}

	/** X position of Exit point */
	protected double x;

	/** Y position of Exit point */
	protected double y;

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
	public ExitPoint(double x, double y) {
		super(x, y);
	}
}