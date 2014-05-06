package game.struct;

/**
 * Class references any generic point.
 * 
 * @author Samuel
 * 
 */
public class Point {

	/** X position of point */
	protected double x = 0.0;

	/** Y position of point */
	protected double y = 0.0;

	/** String reference to the point */
	protected String pointRef = "";

	/**
	 * Point Constructor taking two doubles for X then Y coordinates.
	 * 
	 * @param xcoord
	 * @param ycoord
	 */
	public Point(double xcoord, double ycoord) {
		x = xcoord;
		y = ycoord;
	}

	/** Empty constructor */
	public Point() {

	}

	// Accessors and Mutators

	/**
	 * Returns X position of point
	 * 
	 * @return
	 */
	public double getX() {
		return this.x;
	}

	/**
	 * Returns Y position of point
	 * 
	 * @return
	 */
	public double getY() {
		return this.y;
	}

	/**
	 * Sets X position of point.
	 * 
	 * @param newX
	 */
	public void setX(double newX) {
		this.x = newX;
	}

	/**
	 * Sets Y position of point.
	 * 
	 * @param newY
	 */
	public void setY(double newY) {
		this.y = newY;
	}

	/**
	 * Get position reference
	 * 
	 * @return
	 */
	public String getPointRef() {
		return this.pointRef;
	}

	/**
	 * Sets position ref to input String
	 * 
	 * @param pointRef
	 */
	public void setPointRef(String pointRef) {
		this.pointRef = pointRef;
	}

	/**
	 * Checks if points are equal
	 * 
	 * @param point
	 *            the point that you are comparing it with
	 * @return whether they have the same position
	 */
	public boolean equals(Point point) {
		if (point instanceof Point) {
			if ((point.getX() == this.x) && (point.getY() == this.y)) {
				return true;
			}
		}

		return false;
	}
}