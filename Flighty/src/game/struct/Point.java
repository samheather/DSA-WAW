package game.struct;

import java.nio.ByteBuffer;

public class Point implements java.io.Serializable {

	public ByteBuffer serialize() {
		ByteBuffer b = ByteBuffer.allocate(16);
		b.putDouble(x);
		b.putDouble(y);
		b.rewind();
		return b;
	}

	public void deserialize(ByteBuffer b) {
		x = b.getDouble();
		y = b.getDouble();
	}

	protected double x = 0.0;
	protected double y = 0.0;
	protected String pointRef = "";

	// CONSTRUCTORS

	// Point Constructor taking two doubles for X then Y coordinates.
	public Point(double xcoord, double ycoord) {
		x = xcoord;
		y = ycoord;
	}

	public Point() {

	}

	// SETTERS AND GETTERS

	public double getX() {
		return this.x;
	}

	public double getY() {
		return this.y;
	}

	public void setX(double newX) {
		this.x = newX;
	}

	public void setY(double newY) {
		this.y = newY;
	}

	public String getPointRef() {
		return this.pointRef;
	}

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