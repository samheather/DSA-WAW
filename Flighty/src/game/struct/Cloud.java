package game.struct;

public class Cloud {

	/** Speed the cloud is traveling at */
	private int velocity;

	/** Current X co-ordinate */
	private float x;

	/** Current Y co-ordinate */
	private float y;

	/** End of a cloud object. */
	private Point end = new Point();

	/** Required by Slick2D */
	public transient Game currentGame;

	/**
	 * Empty constructor for cloud.
	 */
	public Cloud() {
	}

	public Cloud(int velocity, float x, float y, Game currentGame) {
		this.currentGame = currentGame;
		this.velocity = velocity;
		this.x = x;
		this.y = y;
		this.end.setX(x);
		if (y == currentGame.windowHeight) {
			this.end.setY(0);
		} else {
			this.end.setY(currentGame.windowHeight);
		}
	}

	/** Updates x and y coordinates */
	public void updateXYCoordinates() {
		if (getY() > this.end.getY()) {
			setY(getY() - velocity);
		} else if (getY() < this.end.getY()) {
			setY(getY() + velocity);
		}
		if (getX() > this.end.getX()) {
			setX(getX() - velocity);
		} else if (getX() < this.end.getY()) {
			setX(getX() + velocity);
		}

	}

	/**
	 * Function to move a cloud to the next position
	 * 
	 * @return
	 */
	public boolean moveCloud() {
		if (getY() != this.end.getY()) {
			updateXYCoordinates();
			return false;
		}
		return true;
	}

	// All general Accessors

	/**
	 * Get y position of this cloud.
	 * 
	 * @return y position of this cloud
	 */
	public float getY() {
		return this.y;
	}

	/**
	 * Get x position of this cloud
	 * 
	 * @return x position of this cloud
	 */
	public float getX() {
		return this.x;

	}

	/**
	 * Get the current velocity of this cloud
	 * 
	 * @return velocity of cloud
	 */
	public int getVelocity() {
		return this.velocity;
	}

	// All general mutators

	/**
	 * set a new velocity for this cloud
	 * 
	 * @param vel
	 */
	public void setVelocity(int vel) {
		this.velocity = vel;
	}

	/**
	 * Set a new x position for this cloud
	 * 
	 * @param x
	 */
	public void setX(float x) {
		this.x = x;
	}

	/**
	 * Set a new Y position for this cloud
	 * 
	 * @param y
	 */
	public void setY(float y) {
		this.y = y;
	}
}
