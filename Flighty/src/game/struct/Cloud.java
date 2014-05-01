package game.struct;

import java.util.Random;

public abstract class Cloud {

	/** Size to display cloud */
	private int size;

	/** Speed the cloud is traveling at */
	private double velocity;

	/** Current altitude */
	private int altitude;

	/** Current bearing in radians */
	private double bearing;

	/** Current X co-ordinate */
	private double x;

	/** Current Y co-ordinate */
	private double y;

	/** Current destination */
	private Point start = new Point();
	
	private Point end = new Point();
	
	/** Required by Slick2D */
	public transient Game currentGame;
	
	Random rnd = new Random();
	
	public Cloud(){
	}
	
	public  Cloud(int size, double velocity, Game currentGame, double bearing){
		this.currentGame = currentGame;
		this.altitude = 10000;
		this.size = size;
		this.velocity = velocity;
		this.bearing = bearing;
		this.start.setY(rnd.nextInt(4) * currentGame.windowHeight / 4);
		this.start.setX(0);
		this.end.setY(rnd.nextInt(4) * currentGame.windowHeight / 4);
		this.end.setX(currentGame.windowWidth);
		
		
		
		
	}
	
	/** Updates x and y coordinates */
	public void updateXYCoordinates() {
		setX((float) (getX() - (Math.cos(Math.toRadians(getBearing())) * (currentGame
				.getSpeedDifficulty() * getVelocity()))));
	
		setY((float) (getY() - (Math.sin(Math.toRadians(getBearing())) * (currentGame
				.getSpeedDifficulty() * getVelocity()))));
	}
	
	public boolean moveCloud(){
		if( getX()!= this.end.getX()){
			updateXYCoordinates();
			return false;
		}
		return true;
	}
	
	public int getSize(){
		return this.size;
	}
	
	public double getBearing(){
		return this.bearing;
	}
	
	public double getY(){
		return this.y;
	}
	
	public double getX(){
		return this.x;
		
	}
	
	public double getVelocity(){
		return this.velocity;
	}
	
	public void setVelocity(double vel){
		this.velocity = vel;
	}
	
	public void setX(double x){
		this.x = x;
	}
	
	public void setY(double y){
		this.y = y;
	}
	
	public void setBearing(double bearing){
		this.bearing = bearing;
	}
	
	public void setSize(int size){
		this.size = size;
	}
}
