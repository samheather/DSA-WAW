package game.struct;

public class Cloud {

	/** Speed the cloud is traveling at */
	private int velocity;

	/** Current X co-ordinate */
	private float x;
	/** Current Y co-ordinate */
	private float y;
	
	private Point end = new Point();
	
	/** Required by Slick2D */
	public transient Game currentGame;
	
	
	public Cloud(){
	}
	
	public  Cloud(int velocity, float x, float y, Game currentGame){
		this.currentGame = currentGame;
		this.velocity = velocity;
		this.x = x;
		this.y = y;
		this.end.setX(x);
		if(y == currentGame.windowHeight){
			this.end.setY(0);
		}else{this.end.setY(currentGame.windowHeight);}
	}
	
	/** Updates x and y coordinates */
	public void updateXYCoordinates() {
		if( getY() > this.end.getY()){
			setY(getY() - velocity);	
		}else if(getY() < this.end.getY()){
			setY(getY() + velocity);	
		}
		if( getX() > this.end.getX()){
			setX(getX() - velocity);	
		}else if(getX() < this.end.getY()){
			setX(getX() + velocity);	
		}
		
	}
	
	public boolean moveCloud(){
		if( getY()!= this.end.getY()){
			updateXYCoordinates();
			return false;
		}
		return true;
	}
	
	
	public float getY(){
		return this.y;
	}
	
	public float getX(){
		return this.x;
		
	}
	
	public int getVelocity(){
		return this.velocity;
	}
	
	public void setVelocity(int vel){
		this.velocity = vel;
	}
	
	public void setX(float x){
		this.x = x;
	}
	
	public void setY(float y){
		this.y = y;
	}
}
