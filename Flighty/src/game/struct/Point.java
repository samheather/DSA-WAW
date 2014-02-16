package game.struct;


public class Point
{
    protected double x;
    protected double y;
    protected String pointRef;   
	
	// CONSTRUCTORS
    
    //Point Constructor taking two doubles for X then Y coordinates.
    public Point(double xcoord, double ycoord)
	{
        x = xcoord;
        y = ycoord;
     }
    
    public Point(){
    	
    }
    
    // SETTERS AND GETTERS
    
    public double getX()
	{
    	return this.x;
    }
    
    public double getY()
	{
    	return this.y;
    }
    
    public void setX(double newX)
	{
    	this.x = newX;
    }
    
    public void setY(double newY)
	{
    	this.y = newY;
    }
    
    public String getPointRef()
	{
    	return this.pointRef;
    }
    
    public void setPointRef(String pointRef)
	{
  		this.pointRef = pointRef;
  	}
    
    public boolean equals(Point point){
    	if(point instanceof Point)
    	{
    		if ((point.getX() ==  this.x) && (point.getY() == this.y))
    		{
    			return true;
    		}
    	}
    	
    	return false;
   }
}