public class Point {
    //A "Point" object has a (X,Y) co-ordinate, representing a line in the
    //game airspace along the altitude axis. Values set individually as (xCoOrd, 
    // yCoOrd) double variables, initialised by default as 0,0.
    
    //If this helps:
    //String pointRef is the letter reference (Point A would be "A").
    //Class Var. static int PointCount tracks number of Points set up.
    double xCoOrd;
    double yCoOrd;
    String pointRef = "";
    static int pointCount = 0;
    
    //Point Constructor take two doubles, for x then y.
    Point(double b, double c){
    xCoOrd = b;
    yCoOrd = c;
    pointCount += 1;}
    
    //Point Constructor that also takes pointRef string:
    Point(double d, double e, String f){
    xCoOrd = d; 
    yCoOrd = e;
    pointRef = f;
    pointCount += 1;
    System.out.println("Point " + f + " set:(" + d + "," + e +").");
    }
    
    //Constructor pointing out that X,Y has not been set.
    Point(){
    System.out.println("Warning: Constructor method for Point Object called, "
            + "point set as (0,0) with no point ref.");
    pointCount +=1;
    }
    
    
    //Accessor methods for xCoOrd double, for yCoOrd double, 
    //for pointRef string, for pointcount int.
    double getXCoOrd() {return this.xCoOrd;}
    double getYCoOrd() {return this.yCoOrd;}
    String getPointRef() {return this.pointRef;}
    int getPointCount() {return Point.pointCount;}
    
    //Mutator for setting xCoOrd and yCoOrd. Each method takes a sole double.
    void setXCoOrd(double newX) {this.xCoOrd = newX;}
    void setYCoOrd(double newY) {this.yCoOrd = newY;}
    
    //If this helps:
    //Accessor method returning an array containing xCoOrd and yCoOrd
    double[] coOrdArray = {xCoOrd, yCoOrd};
    double[] getPointArray() {return coOrdArray;}
}

//Defining the subclasses.
class EntryPoint extends Point {}
class WayPoint extends Point {}
class ExitPoint extends Point {}

//Debugging.
class DebuggingRun {
    public static void main (String[] arguments) {
        Point A = new Point();
        Point B = new Point(10,11,"B");
        System.out.println(B.getXCoOrd());
        System.out.println(B.getYCoOrd());
        System.out.println(B.getPointCount());
        Point C = new Point();
        Point D = new Point();
        System.out.println(D.getPointCount());
    }
}