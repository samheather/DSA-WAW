import java.util.ArrayList;



public class Game   {
	static ArrayList<Waypoint> wps= new ArrayList<Waypoint>();
	static ArrayList<ExitPoint> exp = new ArrayList<ExitPoint>();
	public static void main(String[] args) {
		Waypoint wp1 = new Waypoint(10,10);
		Waypoint wp2 = new Waypoint(10,10);
		Waypoint wp3 = new Waypoint(10,10);
		Waypoint wp4 = new Waypoint(10,10);
		Waypoint wp5 = new Waypoint(10,10);
		Waypoint wp6 = new Waypoint(10,10);
		Waypoint wp7 = new Waypoint(10,10);
		Waypoint wp8 = new Waypoint(10,10);
		Waypoint wp9 = new Waypoint(10,10);
		Waypoint wp10 = new Waypoint(10,10);
		ExitPoint exp1 = new ExitPoint(10,10);
		ExitPoint exp2 = new ExitPoint(10,10);
		ExitPoint exp3 = new ExitPoint(10,10);
		
		wps.add(wp1);
		wps.add(wp2);
		wps.add(wp3);
		wps.add(wp4);
		wps.add(wp5);
		wps.add(wp6);
		wps.add(wp7);
		wps.add(wp8);
		wps.add(wp9);
		wps.add(wp10);
		
		exp.add(exp1);
		exp.add(exp2);
		exp.add(exp3);
		AirSpace a = new AirSpace();
		a.setList_of_waypoints(wps);
		a.setList_of_exitpoints(exp);
		FlightPlan f = new FlightPlan(a);
		System.out.println(f.getWaypoints().toString());
		System.out.println(f.getVelocity());
		
	}
	

}
