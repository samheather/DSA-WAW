public class Calculations {

	Calculations() {
	}

	public static double lateral_distance_between_flights(Flight flight1,
			Flight flight2) {

		return Math.sqrt(Math.pow((flight1.getX() - flight2.getX()), 2)
				+ Math.pow((flight1.getY() - flight2.getY()), 2));

	}

	public static int vertical_distance_between_flights(Flight flight1,
			Flight flight2) {

		return Math.abs(flight1.getAltitude() - flight2.getAltitude());

	}

	public static double lateral_distance_between_flight_and_waypoint(
			Flight flight, Waypoint waypoint) {

		return Math.sqrt(Math.pow((flight.getX() - waypoint.getX()), 2)
				+ Math.pow((flight.getY() - waypoint.getY()), 2));

	}

	// destination x and destination y will be replaced with the waypoints x and y
	public static double calculate_heading_to_first_waypoint(Flight flight1,
			double destination_x, double destination_y) {
		double deltaX;
		double deltaY;
		deltaY = destination_y - flight1.getY();
		deltaX = destination_x - flight1.getX();
		double angle = Math.toDegrees(Math.atan2(deltaY, deltaX));
		angle += 90;
		if (angle < 0) {
			angle += 360;
		}
		return angle;

		// DIAGONALS
		/*
		 * if ((flight1.getX() > destination_x) && (flight1.getY() < destination_y)){ return 360 -
		 * Math.toDegrees(Math.atan((Math.abs(flight1.getX()-destination_x) / Math.abs(flight1.getY()-destination_y)))); }
		 * 
		 * else if ((flight1.getX() < destination_x) && (flight1.getY() < destination_y)){ return
		 * Math.toDegrees(Math.atan((Math.abs(flight1.getX()-destination_x) / Math.abs(flight1.getY()-destination_y)))); }
		 * 
		 * else if ((flight1.getX() < destination_x) && (flight1.getY() > destination_y )){ return 180 -
		 * Math.toDegrees(Math.atan((Math.abs(flight1.getX()-destination_x) / Math.abs(flight1.getY()-destination_y)))); }
		 * 
		 * else if ((flight1.getX() > destination_x) && (flight1.getY() > destination_y )){ return 180 +
		 * Math.toDegrees(Math.atan((Math.abs(flight1.getX()-destination_x) / Math.abs(flight1.getY()-destination_y)))); }
		 * 
		 * // HORIZONTALS
		 * 
		 * else if ((flight1.getX() < destination_x) && (flight1.getY() == destination_y)){ return 90; }
		 * 
		 * else if ((flight1.getX() > destination_x) && (flight1.getY() == destination_y)){ return 270; }
		 * 
		 * //VERTICALS
		 * 
		 * else if ((flight1.getX() == destination_x) && (flight1.getY() < destination_y)){ return 0; }
		 * 
		 * // Below = ((flight1.x == destination_x) && (flight1.y > destination_y))
		 * 
		 * else{ return 180; }
		 */

	}
	public static boolean check_if_point_in_circle(double pointX, double pointY, double circleX, double circleY, int radius) {
		double distance_between_points = Math.sqrt(Math.pow(pointX-circleX, 2)+Math.pow(pointY-circleY, 2));
		return distance_between_points<=radius;
	}
}
