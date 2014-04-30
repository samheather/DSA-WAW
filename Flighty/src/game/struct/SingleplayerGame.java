package game.struct;

import java.io.IOException;
import java.net.UnknownHostException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

import org.newdawn.slick.GameContainer;
import org.newdawn.slick.state.StateBasedGame;

public class SingleplayerGame extends Game {


	
	private ArrayList<SingleplayerPlane> singleplayerPlanes = new ArrayList<SingleplayerPlane>();
	
	@Override
	public List<? extends Plane> getCurrentPlanes() {
		return singleplayerPlanes;
	}

	@Override
	protected Plane constructPlane(int id, double velocity, int altitude,
			double bearing, long uniqueNetworkObjectId) {
		SingleplayerPlane p = new SingleplayerPlane(id, velocity, altitude, bearing, this, uniqueNetworkObjectId);
		singleplayerPlanes.add(p);
		return p;
	}
	
	
	public SingleplayerGame(int newSeparationDistance, int newPenaltyDistance,
			int distFromLeft)
			throws NoSuchAlgorithmException, UnknownHostException, IOException {
		super(newSeparationDistance, newPenaltyDistance, distFromLeft);
		System.out.println("singlep game constructed");
	}

	@Override
	protected Airport createAirport() {
		return new Airport(720, 460, 1180, -320, 230);
	}

	@Override
	protected ArrayList<Point> createExitPoints() {
		ArrayList<Point> exitPoints = new ArrayList<Point>();
		exitPoints.add(new ExitPoint(1200, 300));
		exitPoints.add(new ExitPoint(950, 0));
		return exitPoints;
	}

	@Override
	protected ArrayList<Waypoint> createWayPoints() {
		ArrayList<Waypoint> waypoints = new ArrayList<Waypoint>();
		waypoints.add(new Waypoint(1100, 140));
		waypoints.add(new Waypoint(1150, 330));
		waypoints.add(new Waypoint(910, 150));
		waypoints.add(new Waypoint(850, 310));
		waypoints.add(new Waypoint(700, 200));
		return waypoints;
	}

	@Override
	protected ArrayList<Point> createEntryPoints() {
		ArrayList<Point> entryPoints = new ArrayList<Point>();
		entryPoints.add(new EntryPoint(1200, 200));
		entryPoints.add(new EntryPoint(750, 0));
		return entryPoints;
	}

	@Override
	protected void configurePlane(Plane p) {
		p.ownedByCurrentPlayer = true;
	}

	@Override
	protected void planeUpdate(Plane plane) {
		if ((plane.getX() > windowWidth)
				|| (plane.getX() < distFromLeftEdge)
				|| (plane.getY() > windowHeight) || (plane.getY() < 0)) {
			// Updates score if plane in game area
			if (plane.ownedByCurrentPlayer)
				getScore().planeLeftAirspaceOrWaitingToTakeOffMinusScore();

			// Deselects plane that left the airspace
			if (currentPlane != null) {
				if (plane.equals(currentPlane)) {
					currentPlane = null;
				}
			}

			// Removes planes that left the airspace
			plane.markForDeletion();
		}
		
	}
	
	@Override
	public void update(GameContainer gameContainer, StateBasedGame game)
			throws IOException {
		super.update(gameContainer, game);
		ListIterator<SingleplayerPlane> i = singleplayerPlanes
				.listIterator();
		while (i.hasNext()) {
			SingleplayerPlane p = i.next();
			if (p.deleted())
				i.remove();
		}
	}

	@Override
	public void endingRoutine() {
		// TODO Auto-generated method stub
		
	}

}
