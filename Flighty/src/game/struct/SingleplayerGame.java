package game.struct;

import game.vision.LocateFace;

import java.io.IOException;
import java.net.UnknownHostException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

import org.newdawn.slick.GameContainer;
import org.newdawn.slick.state.StateBasedGame;

public class SingleplayerGame extends Game {

	/**Face Locator */
	private LocateFace faceLocator;
	private float faceTrackingTranslationalScaleFactor = 0.1f;
	private float faceTrackingZoomScaleFactor = 0.01f;
	
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
			int distFromLeft, int multiplier)
			throws NoSuchAlgorithmException, UnknownHostException, IOException {
		super(newSeparationDistance, newPenaltyDistance, distFromLeft);
		faceLocator = new LocateFace();
		System.out.println("singlep game constructed");
		this.getScore().setMultiplier(multiplier);
	}
	
	@Override
	public void update(GameContainer gameContainer, StateBasedGame game) throws IOException {
		super.update(gameContainer, game);
		// Rescan for faces to get updated face locations
		boolean faceScanSuccessful = false;
		faceScanSuccessful = faceLocator.updateFacePosition();
		
		for (Plane plane : getCurrentPlanes()) {
			plane.movePlane();
 			if (faceScanSuccessful) {
 				plane.updateFaceDetectionPosition(
 						faceLocator.getImmediateHorizontalAngle(true)*faceTrackingTranslationalScaleFactor,
 						faceLocator.getImmediateVerticalAngle(true)*faceTrackingTranslationalScaleFactor,
 						(int)(faceLocator.getImmediateDistance(true)*faceTrackingZoomScaleFactor));
 			}
		}
		
		ListIterator<SingleplayerPlane> i = singleplayerPlanes
				.listIterator();
		while (i.hasNext()) {
			SingleplayerPlane p = i.next();
			if (p.deleted())
				i.remove();
		}
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
	protected void planeUpdate(Plane plane, GameContainer gameContainer) {
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
	public void endingRoutine() {
		// TODO Auto-generated method stub
		
	}

}
