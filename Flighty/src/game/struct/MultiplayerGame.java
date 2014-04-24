package game.struct;

import java.io.IOException;
import java.net.UnknownHostException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;

import org.lwjgl.opengl.Display;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.state.StateBasedGame;

import game.gfx.WindowManager;
import game.network.*;

public class MultiplayerGame extends Game {
	
	
	private class MultiplayerPlane extends Game.Plane {
		
	}

	public MultiplayerGame(int newSeparationDistance, int newPenaltyDistance,
			int distFromLeft) throws NoSuchAlgorithmException,
			UnknownHostException, IOException {
		super(newSeparationDistance, newPenaltyDistance, distFromLeft, true);
		protocol.putMessage(new Message.ClientServer.BeginMM());
	}
	int state = 0;
	
	private ArrayList<MultiplayerPlane> multiplayerPlanes = new ArrayList<MultiplayerPlane>();
	
	
	Protocol protocol = new Protocol("multi.atcga.me", 1025, Arrays.asList((Class)MultiplayerPlane.class));
	
	@Override
	public void removePlane(AbstractPlane toDelete) {
		toDelete.markForDeletion();
	}
	
	@Override
	public void update(GameContainer gameContainer, StateBasedGame game)
			throws IOException {
		if (state == 0) {
			
			//waiting for connection to server
			Message.Receivable r = protocol.getMessage();
			if (r == null)
				return;
			if (r instanceof Message.ServerClient.AckBeginMM) {
				state = 1;
				System.out.println("in mm");
			}
		} else if (state == 1) {
			
			// waiting for client
			Message.Receivable r = protocol.getMessage();
			if (Display.isCloseRequested()) {
				protocol.putMessage(new Message.ClientServer.CancelMM());
				game.enterState(WindowManager.MAIN_MENU_STATE);
			}
			if (r == null)
				return;
			if (r instanceof Message.ServerClient.FoundGame) {
				state = 2;
				System.out.println("got game");
			}
		} else if (state == 2) {
			Message.Receivable r = null;
			while((r = protocol.getMessage()) != null) {
				if (r instanceof Message.Error) {
					System.out.println("error!");
					((Message.Error) r).log();
				} else if (r instanceof Message.ClientClient.CCObject) {
					MultiplayerPlane p = (MultiplayerPlane)((Message.ClientClient.CCObject) r).getObject();
					p.currentGame = this;
					p.resetSyncState();
					p.ownedByCurrentPlayer = !p.ownedByCurrentPlayer;
					ListIterator<MultiplayerPlane> i = multiplayerPlanes.listIterator();
					while (i.hasNext()) {
						AbstractPlane p2 = i.next();
						if (p2.getUniqueNetworkObjectID() == p
								.getUniqueNetworkObjectID()) {
							//p.ownedByCurrentPlayer = false;
							if (p.deleted())
								i.remove();
							else
								i.set(p);
							System.out.println("received existing plane");
							p = null;
							break;
						}
					}
					if (p != null && !p.deleted()){
						//p.ownedByCurrentPlayer = false;
						multiplayerPlanes.add(p);
						System.out.println("received new plane");
						
					}
				}
			}
			super.update(gameContainer, game);
			ListIterator<MultiplayerPlane> i = multiplayerPlanes.listIterator();
			while (i.hasNext()) {
				AbstractPlane plane = i.next();
				if (plane.needsSyncing()) {
					//plane.ownedByCurrentPlayer = true;
					protocol.putMessage(new Message.ClientClient.CCObject(plane));
					plane.resetSyncState();
					//System.out.println("sent plane");
				}
				if (plane.deleted())
					i.remove();
			}
		}
	}

	@Override
	protected Airport createAirport() {
		return new Airport(415, 515, 170, 200, 100);
	}

	@Override
	protected ArrayList<Point> createExitPoints() {
		ArrayList<Point> exitPoints = new ArrayList<Point>();
		exitPoints.add(new ExitPoint(
				(windowWidth + distFromLeftEdge) / 2, (windowHeight / 2)));
		return exitPoints;
	}

	@Override
	protected ArrayList<Waypoint> createWayPoints() {
		ArrayList<Waypoint> waypoints = new ArrayList<Waypoint>();
		waypoints .add(new Waypoint(540, 115));
		waypoints.add(new Waypoint(430, 400));
		return waypoints;
	}

	@Override
	protected ArrayList<Point> createEntryPoints() {
		return new ArrayList<Point>();
	}

	@Override
	protected void configurePlane(AbstractPlane p) {
		//p.ownedByCurrentPlayer = false;
	}

	@Override
	protected void planeUpdate(AbstractPlane plane) {
		if ((plane.getX() < distFromLeftEdge)
				|| (plane.getY() > windowHeight) || (plane.getY() < 0)) {
			// Updates score if plane in game area
			getScore().planeLeftAirspaceOrWaitingToTakeOffMinusScore();

			// Deselects plane that left the airspace
			if (currentPlane != null) {
				if (plane.equals(currentPlane)) {
					currentPlane = null;
				}
			}

			// Removes planes that left the airspace
			removePlane(plane);

		} else if (plane.getX() > (windowWidth + distFromLeftEdge) / 2) {
			System.out.println(plane);
			System.out.println(plane.getX());
			System.out.println((windowWidth + distFromLeftEdge) / 2);
				// Updates score if plane in game area
				getScore()
						.planeLeftAirspaceOrWaitingToTakeOffMinusScore();
				

				// Deselects plane that left the airspace
				if (currentPlane != null) {
					currentPlane.setOwnedByCurrentPlayer(false);
					if (plane.equals(currentPlane)) {
						currentPlane = null;
						
					}
				}
				removePlane(plane);
			}
	}

	@Override
	public List<? extends AbstractPlane> getCurrentPlanes() {
		return multiplayerPlanes;
	}

	@Override
	protected AbstractPlane constructPlane() {
		MultiplayerPlane p = new MultiplayerPlane();
		multiplayerPlanes.add(p);
		return p;
	}

}
