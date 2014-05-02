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
import game.network.Message;
import game.network.Protocol;

public class MultiplayerGame extends Game {

	public MultiplayerGame(int newSeparationDistance, int newPenaltyDistance,
			int distFromLeft) throws NoSuchAlgorithmException,
			UnknownHostException, IOException {
		super(newSeparationDistance, newPenaltyDistance, distFromLeft);
		protocol.putMessage(new Message.ClientServer.BeginMM());
		WindowManager.opponentFound = false;
	}

	int state = 0;

	private ArrayList<MultiplayerPlane> multiplayerPlanes = new ArrayList<MultiplayerPlane>();

	// TODO(jamaal) please supress if necessary
	Protocol protocol = new Protocol("multi.atcga.me", 1025, Arrays.asList(
			(Class) MultiplayerPlane.class, Score.class));

	@Override
	public void update(GameContainer gameContainer, StateBasedGame game)
			throws IOException {
		if (state == 0) {
			WindowManager.opponentFound = false;
			// waiting for connection to server
			Message.Receivable r = protocol.getMessage();
			if (r == null) {
				return;
			} else if (r instanceof Message.Error) {
				System.out.println("error!");
				((Message.Error) r).log();
				System.exit(1);
			} else if (r instanceof Message.ServerClient.AckBeginMM) {
				state = 1;
				System.out.println("in mm");
			}
		} else if (state == 1) {
			WindowManager.opponentFound = false;
			// waiting for client
			Message.Receivable r = protocol.getMessage();
			if (Display.isCloseRequested()) {
				protocol.putMessage(new Message.ClientServer.CancelMM());
				game.enterState(WindowManager.MAIN_MENU_STATE);
			}
			if (r == null) {
				return;
			} else if (r instanceof Message.Error) {
				System.out.println("error!");
				((Message.Error) r).log();
				System.exit(1);
			} else if (r instanceof Message.ServerClient.FoundGame) {
				state = 2;
				System.out.println("got game");
			}
		} else if (state == 2) {
			WindowManager.opponentFound = true;
			Message.Receivable r = null;
			while ((r = protocol.getMessage()) != null) {
				if (r instanceof Message.Error) {
					System.out.println("error!");
					((Message.Error) r).log();
					System.exit(1);
				} else if (r instanceof Message.ServerClient.OpponentQuit) {
					WindowManager.endingText = "Opponent quit the game you won";
					state = 3;
					setEnding(true);
					return;
				} else if (r instanceof Message.ClientClient.CCObject) {
					Object o = ((Message.ClientClient.CCObject) r).getObject();
					if (o instanceof Score) {
						protocol.putMessage(new Message.ClientClient.CCObject(
								this.getScore()));
						protocol.putMessage(new Message.ClientServer.RequestQuit());
						WindowManager.endingText = "Opponent died their score was  "
								+ ((Score) (o)).getScore();
						state = 3;
						setEnding(true);
						return;
					} else if (o instanceof MultiplayerPlane) {
						MultiplayerPlane p = (MultiplayerPlane) o;
						p.currentGame = this;
						p.resetSyncState();
						p.ownedByCurrentPlayer = !p.ownedByCurrentPlayer;
						ListIterator<MultiplayerPlane> i = multiplayerPlanes
								.listIterator();
						while (i.hasNext()) {
							Plane p2 = i.next();
							if (p2.equals(p)) {
								// p.ownedByCurrentPlayer = false;
								if (p.deleted())
									i.remove();
								else
									i.set(p);
								System.out.println("received existing plane");
								p = null;
								break;
							}
						}
						if (p != null && !p.deleted()) {
							// p.ownedByCurrentPlayer = false;
							multiplayerPlanes.add(p);
							System.out.println("received new plane");
						}
					} else if (o instanceof AutoPilot) {
						WindowManager.autopilotInit = true;
						WindowManager.autopilotOff = true;
					}
				}
			}
			super.update(gameContainer, game);
			ListIterator<MultiplayerPlane> i = multiplayerPlanes.listIterator();
			while (i.hasNext()) {
				MultiplayerPlane plane = i.next();
				if (plane.needsSyncing()) {
					// plane.ownedByCurrentPlayer = true;
					protocol.putMessage(new Message.ClientClient.CCObject(plane));
					plane.resetSyncState();
					// System.out.println("sent plane");
				}
				if (plane.deleted())
					i.remove();
			}
			if (WindowManager.turnOffAutopilot) {
				AutoPilot autopilot = new AutoPilot();
				protocol.putMessage(new Message.ClientClient.CCObject(autopilot));
				WindowManager.turnOffAutopilot = false;
			}
		} else if (state == 3) {
			super.update(gameContainer, game);
		}
	}

	@Override
	protected Airport createAirport() {
		return new Airport(415, 515, 170, 200, 100);
	}

	@Override
	protected ArrayList<Point> createExitPoints() {
		ArrayList<Point> exitPoints = new ArrayList<Point>();
		exitPoints.add(new ExitPoint((windowWidth + distFromLeftEdge) / 2,
				(windowHeight / 2)));
		return exitPoints;
	}

	@Override
	protected ArrayList<Waypoint> createWayPoints() {
		ArrayList<Waypoint> waypoints = new ArrayList<Waypoint>();
		waypoints.add(new Waypoint(540, 115));
		waypoints.add(new Waypoint(430, 400));
		return waypoints;
	}

	@Override
	protected ArrayList<Point> createEntryPoints() {
		return new ArrayList<Point>();
	}

	@Override
	protected void configurePlane(Plane p) {
		// p.ownedByCurrentPlayer = false;
	}

	@Override
	protected void planeUpdate(Plane plane, GameContainer gameContainer) {
		if ((plane.getX() < distFromLeftEdge) || (plane.getY() > windowHeight)
				|| (plane.getY() < 0)) {
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

		} else if ((plane.getX() > (windowWidth + distFromLeftEdge) / 2)) { // FIXME This handles things
													// off the right edge
			// Updates score if plane in game area
			if (plane.ownedByCurrentPlayer) {
				getScore().planeLeftAirspaceOrWaitingToTakeOffMinusScore();

				// Deselects plane that left the airspace
				plane.setBearing(plane.getBearing() + Math.PI);
				plane.setTarget(plane.getFlightPlan().getCurrentRoute().get(0));
				plane.setY(gameContainer.getHeight() - plane.getY());
				plane.setX(plane.getX() - 30); //TODO remove this -30 once fixed
				/*plane.clearFlightPlan();
				plane.setFlightPlan(new FlightPlan(this, plane));*/
				if (plane.equals(currentPlane)) {
					currentPlane = null;
				}
				plane.setOwnedByCurrentPlayer(false);
				plane.setAuto();
			} else {
				plane.setOwnedByCurrentPlayer(true);
			}
		}
	}

	@Override
	public List<? extends Plane> getCurrentPlanes() {
		return multiplayerPlanes;
	}

	@Override
	protected Plane constructPlane(int id, double velocity, int altitude,
			double bearing, long uniqueNetworkObjectId) {
		MultiplayerPlane p = new MultiplayerPlane(id, velocity, altitude,
				bearing, this, uniqueNetworkObjectId);
		multiplayerPlanes.add(p);
		return p;
	}

	@Override
	public void endingRoutine() {
		if (state != 3) {
			protocol.putMessage(new Message.ClientClient.CCObject(this
					.getScore()));

			for (;;) {
				Message.Receivable r = protocol.getMessage();
				if (r != null && r instanceof Message.ClientClient.CCObject) {
					if (((Message.ClientClient.CCObject) (r)).getObject() instanceof Score) {
						for (;;) {
							Message.Receivable r2 = protocol.getMessage();
							if (r2 == null)
								continue;
							if (r2 instanceof Message.ServerClient.OpponentQuit) {
								break;
							}
						}
						state = 3;
						WindowManager.endingText = "You died Opponents score was  "
								+ ((Score) (((Message.ClientClient.CCObject) (r))
										.getObject())).getScore();
						break;
					}
				}
			}
		}
	}
}
