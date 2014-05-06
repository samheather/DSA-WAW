package game.struct;

/**
 * A plane in singleplayer
 * @author mjm540
 *
 */
public class SingleplayerPlane extends Plane {

/**
 * Constructor
 * @param id
 * The identifier of the flight
 * @param velocity
 * The velocity of the plane
 * @param altitude
 * The altitude of the plane
 * @param bearing
 * The bearing of the plane
 * @param currentGame
 * The game object associated with the plane
 * @param uniqueNetworkObjectId
 * A unique id for the plane that is the same across all clients in a networked game
 */
	public SingleplayerPlane(int id, double velocity, int altitude,
			double bearing, Game currentGame, long uniqueNetworkObjectId) {
		super(id, velocity, altitude, bearing, currentGame,
				uniqueNetworkObjectId);
	}

	@Override
	public boolean allowedToLand() {
		return (!currentGame.getAirport().isPlaneLanding()
				&& currentGame.getAirport().getLandingApproachArea()
						.contains((float) getX(), (float) getY())
				&& ((!false && ((getBearing() <= takeoffAngleHighSingle) || (getBearing() >= takeoffAngleLowSingle)))) && getAltitude() <= 2000);
	}

	@Override
	public void setBearingForTakeoff() {
		setBearing(0);
	}

}