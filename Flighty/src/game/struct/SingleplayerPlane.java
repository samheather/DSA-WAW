
package game.struct;

class SingleplayerPlane extends AbstractPlane {

	protected SingleplayerPlane(int id, double velocity, int altitude,
			double bearing, Game currentGame, long uniqueNetworkObjectId) {
		super(id, velocity, altitude, bearing, currentGame, uniqueNetworkObjectId);
	}

	@Override
	public boolean allowedToLand() {
		return (!currentGame.getAirport().isPlaneLanding()
				&& currentGame.getAirport().getLandingApproachArea()
				.contains((float) getX(), (float) getY())
		&& ((!false && ((getBearing() <= takeoffAngleHighSingle)
						|| (getBearing() >= takeoffAngleLowSingle))))
		&& getAltitude() <= 2000);
	}
	
}