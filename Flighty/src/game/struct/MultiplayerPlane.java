package game.struct;

class MultiplayerPlane extends Plane {
	
	public MultiplayerPlane() {
		// required for serialization
	}

	public MultiplayerPlane(int id, double velocity, int altitude,
			double bearing, Game currentGame, long uniqueNetworkObjectId) {
		super(id, velocity, altitude, bearing, currentGame, uniqueNetworkObjectId);
	}

	@Override
	public boolean allowedToLand() {
		return (!currentGame.getAirport().isPlaneLanding()
				&& currentGame.getAirport().getLandingApproachArea()
				.contains((float) getX(), (float) getY())
		&& ((true && ((getBearing() >= takeoffAngleHighMulti && getBearing() <= 359)
				|| (getBearing() <= takeoffAngleLowMulti && getBearing() >= 0))))
		&& getAltitude() <= 2000);
	}

	@Override
	public void setBearingForTakeoff() {
		setBearing(180);
	}
	
}