package game.struct;

class MultiplayerPlane extends Plane {

	/**
	 * Empty constructor, required for serialization
	 */
	public MultiplayerPlane() {
		// required for serialization
	}

	/**
	 * Constructor, passes all back to superclass
	 * 
	 * @param id
	 * @param velocity
	 * @param altitude
	 * @param bearing
	 * @param currentGame
	 * @param uniqueNetworkObjectId
	 */
	public MultiplayerPlane(int id, double velocity, int altitude,
			double bearing, Game currentGame, long uniqueNetworkObjectId) {
		super(id, velocity, altitude, bearing, currentGame,
				uniqueNetworkObjectId);
	}

	/**
	 * Special override to determine if plane is allowed to land.
	 */
	@Override
	public boolean allowedToLand() {
		return (!currentGame.getAirport().isPlaneLanding()
				&& currentGame.getAirport().getLandingApproachArea()
						.contains((float) getX(), (float) getY())
				&& ((true && ((getBearing() >= takeoffAngleHighMulti && getBearing() <= 359)
						|| (getBearing() <= takeoffAngleLowMulti && getBearing() >= 0))))
						&& getAltitude() <= 2000);
	}

	/**
	 * Take off bearings change because of mirroring, hence set manually below.
	 */
	@Override
	public void setBearingForTakeoff() {
		setBearing(180);
	}

}