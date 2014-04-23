package game.struct;

import java.io.IOException;
import java.net.UnknownHostException;
import java.security.NoSuchAlgorithmException;

public class SingleplayerGame extends Game {

	public SingleplayerGame(int newSeparationDistance, int newPenaltyDistance,
			int distFromLeft)
			throws NoSuchAlgorithmException, UnknownHostException, IOException {
		super(newSeparationDistance, newPenaltyDistance, distFromLeft,
				false);
		System.out.println("singlep game constructed");
		// TODO Auto-generated constructor stub
	}
	
	@Override
	public void removePlane(Plane toDelete) {
		reallyRemovePlane(toDelete);
	}

}
