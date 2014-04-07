package game.struct;

import java.io.IOException;
import java.net.UnknownHostException;
import java.security.NoSuchAlgorithmException;
import java.util.ListIterator;

import org.newdawn.slick.GameContainer;
import org.newdawn.slick.state.StateBasedGame;

import game.network.*;

public class MultiplayerGame extends Game {
	
	Protocol protocol;
	
	public MultiplayerGame() {
		super();
	}

	public MultiplayerGame(Protocol protocol, int newSeparationDistance, int newPenaltyDistance,
			int distFromLeft) throws NoSuchAlgorithmException,
			UnknownHostException, IOException {
		super(newSeparationDistance, newPenaltyDistance, distFromLeft);
		// TODO Auto-generated constructor stub
		this.protocol = protocol;
	}
	
	@Override
	public void update(GameContainer gameContainer, StateBasedGame game)
			throws IOException {
		Message.Receivable r = null;
		while((r = protocol.getMessage()) != null) {
			if (r instanceof Message.Error) {
				System.out.println("error!");
				((Message.Error) r).log();
			} else if (r instanceof Message.ClientClient.CCObject) {
				Plane p = (Plane)((Message.ClientClient.CCObject) r).getObject();
				p.currentGame = this;
				p.resetSyncState();
				ListIterator<Plane> i = getCurrentPlanes().listIterator();
				while (i.hasNext()) {
					Plane p2 = i.next();
					if (p2.getUniqueNetworkObjectID() == p
							.getUniqueNetworkObjectID()) {
						i.set(p);
						System.out.println("received existing plane");
						p = null;
						break;
					}
				}
				if (p != null){
					getCurrentPlanes().add(p);
					System.out.println("received new plane");
				}
			}
		}
		super.update(gameContainer, game);
		for (Plane plane : getCurrentPlanes()) {
			if (plane.needsSyncing()) {
				protocol.putMessage(new Message.ClientClient.CCObject(plane));
				plane.resetSyncState();
				//System.out.println("sent plane");
			}
		}
	}

}
