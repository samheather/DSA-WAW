package game.struct;

import java.io.IOException;
import java.net.UnknownHostException;
import java.security.NoSuchAlgorithmException;
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
		super(newSeparationDistance, newPenaltyDistance, distFromLeft, true);
		protocol.putMessage(new Message.ClientServer.BeginMM());
		WindowManager.opponentFound = false;
	}
	int state = 0;
	
	
	Protocol protocol = new Protocol("multi.atcga.me", 1025);
	
	@Override
	public void removePlane(Plane toDelete) {
		toDelete.delete();
	}
	
	@Override
	public void update(GameContainer gameContainer, StateBasedGame game)
			throws IOException {
		if (state == 0) {
			WindowManager.opponentFound = false;
			//waiting for connection to server
			Message.Receivable r = protocol.getMessage();
			if (r == null)
				return;
			if (r instanceof Message.ServerClient.AckBeginMM) {
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
			if (r == null)
				return;
			if (r instanceof Message.ServerClient.FoundGame) {
				state = 2;
				System.out.println("got game");
			}
		} else if (state == 2) {
			WindowManager.opponentFound = true;
			Message.Receivable r = null;
			while((r = protocol.getMessage()) != null) {
				if (r instanceof Message.Error) {
					System.out.println("error!");
					((Message.Error) r).log();
				} else if (r instanceof Message.ClientClient.CCObject) {
					Plane p = (Plane)((Message.ClientClient.CCObject) r).getObject();
					p.currentGame = this;
					p.resetSyncState();
					p.ownedByCurrentPlayer = !p.ownedByCurrentPlayer;
					ListIterator<Plane> i = getCurrentPlanes().listIterator();
					while (i.hasNext()) {
						Plane p2 = i.next();
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
						getCurrentPlanes().add(p);
						System.out.println("received new plane");
						
					}
				}
			}
			super.update(gameContainer, game);
			ListIterator<Plane> i = getCurrentPlanes().listIterator();
			while (i.hasNext()) {
				Plane plane = i.next();
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

}
