package game.tests;

import static org.junit.Assert.*;

import java.io.IOException;
import java.net.UnknownHostException;
import java.security.NoSuchAlgorithmException;
import java.util.Random;

import game.gfx.MultiplayerWindow;
import game.struct.Game;
import game.struct.MultiplayerGame;
import game.struct.Plane;

import org.junit.Before;
import org.junit.Test;

public class MultiplayerTests {
	Game game;
	Plane plane1;
	Random rand;
	
	@Before
	public void beforeTests() throws NoSuchAlgorithmException, UnknownHostException, IOException{
		game = new MultiplayerGame(50, 100, MultiplayerWindow.getSidebarWidth());
		game.createPlane();
		game.getCurrentPlanes().get(0).setID(100);
		plane1 = game.getPlaneFromID(100);
		
	}
	
	@Test
	public void switchTest() {
		plane1.setOwnedByCurrentPlayer(true);
		assertTrue(plane1.getOwnedByCurrentPlayer());
		plane1.setX(800);
		assertFalse(plane1.getOwnedByCurrentPlayer());
		
	}

}
