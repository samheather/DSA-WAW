package game.tests;

import static org.junit.Assert.*;

import java.io.IOException;
import java.net.UnknownHostException;
import java.security.NoSuchAlgorithmException;

import game.gfx.MultiplayerWindow;
import game.struct.Game;
import game.struct.MultiplayerGame;
import game.struct.Plane;

import org.junit.Before;
import org.junit.Test;

public class MultiplayerTests {
	Game game;
	Plane plane1;
	
	@Before
	public void beforeTests() throws NoSuchAlgorithmException, UnknownHostException, IOException{
		game = new MultiplayerGame(50, 100, MultiplayerWindow.getSidebarWidth());
		game.createPlane();
		
	}
	
	@Test
	public void test() {
		System.out.print(game.getCurrentPlanes());
	}

}
