package game.tests;

import static org.junit.Assert.*;

import java.io.IOException;
import java.net.UnknownHostException;
import java.security.NoSuchAlgorithmException;
import java.util.Random;

import game.gfx.MultiplayerWindow;
import game.gfx.WindowManager;
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
		assertFalse(plane1.ownedByCurrentPlayer);
		plane1.setX(1000);
		System.out.println(game.getCurrentPlanes());
		assertFalse(plane1.ownedByCurrentPlayer);
		
	}
	
	@Test
	public void changeCredits() {
		assertEquals(0, game.getScore().getCredits());
		game.getScore().addScore(plane1, game);
		assertEquals(5, game.getScore().getCredits());
	}
	
	@Test
	public void switchScoreTest(){
		plane1.setX(300);
		game.getScore().setScore(20);
		plane1.setX(1000);
		game.getScore().planeLeftAirspaceOrWaitingToTakeOffMinusScore();
		assertEquals(10, game.getScore().getScore());
	}

}
