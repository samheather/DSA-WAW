package logicClasses;

import org.newdawn.slick.*;
import org.newdawn.slick.state.*;

import states.GameOverState;
import states.MenuState;
import states.PauseState;
import states.PlayState;
import states.CreditsState;
import states.ControlsState;

public class Game extends StateBasedGame {

	public static final String NAME = "Turbulence";
	public static final int MENUSTATE = 0;
	public static final int PLAYSTATE = 1;
	public static final int GAMEOVERSTATE = 2;
	public static final int PAUSESTATE = 3;
	public static final int CREDITSSTATE = 4;
	public static final int CONTROLSSTATE = 5;
	
	public static final int MAXIMUMWIDTH = 1200;
	public static final int MAXIMUMHEIGHT = 600;
	

	public Game(String NAME) {
		super(NAME);
		this.addState(new MenuState(MENUSTATE));
		this.addState(new PlayState(PLAYSTATE));
		this.addState(new GameOverState(GAMEOVERSTATE));
		this.addState(new PauseState(PAUSESTATE));
		this.addState(new CreditsState(CREDITSSTATE));
		this.addState(new ControlsState(CONTROLSSTATE));
		this.enterState(MENUSTATE);
	}

	public void initStatesList(GameContainer gc) throws SlickException {

		

	}

	public static void main(String[] args) {
		AppGameContainer appgc;
		try {
			appgc = new AppGameContainer(new Game(NAME));
			appgc.setDisplayMode(MAXIMUMWIDTH, MAXIMUMHEIGHT, false);
			appgc.setTargetFrameRate(60);
			
			appgc.setIcon("res/graphics/icon.png");
			appgc.start();
		} catch (SlickException e) {
			e.printStackTrace();
		}
	}
}