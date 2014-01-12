package logicClasses;

import org.newdawn.slick.*;
import org.newdawn.slick.state.*;

import states.DifficultyState;
import states.GameOverState;
import states.MenuState;
import states.PauseState;
import states.PlayState;

public class Game extends StateBasedGame {

	public static final String NAME = "Turbulence";
	public static final int MENUSTATE = 0;
	public static final int DIFFICULTYSTATE = 1;
	public static final int PLAYSTATE = 2;
	public static final int GAMEOVERSTATE = 3;
	public static final int PAUSESTATE = 4;
	public static final int MAXIMUMWIDTH = 1200;
	public static final int MAXIMUMHEIGHT = 600;
	

	public Game(String NAME) {
		super(NAME);
		this.addState(new MenuState(MENUSTATE));
		this.addState(new DifficultyState(DIFFICULTYSTATE));
		this.addState(new PlayState(PLAYSTATE));
		this.addState(new GameOverState(GAMEOVERSTATE));
		this.addState(new PauseState(PAUSESTATE));
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
			
			appgc.setIcon("res/icon.png");
			appgc.start();
		} catch (SlickException e) {
			e.printStackTrace();
		}
	}
}