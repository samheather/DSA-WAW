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
	public static final int MENU_STATE = 0;
	public static final int DIFFICULTY_STATE = 1;
	public static final int PLAY_STATE = 2;
	public static final int GAMEOVER_STATE = 3;
	public static final int PAUSE_STATE = 4;
	public static final int MAXIMUM_WIDTH = 1200;
	public static final int MAXIMUM_HEIGHT = 600;
	

	public Game(String NAME) {
		super(NAME);
		this.addState(new MenuState(MENU_STATE));
		this.addState(new DifficultyState(DIFFICULTY_STATE));
		this.addState(new PlayState(PLAY_STATE));
		this.addState(new GameOverState(GAMEOVER_STATE));
		this.addState(new PauseState(PAUSE_STATE));
		this.enterState(MENU_STATE);
	}

	public void initStatesList(GameContainer gc) throws SlickException {

		

	}

	public static void main(String[] args) {
		AppGameContainer appgc;
		try {
			appgc = new AppGameContainer(new Game(NAME));
			appgc.setDisplayMode(MAXIMUM_WIDTH, MAXIMUM_HEIGHT, false);
			appgc.setTargetFrameRate(60);
			
			appgc.setIcon("res/icon.png");
			appgc.start();
		} catch (SlickException e) {
			e.printStackTrace();
		}
	}
}