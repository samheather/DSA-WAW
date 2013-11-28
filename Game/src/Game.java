import java.util.ArrayList;
import org.newdawn.slick.*;
import org.newdawn.slick.state.*;

public class Game extends StateBasedGame {

	public static final String NAME = "ATC Game - Team WAW";
	public static final int MENU = 0;
	public static final int DIFFICULTY = 1;
	public static final int PLAY = 2;
	public static final int GAMEOVER = 3;
	public static final int PAUSE = 4;
	public static final int MAXIMUM_WIDTH = 1200;
	public static final int MAXIMUM_HEIGHT = 600;

	public Game(String NAME) {
		super(NAME);
		this.addState(new Menu(MENU));
		this.addState(new Difficulty(DIFFICULTY));
		this.addState(new Play(PLAY));
		this.addState(new GameOverState(GAMEOVER));
		this.addState(new Pause(PAUSE));
		this.enterState(MENU);
	}

	public void initStatesList(GameContainer gc) throws SlickException {

		// this.getState(MENU).init(gc, this);
		// this.getState(PLAY).init(gc, this);

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