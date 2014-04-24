package game.gfx;


import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import javax.imageio.ImageIO;

import org.lwjgl.openal.AL;
import org.lwjgl.opengl.Display;
import org.newdawn.slick.AppGameContainer;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.state.StateBasedGame;

/**
 * WindowManager class controls the game GUI
 * <p>
 * WindowManager loads the game states, the game container, and controls how
 * states exit.
 * </p>
 */
public class WindowManager extends StateBasedGame {
	/** The window title */
	public static final String GAME_TITLE = "Plane Chaos";

	/** Standard window width */
	public static final int WINDOW_WIDTH = 1024;

	/** Standard window height */
	public static final int WINDOW_HEIGHT = 700;

	/** Reference to the main menu state */
	public static final int MAIN_MENU_STATE = 1;

	/** Reference to the level select state */
	public static final int LEVEL_SELECT_STATE = 2;

	/** Reference to the game state */
	public static final int GAME_STATE = 3;

	/** Reference to the credits state */
	public static final int CREDITS_STATE = 4;

	/** Reference to the credits state */
	public static final int CONTROLS_STATE = 5;

	/** Reference to the multiplayer selection state */
	public static final int MULTIPLAYER_STATE = 6;
	

	/** Reference to the leaderBoard state */
	
	public static final int LEADERBOARD_STATE = 7;

	/** Reference to the multiplayer game state */
	public static final int MULTIPLAYER_GAME_STATE = 8;


	/** The level currently being played */
	private int currentLevel;

	/** The position of the sky background */
	private double skyProgress;

	/** The position of the title plane */
	private double planeProgress;
	
	/** LeaderBoard */
	
	public static LeaderBoard leaderBoard = new LeaderBoard();

	// Entry point
	public static void main(String[] args) throws UnknownHostException,
			IOException {

		try {
			// Set up app for game
			AppGameContainer appgc = new AppGameContainer(new WindowManager());

			// Suppress output
			appgc.setVerbose(false);

			// Set frame rate to 60
			appgc.setTargetFrameRate(60);

			// Don't show the FPS rate on screen
			appgc.setShowFPS(false);

			// Sets the size of the window
			appgc.setDisplayMode(WINDOW_WIDTH, WINDOW_HEIGHT, false);

			// Start app
			appgc.start();
		} catch (SlickException e) {
			e.printStackTrace();
		}
	}
	// Constructor
	/**
	 * Constructor for WindowManager
	 * <p>
	 * Initialises background progress and title plane progress.
	 * </p>
	 * <p>
	 * Loads states into the game.
	 * </p>
	 * 
	 * @throws IOException
	 * @throws UnknownHostException
	 */
	public WindowManager() throws UnknownHostException, IOException {
		super(GAME_TITLE);

		// Initialise background movement variable
		this.skyProgress = 0;

		// Initialise title plane movement variable
		this.planeProgress = 0;

		// Set application icon
		try {
			ByteBuffer[] icons = new ByteBuffer[1];
			icons[0] = loadIcon("/resources/other/Icon16.png", 16, 16);
			Display.setIcon(icons);
		} catch (IOException e) {
			e.printStackTrace();
		}

		// Add in the screens
		this.addState(new MainMenu());
		this.addState(new LevelSelect());
		this.addState(new GameWindow());
		this.addState(new Credits());
	    this.addState(new LeaderBoard());
		this.addState(new Controls());
		this.addState(new Multiplayer());
		this.addState(new MultiplayerWindow());
	}

	/**
	 * Used to load application icons
	 * <p>
	 * Found at: http://stackoverflow.com/questions/4791654/
	 * taskbar-icon-with-lwjgl
	 * </p>
	 * 
	 * @param filename
	 *            the icon's filename
	 * @param width
	 *            the width of the icon
	 * @param height
	 *            the height of the icon
	 * @return the byte buffer containing the icon
	 */
	private ByteBuffer loadIcon(String filename, int width, int height)
			throws IOException {
		InputStream imageStream = this.getClass().getResourceAsStream(filename);
		BufferedImage image = ImageIO.read(imageStream);

		// Convert image to byte array
		byte[] imageBytes = new byte[width * height * 4];
		for (int i = 0; i < height; i++) {
			for (int j = 0; j < width; j++) {
				int pixel = image.getRGB(j, i);
				for (int k = 0; k < 3; k++) {
					imageBytes[(((i * 16) + j) * 4) + k] = (byte) (((pixel >> (2 - k) * 8)) & 255);
				}

				imageBytes[(((i * 16) + j) * 4) + 3] = (byte) (((pixel >> (3) * 8)) & 255);
			}
		}

		return ByteBuffer.wrap(imageBytes);
	}

	// Overrides
	/**
	 * Run initialisation across <b>all</b> states
	 * <p>
	 * <b>Not currently in use</b>
	 * </p>
	 */
	@Override
	public void initStatesList(GameContainer gameContainer)
			throws SlickException {
	}

	/**
	 * Closes the current window
	 * <p>
	 * If currently on the main menu, this will exit the application.
	 * </p>
	 * <p>
	 * Otherwise, this will navigate back one level.
	 * </p>
	 */
	@Override
	public boolean closeRequested() {
		if (this.getCurrentStateID() == MAIN_MENU_STATE) {
			AL.destroy();
			System.exit(0);
			return true;
		} else if (getCurrentStateID() == LEVEL_SELECT_STATE) {
			AL.destroy();
			System.exit(0);
			return true;
		} else if (getCurrentStateID() == GAME_STATE) {
			enterState(MAIN_MENU_STATE);
		} else if (getCurrentStateID() == CREDITS_STATE) {
			AL.destroy();
			System.exit(0);
			return true;
		} else if (this.getCurrentStateID() == CONTROLS_STATE) {
			AL.destroy();
			System.exit(0);
			return true;
		} else if (this.getCurrentStateID() == LEADERBOARD_STATE){
			AL.destroy();
			System.exit(0);
			return true;
		} else if (this.getCurrentStateID() == MULTIPLAYER_STATE) {
			AL.destroy();
			System.exit(0);
			return true;
		} else if (this.getCurrentStateID() == MULTIPLAYER_GAME_STATE) {
			this.enterState(MAIN_MENU_STATE);
		}

		return false;
	}

	// Accessors
	/**
	 * @return the level currently being played
	 */
	public int getCurrentLevel() {
		return this.currentLevel;
	}

	/**
	 * @return the progress of the background image
	 */
	public double getSkyProgress() {
		return this.skyProgress;
	}

	/**
	 * @return the progress of the title plane
	 */
	public double getPlaneProgress() {
		return this.planeProgress;
	}

	// Mutators
	/**
	 * @param currentLevel
	 *            the level to enter
	 */
	public void setCurrentLevel(int currentLevel) {
		this.currentLevel = currentLevel;
	}

	/**
	 * @param skyProgress
	 *            the updated progress of the background image
	 */
	public void setSkyProgress(double skyProgress) {
		this.skyProgress = skyProgress;
	}

	/**
	 * @param planeProgress
	 *            the updated progress of the title plane
	 */
	public void setPlaneProgress(double planeProgress) {
		this.planeProgress = planeProgress;
	}
}
