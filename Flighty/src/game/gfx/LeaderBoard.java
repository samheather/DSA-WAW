package game.gfx;

import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.state.StateBasedGame;

import game.struct.LeaderBoardEntry;
import game.gfx.WindowManager;
import game.gfx.GameWindow;

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.UnknownHostException;
import java.util.Arrays;

public class LeaderBoard extends GenericWindow {
	/** The image to display behind window content */

	private Image backgroundImage;

	/** The arrow icon */
	private Image arrowIcon;

	/** The shaded arrow icon */
	private Image arrowIconShaded;

	/** variable showing Internet's connection */
	public static boolean connected = false;

	private boolean init = true;
	/**
	 * Array containing the LeaderboardEntries
	 */
	public LeaderBoardEntry[] leaderboardEntries = new LeaderBoardEntry[5];

	/**
	 * Path to the file in which leaderboardScores are stored.
	 */

	public LeaderBoard() {
		// Initialise
		for (int i = 0; i < leaderboardEntries.length; i++) {
			leaderboardEntries[i] = new LeaderBoardEntry();
		}

		for (int i = 0; i < leaderboardEntries.length; i++) {
			leaderboardEntries[i].setName("PLAYER");
			leaderboardEntries[i].setScore(0);
		}
		sortLeaderboard(leaderboardEntries);
	}

	/**
	 * Add entry to the leaderboard - creates an instance of LeaderboardEntry
	 * and adds it to the List.
	 * 
	 * @param String
	 *            name, double score.
	 */
	public void addLeaderboardEntry(String name, double score) {
		LeaderBoardEntry[] tempLeaderboardEntries = new LeaderBoardEntry[6];
		System.arraycopy(leaderboardEntries, 0, tempLeaderboardEntries, 0,
				leaderboardEntries.length);
		tempLeaderboardEntries[5] = new LeaderBoardEntry(name, score);
		sortLeaderboard(tempLeaderboardEntries);
		System.arraycopy(tempLeaderboardEntries, 0, leaderboardEntries, 0,
				leaderboardEntries.length);
	}

	/**
	 * Sorts the List of LeaderboardEntries according to their CompareTo method.
	 * 
	 * @param leaderboardToSort
	 */
	private void sortLeaderboard(LeaderBoardEntry[] leaderboardToSort) {
		Arrays.sort(leaderboardToSort);
	}

	/**
	 * Monitors the mouse position and state
	 * <p>
	 * This causes options to be highlighted etc. when hovered over.
	 * </p>
	 * <p>
	 * Also handles item selection (through clicking)
	 * </p>
	 * 
	 * @param gameContainer
	 *            the game container holding this state
	 * @param game
	 *            the game running this state
	 */
	private void checkForSelection(GameContainer gameContainer,
			StateBasedGame game) {
		int x = gameContainer.getInput().getMouseX();
		int y = gameContainer.getInput().getMouseY();
		boolean clicked = gameContainer.getInput().isMousePressed(0);

		String mainMenuText = "Main Menu";

		int mainMenuWidth = this.font.getWidth(mainMenuText);
		int textHeight = this.font.getHeight();

		Color mainMenuColor = Color.orange;

		// If text is hovered
		if ((x >= (50 - 25)) && (y >= (gameContainer.getHeight() - 50 - 25))
				&& (x <= (50 - 25) + mainMenuWidth + 25)
				&& (y <= (gameContainer.getHeight() - 50 + textHeight + 25))) {
			if (clicked) {
				init = true;
				WindowManager.leaderBoard.clearLeaderboard();
				game.enterState(WindowManager.MAIN_MENU_STATE);
			} else {
				// Change colour on hover
				mainMenuColor = Color.white;
			}
		} else {
			mainMenuColor = Color.orange;
		}

		// Draw the actual text
		this.drawShadowedText(this.font, 50, gameContainer.getHeight() - 50,
				mainMenuText, mainMenuColor);
	}

	/**
	 * Initialises the state
	 * 
	 * @param gameContainer
	 *            the game container holding this state
	 * @param game
	 *            the game running this state
	 */
	@Override
	public void init(GameContainer gameContainer, StateBasedGame game)
			throws SlickException {
		super.init(gameContainer, game);

		// Load images
		InputStream backgroundStream = this.getClass().getResourceAsStream(
				"/resources/backgrounds/CloudBackground.png");
		InputStream arrowStream = this.getClass().getResourceAsStream(
				"/resources/other/ArrowR.png");
		InputStream arrowShadedStream = this.getClass().getResourceAsStream(
				"resources/other/ArrowB.png");

		this.backgroundImage = new Image(backgroundStream, "Background Image",
				false);
		this.arrowIcon = new Image(arrowStream, "Arrow Image", false);
		this.arrowIconShaded = new Image(arrowShadedStream,
				"Arrow Shaded Image", false);

	}

	/**
	 * Renders the state
	 * 
	 * @param gameContainer
	 *            the game container holding this state
	 * @param game
	 *            the game running this state
	 * @param graphics
	 *            the graphics container to display content in
	 */
	@Override
	public void render(GameContainer gameContainer, StateBasedGame game,
			Graphics graphics) throws SlickException {
		super.render(gameContainer, game, graphics);

		int textHeight = this.font.getHeight();

		// Draw the Background Image
		this.backgroundImage.draw(
				(float) (0 - ((WindowManager) game).getSkyProgress()), 0,
				gameContainer.getWidth() * 2, gameContainer.getHeight());

		// Draw main title
		int mainXPos = (gameContainer.getWidth() / 2)
				- (this.titleFont.getWidth("LeaderBoard") / 2);

		this.drawShadowedText(this.titleFont, mainXPos, 20, "LeaderBoard",
				Color.orange);

		// Draw leaderboard entries

		if (init) {
			// checks if there is an internet connection
			this.isConnected();
			// Gets and updates scores form online leaderboard
			if (connected) {
				GameWindow.saveFile.decodeLeaderboardScores(GameWindow.saveFile
						.getLeaderboardScores());
				sortLeaderboard(leaderboardEntries);
			}
			init = false;
		}
		if (connected) {
			int score;
			String name;
			this.drawShadowedText(this.font,
					(gameContainer.getWidth() / 2) - 275, 140, "NAME",
					Color.orange);
			this.drawShadowedText(this.font,
					(gameContainer.getWidth() / 2) + 275, 140, "SCORE",
					Color.orange);
			for (int i = 0; i < WindowManager.leaderBoard.leaderboardEntries.length; i++) {
				name = WindowManager.leaderBoard.leaderboardEntries[i]
						.getName();
				score = (int) WindowManager.leaderBoard.leaderboardEntries[i]
						.getScore();
				this.drawShadowedText(this.font,
						(gameContainer.getWidth() / 2) - 275, 240 + (textHeight
								* i * 3), name, Color.orange);
				this.drawShadowedText(this.font,
						(gameContainer.getWidth() / 2) + 275, 240 + (textHeight
								* i * 3), "" + score, Color.orange);
			}
		} else {
			this.drawShadowedText(this.titleFont, 125, 350,
					"No internet connection", Color.orange);

		}

		// Draw back text
		this.checkForSelection(gameContainer, game);

		// Draw shaded arrow icon
		this.arrowIconShaded.draw(247, gameContainer.getHeight() - 48
				- (textHeight / 4), 45, 35);

		// Draw arrow icon
		this.arrowIcon.draw(245, gameContainer.getHeight() - 50
				- (textHeight / 4), 45, 35);

	}

	// Checks if internet connection is available
	public void isConnected() {
		try {
			URL url = new URL("http://www.google.com");
			HttpURLConnection conn = (HttpURLConnection) url.openConnection();
			conn.setConnectTimeout(400);
			conn.getContent();
		} catch (UnknownHostException e) {
			e.printStackTrace();
			connected = false;
			return;
		} catch (IOException e) {
			e.printStackTrace();
			connected = false;
			return;
		}
		connected = true;

	}

	// Clears leaderboard so that same scores wouldn't overwrite the ones
	// already in leaderboard.
	public void clearLeaderboard() {
		for (int i = 0; i < 4; i++) {
			WindowManager.leaderBoard.leaderboardEntries[i].setScore(0);
			WindowManager.leaderBoard.leaderboardEntries[i].setName("PLAYER");
		}
	}

	/**
	 * @return the state's unique ID
	 */
	@Override
	public int getID() {
		return WindowManager.LEADERBOARD_STATE;
	}

	// All general Accessors
	/**
	 * @return the background image
	 */
	public Image getBackgroundImage() {
		return this.backgroundImage;
	}

	/**
	 * @return the arrow icon
	 */
	public Image getArrowIcon() {
		return this.arrowIcon;
	}

	/**
	 * @return the shaded arrow icon
	 */
	public Image getArrowIconShaded() {
		return this.arrowIconShaded;
	}

	// All general Mutators
	/**
	 * @param backgroundImage
	 *            the new background image
	 */
	public void setBackgroundImage(Image backgroundImage) {
		this.backgroundImage = backgroundImage;
	}

	/**
	 * @param arrowIcon
	 *            the new arrow icon
	 */
	public void setArrowIcon(Image arrowIcon) {
		this.arrowIcon = arrowIcon;
	}

	/**
	 * @param shadedArrowIcon
	 *            the new shaded arrow icon
	 */
	public void setArrowIconShaded(Image shadedArrowIcon) {
		this.arrowIconShaded = arrowIcon;
	}
}