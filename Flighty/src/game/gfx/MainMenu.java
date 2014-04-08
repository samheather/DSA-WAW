package game.gfx;

import org.lwjgl.input.Keyboard;
import org.lwjgl.openal.AL;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.state.StateBasedGame;

import java.io.InputStream;

/**
 * MainMenu class provides a main (options) menu
 */
public class MainMenu extends GenericWindow {

	/** The image to display behind window content */
	private Image backgroundImage;

	/** The plane icon */
	private Image planeIcon;

	/** The waypoint icon */
	private Image waypointIcon;

	
	private boolean isInHitBox(int mouseX, int mouseY, int[] pos, int width, int height, int tolerance) {
		return ((mouseX >= (pos[0] - tolerance))
				&& (mouseX <= (pos[0] + width + tolerance))
				&& (mouseY >= (pos[1] - tolerance))
				&& (mouseY <= (pos[1] + height + tolerance)));
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
	private void drawButtons(GameContainer gameContainer, StateBasedGame game) {
		// Mouse coordinates
		int x = gameContainer.getInput().getMouseX();
		int y = gameContainer.getInput().getMouseY();

		// Mouse action
		boolean clicked = gameContainer.getInput().isMousePressed(0);

		// Text
		String gameTitleText = "Plane Chaos";
		String startGameText = "Single Player";
		String multiplayerText = "Multiplayer";
		String creditsText = "Credits";
		String leaderBoardText = "LeaderBoard";
		String controlsText = "Controls";
		String exitText = "Exit";

		// Get the width of the buttons
		int gameTitleWidth = this.font.getWidth(gameTitleText);
		int startGameWidth = this.font.getWidth(startGameText);
		int multiplayerWidth = this.font.getWidth(multiplayerText);
		int creditsWidth = this.font.getWidth(creditsText);
		int leaderBoardWidth = this.font.getWidth(leaderBoardText);
		int controlsWidth = this.font.getWidth(controlsText);
		int exitWidth = this.font.getWidth(exitText);

		// Get the height of the text
		int fontHeight = font.getHeight();

		// Set the colour for text once clicked
		Color gameTitleColor = Color.orange;
		Color startGameColor = Color.orange;
		Color multiplayerColor = Color.orange;
		Color creditsColor = Color.orange;
		Color leaderBoardColor = Color.orange;
		Color controlsColor = Color.orange;
		Color exitColor = Color.orange;

		// Button positions from top left as absolute values
		int[] gameTitlePos = { (gameContainer.getWidth()) / 2 - gameTitleWidth,
				75 };
		int[] startGamePos = { (gameContainer.getWidth() - startGameWidth) / 2,
				200 };
		int[] multiplayerPos = {
				(gameContainer.getWidth() - multiplayerWidth) / 2, 300 };
		int[] leaderBoardPos = {
				(gameContainer.getWidth() - leaderBoardWidth) / 2, 400 };
	
		int[] creditsPos = { (gameContainer.getWidth() - creditsWidth) / 2, 500 };
		int[] controlsPos = { (gameContainer.getWidth() - controlsWidth) / 2,
				600 };
		int[] exitPos = { 50, gameContainer.getHeight() - 50 };

		// Hovering box tolerance in pixels
		int tolerance = 10;
		
		//Distance that waypoint is away from text
		int waypointDistFromText = 10;

		// Draw main title
		this.drawShadowedText(titleFont, gameTitlePos[0], gameTitlePos[1],
				gameTitleText, gameTitleColor);

		// Start Game button
		if (isInHitBox(x, y, startGamePos, startGameWidth, fontHeight, tolerance)) {
			if (clicked) {
				// Change game state
				game.enterState(WindowManager.LEVEL_SELECT_STATE);
			} else {
				// Change hover text and add waypoint next to text
				startGameColor = Color.white;
				waypointIcon
						.draw(startGamePos[0] + startGameWidth + waypointDistFromText,
								startGamePos[1]);
			}
		} else { // Default colour
			startGameColor = Color.orange;
		}

		// Draw the Start Game text with a shadow
		this.drawShadowedText(font, startGamePos[0], startGamePos[1],
				startGameText, startGameColor);

		if (isInHitBox(x, y, multiplayerPos, multiplayerWidth, fontHeight, tolerance)) {
			if (clicked) {
				// Change game state
				game.enterState(WindowManager.MULTIPLAYER_STATE);
			} else {
				// Change hover text and add waypoint next to text
				multiplayerColor = Color.white;
				waypointIcon
				.draw(multiplayerPos[0] + multiplayerWidth + waypointDistFromText,
						multiplayerPos[1]);
			}
		} else { // Default colour
			multiplayerColor = Color.orange;
		}

		// Draw the Controls text with a shadow
		this.drawShadowedText(font, multiplayerPos[0], multiplayerPos[1],
				multiplayerText, multiplayerColor);
		
		//Leaderboard button
		
				if (isInHitBox(x, y, leaderBoardPos, leaderBoardWidth, fontHeight, tolerance)) {
					if (clicked) {
						// Change game state
						game.enterState(WindowManager.LEADERBOARD_STATE);
					} else {
						leaderBoardColor = Color.white;

						// Change hover text and add waypoint next to text
						this.waypointIcon.draw(leaderBoardPos[0] + leaderBoardWidth,
								leaderBoardPos[1]);
					}
				} else { // Default colour
					leaderBoardColor = Color.orange;
				}

				this.drawShadowedText(this.font, leaderBoardPos[0], leaderBoardPos[1],
						leaderBoardText, leaderBoardColor);

		// Credits button \\
		if (isInHitBox(x, y, creditsPos, creditsWidth, fontHeight, tolerance)) {
			if (clicked) {
				// Change game state
				game.enterState(WindowManager.CREDITS_STATE);
			} else {
				creditsColor = Color.white;

				// Change hover text and add waypoint next to text
				waypointIcon
				.draw(creditsPos[0] + creditsWidth + waypointDistFromText,
						creditsPos[1]);
			}
		} else { // Default colour
			creditsColor = Color.orange;
		}

		drawShadowedText(font, creditsPos[0], creditsPos[1],
				creditsText, creditsColor);

		// Controls button \\
		if (isInHitBox(x, y, controlsPos, controlsWidth, fontHeight, tolerance)) {
			if (clicked) {
				// Change game state
				game.enterState(WindowManager.CONTROLS_STATE);
			} else {
				// Change hover text and add waypoint next to text
				controlsColor = Color.white;
				waypointIcon
				.draw(controlsPos[0] + controlsWidth + waypointDistFromText,
						controlsPos[1]);
			}
		} else { // Default colour
			controlsColor = Color.orange;
		}

		// Draw the Controls text with a shadow
		drawShadowedText(font, controlsPos[0], controlsPos[1],
				controlsText, controlsColor);

		// Exit
		if (isInHitBox(x, y, exitPos, exitWidth, fontHeight, tolerance)) {
			if (clicked) {
				AL.destroy();
				System.exit(0);
			} else { // Colour on hover
				exitColor = Color.white;
			}
		} else {
			// Default colour
			exitColor = Color.orange;
		}
		if (Keyboard.isKeyDown(Keyboard.KEY_ESCAPE)){
			AL.destroy();
			System.exit(0);
		}

		// Draw the Exit text with a shadow
		drawShadowedText(font, exitPos[0], exitPos[1], exitText,
				exitColor);
	}

	// Overrides
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

		InputStream backgroundStream = this.getClass().getResourceAsStream(
				"/resources/backgrounds/CloudBackground.png");
		InputStream planeStream = this.getClass().getResourceAsStream(
				"/resources/planes/TitlePlane.png");
		InputStream waypointStream = this.getClass().getResourceAsStream(
				"/resources/waypoints/WaypointRed.png");

		this.backgroundImage = new Image(backgroundStream, "Background Image",
				false);
		this.planeIcon = new Image(planeStream, "Plane Icon", false);
		this.waypointIcon = new Image(waypointStream, "Waypoint Icon", false);
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

		// Draw background
		backgroundImage.draw(
				(float) (0 - ((WindowManager) game).getSkyProgress()), 0,
				gameContainer.getWidth() * 2, gameContainer.getHeight());

		// Draw buttons
		drawButtons(gameContainer, game);

		// Draw plane icon
		planeIcon.draw(
				(float) (((WindowManager) game).getPlaneProgress() - 50),
				50 + font.getHeight() + 105, 50, 50);
	}

	// Accessors
	/**
	 * @return the background image
	 */
	public Image getBackgroundImage() {
		return this.backgroundImage;
	}

	/**
	 * @return the plane icon
	 */
	public Image getPlaneIcon() {
		return this.planeIcon;
	}

	/**
	 * @return the waypoint icon
	 */
	public Image getWaypointIcon() {
		return this.waypointIcon;
	}

	// Mutators
	/**
	 * @param backgroundImage
	 *            the new background image
	 */
	public void setBackgroundImage(Image backgroundImage) {
		this.backgroundImage = backgroundImage;
	}

	/**
	 * @param planeIcon
	 *            the new plane icon
	 */
	public void setPlaneIcon(Image planeIcon) {
		this.planeIcon = planeIcon;
	}

	/**
	 * @param waypointIcon
	 *            the new waypoint icon
	 */
	public void setWaypointIcon(Image waypointIcon) {
		this.waypointIcon = waypointIcon;
	}

	/**
	 * @return the state's unique ID
	 */
	@Override
	public int getID() {
		return WindowManager.MAIN_MENU_STATE;
	}
}
