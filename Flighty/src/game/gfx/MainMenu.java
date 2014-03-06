package game.gfx;

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
		String controlsText = "Controls";
		String exitText = "Exit";

		// Get the width of the buttons
		int gameTitleWidth = this.font.getWidth(gameTitleText);
		int startGameWidth = this.font.getWidth(startGameText);
		int multiplayerWidth = this.font.getWidth(multiplayerText);
		int creditsWidth = this.font.getWidth(creditsText);
		int controlsWidth = this.font.getWidth(controlsText);
		int exitWidth = this.font.getWidth(exitText);

		// Get the height of the text
		int fontHeight = this.font.getHeight();

		// Set the colour for text once clicked
		Color gameTitleColor = Color.orange;
		Color startGameColor = Color.orange;
		Color multiplayerColor = Color.orange;
		Color creditsColor = Color.orange;
		Color controlsColor = Color.orange;
		Color exitColor = Color.orange;

		// Button positions from top left as absolute values
		int[] gameTitlePos = { (gameContainer.getWidth()) / 2 - gameTitleWidth,
				100 };
		int[] startGamePos = { (gameContainer.getWidth() - startGameWidth) / 2,
				250 };
		int[] multiplayerPos = {
				(gameContainer.getWidth() - multiplayerWidth) / 2, 350 };
		int[] creditsPos = { (gameContainer.getWidth() - creditsWidth) / 2, 450 };
		int[] controlsPos = { (gameContainer.getWidth() - controlsWidth) / 2,
				550 };
		int[] exitPos = { 50, gameContainer.getHeight() - 50 };

		// Hovering box tolerance in pixels
		int tolerance = 10;
		
		//Distance that waypoint is away from text
		int waypointDistFromText = 10;

		// Draw main title
		this.drawShadowedText(this.titleFont, gameTitlePos[0], gameTitlePos[1],
				gameTitleText, gameTitleColor);

		// Start Game button
		if ((x >= (startGamePos[0] - tolerance))
				&& (x <= (startGamePos[0] + startGameWidth + tolerance))
				&& (y >= (startGamePos[1] - tolerance))
				&& (y <= (startGamePos[1] + fontHeight + tolerance))) {
			if (clicked) {
				// Change game state
				game.enterState(WindowManager.LEVEL_SELECT_STATE);
			} else {
				// Change hover text and add waypoint next to text
				startGameColor = Color.white;
				this.waypointIcon
						.draw(startGamePos[0] + startGameWidth + waypointDistFromText,
								startGamePos[1]);
			}
		} else { // Default colour
			startGameColor = Color.orange;
		}

		// Draw the Start Game text with a shadow
		this.drawShadowedText(this.font, startGamePos[0], startGamePos[1],
				startGameText, startGameColor);

		if ((x >= (multiplayerPos[0] - tolerance))
				&& (x <= (multiplayerPos[0] + multiplayerWidth + tolerance))
				&& (y >= (multiplayerPos[1] - tolerance))
				&& (y <= (multiplayerPos[1] + fontHeight + tolerance))) {
			if (clicked) {
				// Change game state
				game.enterState(WindowManager.MULTIPLAYER_STATE);
			} else {
				// Change hover text and add waypoint next to text
				multiplayerColor = Color.white;
				this.waypointIcon
				.draw(multiplayerPos[0] + multiplayerWidth + waypointDistFromText,
						multiplayerPos[1]);
			}
		} else { // Default colour
			multiplayerColor = Color.orange;
		}

		// Draw the Controls text with a shadow
		this.drawShadowedText(this.font, multiplayerPos[0], multiplayerPos[1],
				multiplayerText, multiplayerColor);

		// Credits button \\
		if ((x >= (creditsPos[0] - tolerance))
				&& (x <= (creditsPos[0] + creditsWidth + tolerance))
				&& (y >= (creditsPos[1] - tolerance))
				&& (y <= (creditsPos[1] + fontHeight + tolerance))) {
			if (clicked) {
				// Change game state
				game.enterState(WindowManager.CREDITS_STATE);
			} else {
				creditsColor = Color.white;

				// Change hover text and add waypoint next to text
				this.waypointIcon
				.draw(creditsPos[0] + creditsWidth + waypointDistFromText,
						creditsPos[1]);
			}
		} else { // Default colour
			creditsColor = Color.orange;
		}

		this.drawShadowedText(this.font, creditsPos[0], creditsPos[1],
				creditsText, creditsColor);

		// Controls button \\
		if ((x >= (controlsPos[0] - tolerance))
				&& (x <= (controlsPos[0] + controlsWidth + tolerance))
				&& (y >= (controlsPos[1] - tolerance))
				&& (y <= (controlsPos[1] + fontHeight + tolerance))) {
			if (clicked) {
				// Change game state
				game.enterState(WindowManager.CONTROLS_STATE);
			} else {
				// Change hover text and add waypoint next to text
				controlsColor = Color.white;
				this.waypointIcon
				.draw(controlsPos[0] + controlsWidth + waypointDistFromText,
						controlsPos[1]);
			}
		} else { // Default colour
			controlsColor = Color.orange;
		}

		// Draw the Controls text with a shadow
		this.drawShadowedText(this.font, controlsPos[0], controlsPos[1],
				controlsText, controlsColor);

		// Exit
		if ((x >= (exitPos[0] - tolerance))
				&& (x <= (exitPos[0] + exitWidth + tolerance))
				&& (y >= (exitPos[1] - tolerance))
				&& (y <= (exitPos[1] + fontHeight + tolerance))) {
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

		// Draw the Exit text with a shadow
		this.drawShadowedText(this.font, exitPos[0], exitPos[1], exitText,
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
		this.backgroundImage.draw(
				(float) (0 - ((WindowManager) game).getSkyProgress()), 0,
				gameContainer.getWidth() * 2, gameContainer.getHeight());

		// Draw buttons
		this.drawButtons(gameContainer, game);

		// Draw plane icon
		this.planeIcon.draw(
				(float) (((WindowManager) game).getPlaneProgress() - 50),
				50 + this.font.getHeight() + 105, 50, 50);
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
