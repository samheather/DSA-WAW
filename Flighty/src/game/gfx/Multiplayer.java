package game.gfx;

import java.io.InputStream;

import org.newdawn.slick.AppGameContainer;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.state.StateBasedGame;

/**
 * LevelSelect class provides a level selection menu
 */
public class Multiplayer extends GenericWindow {

	/** The image to display behind window content */
	private Image backgroundImage;

	/** The arrow icon */
	private Image arrowIcon;

	/** The shaded arrow icon */
	private Image arrowIconShaded;

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
		String titleText = "Multiplayer";
		String mainMenuText = "Main Menu";

		// Get the text width
		int titleTextWidth = this.font.getWidth(titleText);
		int mainMenuWidth = this.font.getWidth(mainMenuText);

		// Get the text height
		int fontHeight = this.font.getHeight();

		// Set the colours for the text
		Color titleColor = Color.orange;
		Color mainMenuColor = Color.orange;

		// position of images on the screen in pixels
		int[] titlePos = { (gameContainer.getWidth() / 2) - titleTextWidth, 50 };
		int[] mainMenuTextPos = { 50, gameContainer.getHeight() - 50 };

		// tolerance for clicking on image in pixels
		int tolerance = 3;

		// Draw main title
		this.drawShadowedText(this.titleFont, titlePos[0], titlePos[1],
				titleText, titleColor);

		// Main Menu
		if ((x >= (mainMenuTextPos[0] - tolerance))
				&& (x <= (mainMenuTextPos[0] + mainMenuWidth + tolerance))
				&& (y >= (mainMenuTextPos[1] - tolerance))
				&& (y <= (mainMenuTextPos[1] + fontHeight + tolerance))) {
			if (clicked) {
				// Change state to main menu if button clicked
				game.enterState(WindowManager.MAIN_MENU_STATE);
			} else {
				// Change appearance on hover
				mainMenuColor = Color.white;
			}
		} else {
			// Colour when not hovered
			mainMenuColor = Color.orange;
		}

		// Draw the text
		this.drawShadowedText(this.font, mainMenuTextPos[0],
				mainMenuTextPos[1], mainMenuText, mainMenuColor);

		// Draw the arrow icons next to main menu text
		this.arrowIconShaded.draw(mainMenuTextPos[0] + mainMenuWidth + 20,
				mainMenuTextPos[1]);
		this.arrowIcon.draw(mainMenuTextPos[0] + mainMenuWidth + 18,
				mainMenuTextPos[1] - 2);
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

		InputStream arrowStream = this.getClass().getResourceAsStream(
				"/resources/other/ArrowR.png");
		InputStream arrowShadedStream = this.getClass().getResourceAsStream(
				"/resources/other/ArrowB.png");

		this.backgroundImage = new Image(backgroundStream, "Background Image",
				false);

		this.arrowIcon = new Image(arrowStream, "Arrow Image", false);
		this.arrowIconShaded = new Image(arrowShadedStream,
				"Arrow Shaded Image", false);
	}

	/**
	 * Sets properties which need resetting each time the state is accessed
	 * 
	 * @param gameContainer
	 *            The game container holding this state
	 * @param game
	 *            The game running this state
	 */
	@Override
	public void enter(GameContainer gameContainer, StateBasedGame game)
			throws SlickException {
		((AppGameContainer) gameContainer).setDisplayMode(
				WindowManager.WINDOW_WIDTH, 512, false);
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

		graphics.setColor(Color.black);

		// Draw other text and images
		this.drawButtons(gameContainer, game);
	}

	/**
	 * @return the state's unique ID
	 */
	@Override
	public int getID() {
		return WindowManager.MULTIPLAYER_STATE;
	}

	// Accessors
	/**
	 * @return The background image
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

	// Mutators
	/**
	 * @param backgroundImage
	 *            the new background image
	 */
	public void setBackgroundImage(Image backgroundImage) {
		this.backgroundImage = backgroundImage;
	}

	/**
	 * @param arrowIcon
	 *            the arrow icon
	 */
	public void setArrowIcon(Image arrowIcon) {
		this.arrowIcon = arrowIcon;
	}

	/**
	 * @param arrowIconShaded
	 *            the shaded arrow icon
	 */
	public void setArrowIconShaded(Image arrowIconShaded) {
		this.arrowIconShaded = arrowIconShaded;
	}
}
