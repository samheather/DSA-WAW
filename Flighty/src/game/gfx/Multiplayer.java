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
	
	/** Offline image */
	private Image offlineImage;

	/** The active image (map preview) for level 3 */
	private Image offlineImageHover;
	
	/** Absolute position holders for images **/
	private int[] offlinePos;

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
		String offlineText = "Offline";
		String practiceText = "Practice multiplayer";

		// Get the text width
		int titleTextWidth = this.font.getWidth(titleText);
		int mainMenuWidth = this.font.getWidth(mainMenuText);
		int offlineWidth = this.font.getWidth(offlineText);
		int practiceWidth = this.font.getWidth(practiceText);

		// Get the text height
		int fontHeight = this.font.getHeight();

		// Set the colours for the text
		Color titleColor = Color.orange;
		Color mainMenuColor = Color.orange;
		Color offlineColor = Color.orange;
		Color practiceColor = Color.orange;
		
		// size of images on screen
		int[] imageSize = { 200, 200 };

		// position of images on the screen in pixels
		int[] titlePos = { (gameContainer.getWidth() / 2) - titleTextWidth, 50 };
		int[] mainMenuTextPos = { 50, gameContainer.getHeight() - 50 };
		int[] offlineImagePos = { 110, 175 };
		
		// duplicating the values for the black squares to outline the images
		offlinePos = offlineImagePos;
		
		// Set the images for the modes
		Image offlineImageCurrent = this.offlineImage;

		// tolerance for clicking on image in pixels
		int tolerance = 3;

		// Draw main title
		this.drawShadowedText(this.titleFont, titlePos[0], titlePos[1],
				titleText, titleColor);
		
		// Offline Text
				this.drawShadowedText(this.font, offlineImagePos[0]
						+ (imageSize[0] / 2) - (practiceWidth / 2),
						offlineImagePos[1] - 50, practiceText, practiceColor);

				// Offline difficulty text
				this.drawShadowedText(this.font, offlineImagePos[0]
						+ (imageSize[0] / 2) - (offlineWidth / 2),
						offlineImagePos[1] + 230, offlineText, offlineColor);

				// Offline Image
				if ((x >= (offlineImagePos[0] - tolerance))
						&& (x <= (offlineImagePos[0] + imageSize[0] + tolerance))
						&& (y >= (offlineImagePos[1] - tolerance))
						&& (y <= (offlineImagePos[1] + imageSize[1] + tolerance))) {
					// Change state to game state if the level is picked
					if (clicked) {
						((WindowManager) game).setCurrentLevel(1);
						/*TODO
						 * Change this to multiplayer state
						 */
						game.enterState(WindowManager.MULTIPLAYER_GAME_STATE);
					} else {
						// Apply hover image if cursor is hovering
						offlineImageCurrent = this.offlineImageHover;
					}
				} else {
					// Draw the level1 image
					offlineImageCurrent = this.offlineImage;
				}

				offlineImageCurrent.draw(offlineImagePos[0], offlineImagePos[1], 200, 200);
				
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
		
		InputStream offlineStream = this.getClass().getResourceAsStream(
				"/resources/maps/Map1Small.png");
		
		InputStream offlineHoverStream = this.getClass().getResourceAsStream(
				"/resources/maps/Map1SmallHover.png");

		this.backgroundImage = new Image(backgroundStream, "Background Image",
				false);

		this.arrowIcon = new Image(arrowStream, "Arrow Image", false);
		this.arrowIconShaded = new Image(arrowShadedStream,
				"Arrow Shaded Image", false);
		
		this.offlineImage = new Image(offlineStream, "Offline", false);
		this.offlineImageHover = new Image(offlineHoverStream,
				"Offline Hover", false).getScaledCopy(5, 3);
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
		
		if (this.offlinePos != null){
			graphics.fillRect(offlinePos[0] - 4, offlinePos[1] - 4, this
					.getOfflineImage().getWidth() + 8, this.getOfflineImage()
					.getHeight() + 8);
		}
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
	
	/**
	 * @return the shaded arrow icon
	 */
	public Image getOfflineImage() {
		return this.offlineImage;
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
