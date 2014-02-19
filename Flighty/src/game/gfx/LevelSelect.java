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
public class LevelSelect extends GenericWindow {
	
	/** The image to display behind window content */
	private Image backgroundImage;
	
	/** The image (map preview) for level 1 */
	private Image level1Image;
	
	/** The active image (map preview) for level 1 */
	private Image level1ImageHover;
	
	/** The image (map preview) for level 2 */
	private Image level2Image;
	
	/** The active image (map preview) for level 2 */
	private Image level2ImageHover;
	
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
	 * @param gameContainer	the game container holding this state
	 * @param game			the game running this state
	 */
	private void checkForSelection(GameContainer gameContainer,
			StateBasedGame game)
	{
		// Mouse coordinates
		int x 				= gameContainer.getInput().getMouseX();
		int y 				= gameContainer.getInput().getMouseY();
		
		// Mouse action
		boolean clicked 	= gameContainer.getInput().isMousePressed(0);

		// Text
		String level1Text 			= "Level 1";
		String level2Text 			= "Level 2";
		String difficulty1 			= "Easy";
		String difficulty2 			= "Medium";
		String mainMenuText			= "Main Menu";

		// Get the text width
		int level2Width 			= this.font.getWidth(level2Text);
		int mainMenuWidth		 	= this.font.getWidth(mainMenuText);

		// Get the text height
		int textHeight 				= this.font.getHeight();

		// Set the colours for the text
		Color level1Color 			= Color.orange;
		Color level2Color 			= Color.orange;
		
		// Set the images for the levels
		Image level1ImageCurrent	= this.level1Image;
		Color mainMenuColor 		= Color.orange;
		Image level2ImageCurrent 	= this.level1Image;

		// Level 1 Text
		this.drawShadowedText(this.font, 150, 130, level1Text, level1Color);
		
		// Level 1 difficulty text
		this.drawShadowedText(this.font, 160, 400, difficulty1, level1Color);


		// Level 1 Image
		if((x >= (105))
				&& (y >= (200 - 25))
				&& (x <= (90 + 200 + 25))
				&& (y <= (150 + 200 + 25)))
		{
			// Change state to game state if the level is picked
			if(clicked)
			{
				((WindowManager) game).setCurrentLevel(1);
				game.enterState(WindowManager.GAME_STATE);
			}
			else
			{
				// Apply hover image if cursor is hovering
				level1ImageCurrent = this.level1ImageHover;
			}
		}
		else
		{
			// Draw the level1 image
			level1ImageCurrent = this.level1Image;
		}

		level1ImageCurrent.draw(110, 175, 200, 200);
		
		// Level 2 Text
		this.drawShadowedText(this.font, (gameContainer.getWidth() / 2)
				- (level2Width / 2), 130, level2Text, level2Color);
		
		//Level 2 difficulty text
		this.drawShadowedText(this.font, 440, 400, difficulty2, level1Color);

		// Level 2 Image
		if((x >= ((gameContainer.getWidth() / 2)
				- (200 / 2)))
				&& (y >= (200))
				&& (x <= ((gameContainer.getWidth() / 2)
						- (200 / 2) + 200))
				&& (y <= (200 + 200)))
			{
			// Change state to game state if the level is picked
			if(clicked)
			{
				((WindowManager) game).setCurrentLevel(2);
				game.enterState(WindowManager.GAME_STATE);
			}
			else
			{
				// Apply hover image if cursor is hovering
				level2ImageCurrent = this.level1ImageHover;
			}
		}
		else
		{	
			// Draw the level2 image
			level2ImageCurrent = this.level1Image;
		}

		level2ImageCurrent.draw((gameContainer.getWidth() / 2)
				- (200 / 2), 175, 200, 200);

		// Main Menu
		if((x >= (50 - 25))
				&& (y >= (gameContainer.getHeight() - 50 - 25))
				&& (x <= (50 - 25) + mainMenuWidth + 25)
				&& (y <= (gameContainer.getHeight() - 50
						+ textHeight + 25)))
		{
			if(clicked)
			{
				// Change state to main menu if button clicked
				game.enterState(WindowManager.MAIN_MENU_STATE);
			}
			else
			{
				// Change appearance on hover
				mainMenuColor = Color.white;
			}
		}
		else
		{
			// Colour when not hovered
			mainMenuColor = Color.orange;
		}
		
		// Draw the text
		this.drawShadowedText(this.font, 50, gameContainer.getHeight() - 50,
				mainMenuText, mainMenuColor);
	}
	
	
	// Overrides
	/**
	 * Initialises the state
	 * 
	 * @param gameContainer	the game container holding this state
	 * @param game			the game running this state
	 */
	@Override
	public void init(GameContainer gameContainer,
			StateBasedGame game) throws SlickException
	{
		super.init(gameContainer, game);
		
		InputStream backgroundStream = this.getClass()
				.getResourceAsStream(
						"/resources/backgrounds/CloudBackground.png");
		
		InputStream level1Stream = this.getClass()
				.getResourceAsStream("/resources/maps/Map1Small.png");
		InputStream level1HoverStream = this.getClass()
				.getResourceAsStream("/resources/maps/Map1SmallHover.png");
		
		InputStream level2Stream = this.getClass()
				.getResourceAsStream("/resources/maps/Map2Small.png");
		InputStream level2HoverStream = this.getClass()
				.getResourceAsStream("/resources/maps/Map2SmallHover.png");
		
		InputStream arrowStream = this.getClass()
				.getResourceAsStream("/resources/other/ArrowR.png");
		InputStream arrowShadedStream = this.getClass()
				.getResourceAsStream("/resources/other/ArrowB.png");
		
		this.backgroundImage = new Image(backgroundStream,
				"Background Image", false);
		
		this.level1Image = new Image(level1Stream,
				"Level 1 Small", false);
		this.level1ImageHover = new Image(level1HoverStream,
				"Level 1 Small Hover", false).getScaledCopy(5, 3);
		
		this.level2Image = new Image(level2Stream,
				"Level 2 Small", false);
		this.level2ImageHover = new Image(level2HoverStream,
				"Level 2 Small Hover", false).getScaledCopy(5, 3);
		
		this.arrowIcon = new Image(arrowStream,
				"Arrow Image", false);
		this.arrowIconShaded = new Image(arrowShadedStream,
				"Arrow Shaded Image", false);
	}
	
	/**
	 * Sets properties which need resetting each time the state is accessed
	 * 
	 * @param gameContainer	The game container holding this state
	 * @param game			The game running this state
	 */
	@Override
	public void enter(GameContainer gameContainer,
			StateBasedGame game) throws SlickException
	{
		((AppGameContainer) gameContainer).setDisplayMode(
				WindowManager.WINDOW_WIDTH, 512, false);
	}

	/**
	 * Renders the state
	 * 
	 * @param gameContainer	the game container holding this state
	 * @param game			the game running this state
	 * @param graphics		the graphics container to display content in
	 */
	@Override
	public void render(GameContainer gameContainer,
			StateBasedGame game, Graphics graphics) throws SlickException
	{
		super.render(gameContainer, game, graphics);
		
		// Draw background
		this.backgroundImage.draw((float) (0 - ((WindowManager) game)
				.getSkyProgress()), 0, gameContainer.getWidth() * 2,
				gameContainer.getHeight());
		
		// Draw main title
		this.drawShadowedText(this.titleFont, 240, 50, "Select a Level",
				Color.orange);
		
		//Draw the arrow icons
		this.arrowIconShaded.draw(247, 457);
		this.arrowIcon.draw(245,455);
		
		graphics.setColor(Color.black);
		graphics.fillRect(110-4, 175-4, this.getLevel1Image().getWidth()+8, this.getLevel1Image().getHeight()+8);
		graphics.fillRect(412-4, 175-4, this.getLevel1Image().getWidth()+8, this.getLevel1Image().getHeight()+8);
		//graphics.setColor(Color.white);
		//graphics.fillRect(110-2, 175-2, this.getLevel1Image().getWidth()+4, this.getLevel1Image().getHeight()+4);
		graphics.setColor(Color.black);
		
		// Draw other text and images
		this.checkForSelection(gameContainer, game);
	}
	
	/**
	 * @return				the state's unique ID
	 */
	@Override
	public int getID()
	{
		return WindowManager.LEVEL_SELECT_STATE;
	}

	// Accessors
	/**
	 * @return			The background image
	 */
	public Image getBackgroundImage()
	{
		return this.backgroundImage;
	}
	
	/**
	 * @return				image used for level 1 preview
	 */
	public Image getLevel1Image()
	{
		return this.level1Image;
	}
	
	/**
	 * @return				image used for level 1 preview
	 * 						(when hovered over)
	 */
	public Image getLevel1ImageHover()
	{
		return this.level1ImageHover;
	}
	
	/**
	 * @return				image used for level 2 preview
	 */
	public Image getLevel2Image()
	{
		return this.level2Image;
	}
	
	/**
	 * @return				image used for level 2 preview
	 * 						(when hovered over)
	 */
	public Image getLevel2ImageHover()
	{
		return this.level2ImageHover;
	}
	
	/**
	 * @return				the arrow icon
	 */
	public Image getArrowIcon()
	{
		return this.arrowIcon;
	}
	
	/**
	 * @return				the shaded arrow icon
	 */
	public Image getArrowIconShaded()
	{
		return this.arrowIconShaded;
	}
	
	
	// Mutators
	/**
	 * @param backgroundImage	the new background image
	 */
	public void setBackgroundImage(Image backgroundImage)
	{
		this.backgroundImage = backgroundImage;
	}
	
	/**
	 * @param level1Image		the new image to use for level 1 preview
	 */
	public void setLevel1Image(Image level1Image)
	{
		this.level1Image = level1Image;
	}
	
	/**
	/**
	 * @param level1ImageHover	the new image to use for level 1 preview
	 * 							(when hovered over)
	 */
	public void setLevel1ImageHover(Image level1ImageHover)
	{
		this.level1ImageHover = level1ImageHover;
	}
	
	/**
	 * @param level2Image	the new image to use for level 2 preview
	 */
	public void setLevel2Image(Image level2Image)
	{
		this.level2Image = level2Image;
	}
	
	/**
	 * @param level2ImageHover	the new image to use for level 2 preview
	 * 							(when hovered over)
	 */
	public void setLevel2ImageHover(Image level2ImageHover)
	{
		this.level2ImageHover = level2ImageHover;
	}
	
	/**
	 * @param arrowIcon		the arrow icon
	 */
	public void setArrowIcon(Image arrowIcon)
	{
		this.arrowIcon = arrowIcon;
	}
	
	/**
	 * @param arrowIconShaded	the shaded arrow icon
	 */
	public void setArrowIconShaded(Image arrowIconShaded)
	{
		this.arrowIconShaded = arrowIconShaded;
	}
}
