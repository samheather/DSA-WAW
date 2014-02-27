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
	
	/** The image (map preview) for level 3 */
	private Image level3Image;
	
	/** The active image (map preview) for level 3 */
	private Image level3ImageHover;
	
	/** The arrow icon */
	private Image arrowIcon;
	
	/** The shaded arrow icon */
	private Image arrowIconShaded;
	
	private int[] image1Pos;
	private int[] image2Pos;
	private int[] image3Pos;
	
	
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
	private void drawButtons(GameContainer gameContainer,
			StateBasedGame game)
	{
		// Mouse coordinates
		int x 				= gameContainer.getInput().getMouseX();
		int y 				= gameContainer.getInput().getMouseY();
		
		// Mouse action
		boolean clicked 	= gameContainer.getInput().isMousePressed(0);

		// Text
		String titleText 			= "Select a Level";
		String level1Text 			= "Level 1";
		String level2Text 			= "Level 2";
		String level3Text 			= "Level 3";
		String difficulty1 			= "Easy";
		String difficulty2 			= "Medium";
		String difficulty3 			= "Hard";
		String mainMenuText			= "Main Menu";

		// Get the text width
		int titleTextWidth			= this.font.getWidth(titleText);
		int level1Width 			= this.font.getWidth(level1Text);
		int level2Width 			= this.font.getWidth(level2Text);
		int level3Width 			= this.font.getWidth(level3Text);
		int difficulty1Width 		= this.font.getWidth(difficulty1);
		int difficulty2Width 		= this.font.getWidth(difficulty2);
		int difficulty3Width 		= this.font.getWidth(difficulty3);
		int mainMenuWidth		 	= this.font.getWidth(mainMenuText);

		// Get the text height
		int fontHeight 				= this.font.getHeight();

		// Set the colours for the text
		Color titleColor			= Color.orange;
		Color level1Color 			= Color.orange;
		Color level2Color 			= Color.orange;
		Color level3Color 			= Color.orange;
		Color mainMenuColor 		= Color.orange;
		
		//size of images on screen
		
		int[] levelImageSize = {200,200};
		
		// position of images on the screen in pixels
		int[] titlePos = {(gameContainer.getWidth()/2) - titleTextWidth, 50};
		int[] level1ImagePos = {110,175};
		int[] level2ImagePos = {(gameContainer.getWidth() - levelImageSize[0])/2,175};
		int[] level3ImagePos = {(gameContainer.getWidth() - levelImageSize[0] - 110),175};
		int[] mainMenuTextPos = {50, gameContainer.getHeight() - 50};
		
		// duplicating the values for the black squares to outline the images
		image1Pos = level1ImagePos;
		image2Pos = level2ImagePos;
		image3Pos = level3ImagePos;
		
		// Set the images for the levels
		Image level1ImageCurrent	= this.level1Image;
		Image level2ImageCurrent 	= this.level2Image;
		Image level3ImageCurrent 	= this.level1Image;

		// tolerance for clicking on image in pixels
		int tolerance = 3;
		
		// Draw main title
		this.drawShadowedText(this.titleFont, titlePos[0], titlePos[1], titleText,
						titleColor);
		
		// Level 1 Text
		this.drawShadowedText(this.font, level1ImagePos[0] + (levelImageSize[0]/2) - (level1Width/2), level1ImagePos[1] - 50, level1Text, level1Color);
		
		// Level 1 difficulty text
		this.drawShadowedText(this.font, level1ImagePos[0] + (levelImageSize[0]/2) - (difficulty1Width/2), level1ImagePos[1] + 230, difficulty1, level1Color);


		// Level 1 Image
		if ((x >= (level1ImagePos[0] - tolerance))
				&& (x <= (level1ImagePos[0] + levelImageSize[0] + tolerance))
				&& (y >= (level1ImagePos[1] - tolerance))
				&& (y <= (level1ImagePos[1] + levelImageSize[1] + tolerance)))
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

		level1ImageCurrent.draw(level1ImagePos[0], level1ImagePos[1], 200, 200);
		
		
		
		// Level 2 Text
		this.drawShadowedText(this.font, level2ImagePos[0] + (levelImageSize[0]/2) - (level2Width/2), level2ImagePos[1] - 50, level2Text, level2Color);
		
		//Level 2 difficulty text
		this.drawShadowedText(this.font, level2ImagePos[0] + (levelImageSize[0]/2) - (difficulty2Width/2), level2ImagePos[1] + 230, difficulty2, level2Color);

		// Level 2 Image
		if((x >= (level2ImagePos[0] - tolerance))
				&& (x <= (level2ImagePos[0] + levelImageSize[0] + tolerance))
				&& (y >= (level2ImagePos[1] - tolerance))
				&& (y <= (level2ImagePos[1] + levelImageSize[1] + tolerance)))
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
				level2ImageCurrent = this.level2ImageHover;
			}
		}
		else
		{	
			// Draw the level2 image
			level2ImageCurrent = this.level2Image;
		}

		level2ImageCurrent.draw(level2ImagePos[0], level2ImagePos[1], 200, 200);
		
		// Level 3 Text
				this.drawShadowedText(this.font, level3ImagePos[0] + (levelImageSize[0]/2) - (level3Width/2), level3ImagePos[1] - 50, level3Text, level3Color);
				
				//Level 3 difficulty text
				this.drawShadowedText(this.font, level3ImagePos[0] + (levelImageSize[0]/2) - (difficulty3Width/2), level3ImagePos[1] + 230, difficulty3, level3Color);

				// Level 3 Image
				if((x >= (level3ImagePos[0] - tolerance))
						&& (x <= (level3ImagePos[0] + levelImageSize[0] + tolerance))
						&& (y >= (level3ImagePos[1] - tolerance))
						&& (y <= (level3ImagePos[1] + levelImageSize[1] + tolerance)))
					{
					// Change state to game state if the level is picked
					if(clicked)
					{
						((WindowManager) game).setCurrentLevel(3);
						game.enterState(WindowManager.GAME_STATE);
					}
					else
					{
						// Apply hover image if cursor is hovering
						/*TODO
						 * Change this to another image
						 */
						level3ImageCurrent = this.level2ImageHover;
					}
				}
				else
				{	
					// Draw the level3 image
					/*TODO
					 * Change this to another image
					 */
					level3ImageCurrent = this.level2Image;
				}

				level3ImageCurrent.draw(level3ImagePos[0], level3ImagePos[1], 200, 200);

		// Main Menu
		if ((x >= (mainMenuTextPos[0] - tolerance))
				&& (x <= (mainMenuTextPos[0] + mainMenuWidth + tolerance))
				&& (y >= (mainMenuTextPos[1] - tolerance))
				&& (y <= (mainMenuTextPos[1] + fontHeight + tolerance)))
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
		this.drawShadowedText(this.font, mainMenuTextPos[0], mainMenuTextPos[1],
				mainMenuText, mainMenuColor);
		
		//Draw the arrow icons next to main menu text
		this.arrowIconShaded.draw(mainMenuTextPos[0] + mainMenuWidth + 20, mainMenuTextPos[1]);
		this.arrowIcon.draw(mainMenuTextPos[0] + mainMenuWidth + 18,mainMenuTextPos[1] - 2);
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
		
		graphics.setColor(Color.black);
		if (image1Pos != null && image2Pos != null && image3Pos != null){
			graphics.fillRect(image1Pos[0]-4, image1Pos[1]-4, this.getLevel1Image().getWidth()+8, this.getLevel1Image().getHeight()+8);
			graphics.fillRect(image2Pos[0]-4, image2Pos[1]-4, this.getLevel2Image().getWidth()+8, this.getLevel2Image().getHeight()+8);
			graphics.fillRect(image3Pos[0]-4, image3Pos[1]-4, this.getLevel3Image().getWidth()+8, this.getLevel3Image().getHeight()+8);
		}
		
		// Draw other text and images
		this.drawButtons(gameContainer, game);
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
	 * @return				image used for level 3 preview
	 */
	public Image getLevel3Image()
	{
		/*TODO
		 * Change to level3 image once we have one
		 */
		return this.level2Image;
	}
	
	/**
	 * @return				image used for level 3 preview
	 * 						(when hovered over)
	 */
	public Image getLevel3ImageHover()
	{
		return this.level3ImageHover;
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
