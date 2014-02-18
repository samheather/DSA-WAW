package game.gfx;


import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.state.StateBasedGame;

import java.io.InputStream;


/**
 * Credits class provides a credits screen
 */
public class Credits extends GenericWindow
{	
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
	 * @param gameContainer	the game container holding this state
	 * @param game			the game running this state
	 */
	private void checkForSelection(GameContainer gameContainer,
			StateBasedGame game)
	{
		int x 				= gameContainer.getInput().getMouseX();
		int y 				= gameContainer.getInput().getMouseY();
		boolean clicked 	= gameContainer.getInput().isMousePressed(0);
		
		String mainMenuText = "Main Menu";
		
		int mainMenuWidth 	= this.font.getWidth(mainMenuText);
		int textHeight 		= this.font.getHeight();
		
		Color mainMenuColor = Color.orange;
		
		if((x >= (50 - 25))
				&& (y >= (gameContainer.getHeight() - 50 - 25))
				&& (x <= (50 - 25) + mainMenuWidth + 25)
				&& (y <= (gameContainer.getHeight() - 50 + textHeight + 25)))
		{
			if(clicked)
			{
				game.enterState(WindowManager.MAIN_MENU_STATE);
			}
			else
			{
				mainMenuColor = Color.white;
			}
		}
		else
		{
			mainMenuColor = Color.orange;
		}
		
		this.drawShadowedText(this.font, 50, gameContainer.getHeight()
					- 50, mainMenuText, mainMenuColor);
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
		
		// Load images
		InputStream backgroundStream = this.getClass()
				.getResourceAsStream(
						"/resources/backgrounds/CloudBackground.png");
		InputStream arrowStream = this.getClass()
				.getResourceAsStream(
						"/resources/other/ArrowR.png");
		InputStream arrowShadedStream = this.getClass()
				.getResourceAsStream(
						"resources/other/ArrowB.png");
		
		this.backgroundImage = new Image(backgroundStream,
				"Background Image", false);
		this.arrowIcon = new Image(arrowStream,
				"Arrow Image", false);
		this.arrowIconShaded = new Image(arrowShadedStream,
				"Arrow Shaded Image", false);
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
			StateBasedGame game, Graphics graphics) throws SlickException {
		super.render(gameContainer, game, graphics);
		
		int textHeight = this.font.getHeight();
		
		// Draw the Background Image
		this.backgroundImage.draw((float) (0 - ((WindowManager) game)
				.getSkyProgress()), 0, gameContainer.getWidth() * 2,
				gameContainer.getHeight());
		
		// Draw main title 
		int mainXPos = (gameContainer.getWidth() / 2)
				- (this.titleFont.getWidth("Credits") / 2);
		
		this.drawShadowedText(this.titleFont, mainXPos, 20, "Credits", Color.orange);
		
		
		// Draw team name
		int subXPos = (gameContainer.getWidth() / 2)
				- (this.font.getWidth("Team GOA") / 2);

		this.drawShadowedText(this.font, subXPos, 120, "Team GOA", Color.orange);
		this.drawShadowedText(this.font, subXPos, 100, "Team WAW", Color.orange);
		
		// Draw team member names
		this.drawShadowedText(this.font, 75, 180, 
				"Nice Game Find     Copyright 2013", Color.orange);
		this.drawShadowedText(this.font, 75, 180 +(textHeight*1), 
				"Iwan Gabovitch    CCBY3  License", Color.orange);
		
		
/*		this.drawShadowedText(this.font, 75, 180,
	    		"Richard RichTea Kirby", Color.transparent);
		this.drawShadowedText(this.font, 75, 180,
	    		"Liviu Pirvan", Color.orange);
		
		this.drawShadowedText(this.font, 75, 180 + (textHeight * 3),
	    		"Jaron Read Access Ali", Color.transparent);
		this.drawShadowedText(this.font, 75, 180 + (textHeight * 3),
	    		"Oliver Ramirez", Color.orange);
		
		this.drawShadowedText(this.font, 75, 180 + (textHeight * 6),
	    		"Richard Wandering Overlord Aickin", Color.transparent);
		this.drawShadowedText(this.font, 75, 180 + (textHeight * 6),
	    		"Lewis Shaw", Color.orange);
		
		this.drawShadowedText(this.font, 75, 180 + (textHeight * 9),
	    		"Jonathan Slurrrrp Howell", Color.transparent);
		this.drawShadowedText(this.font, 75, 180 + (textHeight * 9),
	    		"Rory Simpson", Color.orange);
		
		this.drawShadowedText(this.font, 75, 180 + (textHeight * 12),
	    		"Emily Emily Hall Hall", Color.transparent);
		this.drawShadowedText(this.font, 75, 180 + (textHeight * 12),
	    		"Adam Taylor", Color.orange);
		
		this.drawShadowedText(this.font, 75, 180 + (textHeight * 15),
	    		"Sam Much Hopkins", Color.transparent);
		this.drawShadowedText(this.font, 75, 180 + (textHeight * 15),
	    		"Adam Wright", Color.orange);
		
		this.drawShadowedText(this.font, 75, 180 + (textHeight * 18),
	    		"Mark Washing Woosey", Color.transparent);
		this.drawShadowedText(this.font, 75, 180 + (textHeight * 18),
				"Team WAW", Color.orange);
*/		
	    // Draw back text
	    this.checkForSelection(gameContainer, game);
	    
	 // Draw shaded arrow icon
		this.arrowIconShaded.draw(247, gameContainer.getHeight()
				- 48 - (textHeight / 4), 45, 35);
	    
		// Draw arrow icon
		this.arrowIcon.draw(245, gameContainer.getHeight()
				- 50 - (textHeight / 4), 45, 35);
		
	}
	
	/**
	 * @return				the state's unique ID
	 */
	@Override
	public int getID()
	{
		return WindowManager.CREDITS_STATE;
	}

	
	// Accessors
	/**
	 * @return				the background image
	 */
	public Image getBackgroundImage()
	{
		return this.backgroundImage;
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
	 * @param backgroundImage		the new background image
	 */
	public void setBackgroundImage(Image backgroundImage)
	{
		this.backgroundImage = backgroundImage;
	}
	
	/**
	 * @param arrowIcon		the new arrow icon
	 */
	public void setArrowIcon(Image arrowIcon)
	{
		this.arrowIcon = arrowIcon;
	}
	
	/**
	 * @param shadedArrowIcon	the new shaded arrow icon
	 */
	public void setArrowIconShaded(Image shadedArrowIcon)
	{
		this.arrowIconShaded = arrowIcon;
	}
}
