package game.gfx;


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
	 * @param gameContainer	the game container holding this state
	 * @param game			the game running this state
	 */
	private void checkForSelection(GameContainer gameContainer,
				StateBasedGame game)
		{
			// Mouse coordinates
			int x 					= gameContainer.getInput().getMouseX();
			int y 					= gameContainer.getInput().getMouseY();
			
			// Mouse action
			boolean clicked 		= gameContainer.getInput().isMousePressed(0);
			
			// Text
			String startGameText 	= "Start Game";
			String creditsText 		= "Credits";
			String controlsText 	= "Controls";
			String exitText 		= "Exit";
			
			// Get the width of the buttons
			int startGameWidth 		= this.font.getWidth(startGameText);
			int creditsWidth 		= this.font.getWidth(creditsText);
			int controlsWidth 		= this.font.getWidth(controlsText);
			int exitWidth 			= this.font.getWidth(exitText);
			
			// Get the height of the text
			int textHeight 			= this.font.getHeight();
			
			// Set the colour for text
			Color startGameColor 	= Color.orange;
			Color creditsColor 		= Color.orange;
			Color controlsColor 	= Color.orange;
			Color exitColor 		= Color.orange;
			
			// Start Game
			if((x >= (gameContainer.getWidth() / 3) + 30 - 25)
					&& (y >= (gameContainer.getHeight() / 3) + 50 - 25)
					&& (x <= (gameContainer.getWidth() / 3) + 30
					+ startGameWidth + 25)
					&& (y <= (gameContainer.getHeight() / 3) + 50
					+ textHeight + 25))
			{
				if(clicked)
				{
					// Change game state
					game.enterState(WindowManager.LEVEL_SELECT_STATE);
				}
				else
				{
					// Change hover text and add waypoint next to text
					startGameColor = Color.white;
					this.waypointIcon.draw((gameContainer.getWidth() / 3)
							+ startGameWidth + 30, (gameContainer.getHeight() / 3) - 47 + (this.font.getHeight() * 4));
				}
			}
			else
			{
				startGameColor = Color.orange;
			}
			
			// Draw the Start Game text with a shadow
			this.drawShadowedText(this.font, (gameContainer.getWidth() / 3) + 30, 
					(gameContainer.getHeight() / 3) - 50 + (this.font.getHeight() * 4),
					startGameText, startGameColor);
			
			
			// Credits
			if((x >= (gameContainer.getWidth() / 3) + 30 - 25)
					&& (y >= (gameContainer.getHeight() / 3) + 50 
					+ (this.font.getHeight() * 4) - 25)
					&& (x <= (gameContainer.getWidth() / 3) + 30
					+ creditsWidth + 25)
					&& (y <= (gameContainer.getHeight() / 3) + 50
					+ (this.font.getHeight() * 4) + textHeight + 25))
			{
				if(clicked)
				{
					// Change game state
					game.enterState(WindowManager.CREDITS_STATE);
				}
				else
				{
					creditsColor = Color.white;
					
					// Change hover text and add waypoint next to text
					this.waypointIcon.draw((gameContainer.getWidth() / 3)
							+ 30 + creditsWidth, (gameContainer.getHeight() / 3) 
							+ 54 + (this.font.getHeight() * 4));
				}
			}
			else
			{
				creditsColor = Color.orange;
			}
			
			// Draw the Credits text with a shadow
			this.drawShadowedText(this.font, (gameContainer.getWidth() / 3)
					+ 30, (gameContainer.getHeight() / 3) + 50
					+ (this.font.getHeight() * 4), creditsText, creditsColor);
			
			
			// Controls
					if((x >= (gameContainer.getWidth() / 3) + 30 - 25)
							&& (y >= (gameContainer.getHeight() / 3) + 150 
							+ (this.font.getHeight() * 4))
							&& (x <= (gameContainer.getWidth() / 3) + 30
							+ creditsWidth + 25)
							&& (y <= (gameContainer.getHeight() / 3) + 160
							+ (this.font.getHeight() * 4) + textHeight + 25))
					{
						if(clicked)
						{
							// Change game state
							game.enterState(WindowManager.CONTROLS_STATE);
						}
						else
						{
							// Change hover text and add waypoint next to text
							controlsColor = Color.white;
							this.waypointIcon.draw((gameContainer.getWidth() / 3)
									+ controlsWidth + 30, (gameContainer.getHeight() / 3) + 153
									+ (this.font.getHeight() * 4));
						}
					}
					else
					{
						controlsColor = Color.orange;
					}
			
			// Draw the Controls text with a shadow
			this.drawShadowedText(this.font, (gameContainer.getWidth() / 3)
						+ 30, (gameContainer.getHeight() / 3) + 150
						+ (this.font.getHeight() * 4), controlsText, controlsColor);
					
			
			
			// Exit
			if((x >= (50 - 25))
					&& (y >= (gameContainer.getHeight() - 50 - 25))
					&& (x <= (50 - 25) + exitWidth + 25)
					&& (y <= (gameContainer.getHeight() - 50 + textHeight + 25)))
			{
				if(clicked)
				{
					System.exit(0);
				}
				else
				{
					exitColor = Color.white;
				}
			}
			else
			{
				// Change colour on hover
				exitColor = Color.orange;
			}
			
			// Draw the Exit text with a shadow
			this.drawShadowedText(this.font, 50, gameContainer.getHeight()
					- 50, exitText, exitColor);
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
				StateBasedGame game) throws SlickException {
			super.init(gameContainer, game);
			
			InputStream backgroundStream = this.getClass()
					.getResourceAsStream(
							"/resources/backgrounds/CloudBackground.png");
			InputStream planeStream = this.getClass()
					.getResourceAsStream("/resources/planes/TitlePlane.png");
			InputStream waypointStream = this.getClass()
					.getResourceAsStream("/resources/waypoints/WaypointRed.png");
			
			this.backgroundImage = new Image(backgroundStream,
					"Background Image", false);
			this.planeIcon = new Image(planeStream,
					"Plane Icon", false);		
			this.waypointIcon = new Image(waypointStream,
					"Waypoint Icon", false);
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
			
			// Draw background
			this.backgroundImage.draw((float) (0 - ((WindowManager) game)
					.getSkyProgress()), 0, gameContainer.getWidth() * 2,
					gameContainer.getHeight());
			
			// Draw main title
			int mainTitleXPos = (gameContainer.getWidth() / 2)
					- (this.titleFont.getWidth("World of") / 2);
			
			int subtitleXPos = (gameContainer.getWidth() / 2)
					- (this.titleFont.getWidth("Flighty") / 2);
			
			this.drawShadowedText(this.titleFont, mainTitleXPos, 50,
					"World of", Color.orange);
			this.drawShadowedText(this.titleFont, subtitleXPos, 125,
					"Flighty", Color.orange);
			
			// Draw other text
			this.checkForSelection(gameContainer, game);
			
			// Draw plane icon
			this.planeIcon.draw((float) (((WindowManager) game).getPlaneProgress()
					- 50), 50 + this.font.getHeight() + 105, 50, 50);
		}
		
	
	
	// Accessors
	/**
	 * @return			the background image
	 */
	public Image getBackgroundImage()
	{
		return this.backgroundImage;
	}
	
	/**
	 * @return			the plane icon
	 */
	public Image getPlaneIcon()
	{
		return this.planeIcon;
	}
	
	/**
	 * @return			the waypoint icon
	 */
	public Image getWaypointIcon()
	{
		return this.waypointIcon;
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
	 * @param planeIcon		the new plane icon
	 */
	public void setPlaneIcon(Image planeIcon)
	{
		this.planeIcon = planeIcon;
	}
	
	/**
	 * @param waypointIcon	the new waypoint icon
	 */
	public void setWaypointIcon(Image waypointIcon)
	{
		this.waypointIcon = waypointIcon;
	}
	
	/**
	 * @return				the state's unique ID
	 */
	@Override
	public int getID()
	{
		return WindowManager.MAIN_MENU_STATE;
	}
}

