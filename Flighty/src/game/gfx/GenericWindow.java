package game.gfx;


import org.newdawn.slick.AppGameContainer;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.TrueTypeFont;
import org.newdawn.slick.state.BasicGameState;
import org.newdawn.slick.state.StateBasedGame;

import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.io.InputStream;


/**
 * GenericWindow class provides a template for a basic state
 * <p>
 * WindowManager loads the game states, the game container,
 * and controls how states exit.
 * </p>
 */
public abstract class GenericWindow extends BasicGameState {
	
	/** The Java font used to generate the fonts used */
	protected Font fontPrimitive;
	
	/** The generic TrueType font */
	protected TrueTypeFont font;
	
	/** The title TrueType font */
	protected TrueTypeFont titleFont;
	
	
	// Accessors
	/**
	 * @return				the base font
	 */
	public Font getFontPrimitive() {
		return this.fontPrimitive;
	}
	
	/**
	 * @return				the normal font
	 */
	public TrueTypeFont getFont() {
		return this.font;
	}
	
	/**
	 * @return				the title font
	 */
	public TrueTypeFont getTitleFont() {
		return this.titleFont;
	}
	
	
	// Mutators
	/**
	 * @param fontPrimitive	the new base font
	 */
	public void setFontPrimitive(Font fontPrimitive) {
		this.fontPrimitive = fontPrimitive;
	}
	
	/**
	 * @param font			the new normal font derivative
	 */
	public void setFont(TrueTypeFont font) {
		this.font = font;
	}
	
	/**
	 * @param titleFont		the new title font derivative
	 */
	public void setTitleFont(TrueTypeFont titleFont) {
		this.titleFont = titleFont;
	}
	
	
	// Other methods
	/**
	 * Draws the text specified with a black shadow
	 * 
	 * @param font			the TrueType font to display the text in
	 * @param x				the x position to display the text at
	 * @param y				the y position to display the text at
	 * @param text			the string to display
	 * @param color			the colour to display the text in
	 */
	protected void drawShadowedText(TrueTypeFont font, int x, int y, String text, Color color) {
		font.drawString(x + 2, y + 2, text, Color.black);
		font.drawString(x, y, text, color);
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
		// Setup input
		this.setInput(gameContainer.getInput());
		
		// Stop game exiting on window close
		gameContainer.setForceExit(false);
		
		// Setup fonts
		try {
			InputStream fontStream = getClass()
					.getResourceAsStream("/resources/fonts/8BitWonder.ttf");
			Font newFont = Font.createFont(Font.TRUETYPE_FONT, fontStream);
		    GraphicsEnvironment.getLocalGraphicsEnvironment().registerFont(newFont);
		    this.fontPrimitive = newFont;
		} catch (Exception e) { 
		    e.printStackTrace();
		    this.fontPrimitive = new Font(Font.SERIF, Font.PLAIN, 12);
		}
		
		this.font = new TrueTypeFont(this.fontPrimitive
				.deriveFont(Font.CENTER_BASELINE, 20.0f), false);
		this.titleFont = new TrueTypeFont(this.fontPrimitive
				.deriveFont(Font.BOLD, 40.0f), false);
	}
	
	/**
	 * Sets properties which need resetting each time the state is accessed
	 * 
	 * @param gameContainer	the game container holding this state
	 * @param game			the game running this state
	 */
	@Override
	public void enter(GameContainer gameContainer,
			StateBasedGame game) throws SlickException {
		((AppGameContainer) gameContainer).setDisplayMode(
				WindowManager.WINDOW_WIDTH,
				WindowManager.WINDOW_HEIGHT, false);
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

		// Set background colour
		graphics.setBackground(Color.black);
	}

	/**
	 * Updates the state
	 * 
	 * @param gameContainer	the game container holding this state
	 * @param game			the game running this state
	 * @param delta			the time change between calls
	 */
	@Override
	public void update(GameContainer gameContainer,
			StateBasedGame game, int delta) throws SlickException {
		double skyProgress = ((WindowManager) game).getSkyProgress();
		double planeProgress = ((WindowManager) game).getPlaneProgress();
		
		if(skyProgress >= gameContainer.getWidth()) {
			((WindowManager) game).setSkyProgress(0);
		} else {
			((WindowManager) game).setSkyProgress(skyProgress + 0.5);
		}
		
		if(planeProgress >= (gameContainer.getWidth() + 50)) {
			((WindowManager) game).setPlaneProgress(0);
		} else {
			((WindowManager) game).setPlaneProgress(planeProgress + 2);
		}
	}

	/**
	 * @return				the state's unique ID
	 */
	@Override
	public int getID() {
		return 0;
	}
}

