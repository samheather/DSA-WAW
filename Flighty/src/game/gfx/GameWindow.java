package game.gfx;

import org.newdawn.slick.AppGameContainer;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.Sound;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.TrueTypeFont;
import org.newdawn.slick.state.BasicGameState;
import org.newdawn.slick.state.StateBasedGame;

import java.awt.Font;
import java.io.InputStream;
import java.util.ArrayList;
import game.struct.Game;
import game.struct.Plane;

/**
 * GameWindow class provides an interactive game
 */
public class GameWindow extends BasicGameState {
	
	/** Time between score penalities for not taking off*/
	private static final int TAKE_OFF_PENALTY_TIME = 3000;

	/** The width the game is displayed at */
	public int windowWidth;

	/** The height the game is displayed at */
	public int windowHeight;

	/** The time the game has been running for */
	private double time;

	/** The time the game ended at */
	private double endTime;

	private Game currentGame;
	
	/** The current map */
	private Image map;

	/** Map image for level 1 */
	private Image map1;

	/** Map image for level 2 */
	private Image map2;

	/** The normal waypoint image */
	private Image waypointNormal;

	/** The waypoint image for the current plane's next waypoint */
	private Image waypointNext;

	/** The normal waypoint image for exit waypoints */
	private Image waypointExit;

	/** The waypoint image for the current plane's exit waypoint */
	private Image waypointLast;

	private Image waypointArrow;

	/** The normal plane image */
	private Image planeNormal;

	/** The edited plane image */
	private Image planeNormalCur;

	/** The plane image for the selected plane */
	private Image planeSelected;

	/** The edited plane image for the selected plane */
	private Image planeSelectedCur;

	// Introducing Plane Needs Landing Image
	private Image planeNeedsLanding;
	private Image planeNeedsLandingCur;

	// Introducing landing approach Image, plus a boolean tracking whether it's
	// been drawn
	private Image landingApproachArea;
	private boolean landingApproachAreaDrawn;

	/** The plane alert range image */
	private Image planeAlert;

	/** The plane collision range image */
	private Image planeAlertMax;

	/** The Java font used to generate the fonts used */
	private Font fontPrimitive;

	/** The generic TrueType font */
	private TrueTypeFont font;

	/** The colour to display the font in */
	private Color fontColor;

	/** Reference to the game container */
	GameContainer currentGameContainer;

	/** Whether it should display extrapoints taken (e.g. above waypoints) **/
	boolean display = false;

	/** Whether it should display more points for some points (e.g. airport) **/
	boolean morePoints = false;

	/** Boolean to help with displaying extra points above the waypoints**/
	double synch = 100;

	/** Boolean to help with timing the penalties for not taking off penalty **/
	double synchTakeOff = TAKE_OFF_PENALTY_TIME;
	
	/** Boolean to make sure the points aren't reduce multiple times**/
	boolean reducePoints = true;	
	
	/** Coordinates of last waypoint passed **/
	double prevX;
	double prevY;

	/** Sound used for indicating that current flight has gone through waypoint **/
	Sound checkpointSound;

	// Other methods (<- locator TODO)

	/**
	 * Converts an altitude level to a height
	 * 
	 * @param altitude
	 *            the altitude level to convert
	 */
	public void giveHeadingThroughMouse(Plane currentPlane, int x, int y)
	{
		this.currentGame.getCurrentPlane().setTurningLeft(false);
		this.currentGame.getCurrentPlane().setTurningRight(false);
		
		if (!this.currentGame.getManualPlanes().contains(currentPlane))
		{
			this.currentGame.getManualPlanes().add(currentPlane);
		}
		
		double newBearing = Math.toDegrees(Math.atan2(this.currentGame
				.getCurrentPlane().getY() - y, this.currentGame
				.getCurrentPlane().getX() - x));
		if (newBearing < 0)
		{
			newBearing += 360;
		}
		this.currentGame.getCurrentPlane().setTargetBearing(newBearing);
	}
	/**
	 * Method ot facilitate the selection of planes via cursors
	 * 
	 * @param x - x coordinate
	 * @param y - y coordinate
	 * @return  - returns selected plane if any
	 */
	public Plane selectFlight(int x, int y)
	{
		Plane nearestPlane;
		
		// Distance from where the user clicked to the nearest plane
		double distanceToPlane;

		if (this.currentGame.getCurrentPlanes().size() >= 1)
		{
			distanceToPlane = Math.sqrt(Math.pow(x
								- this.currentGame.getCurrentPlanes().get(0).getX(), 2)
										+ Math.pow(y - this.currentGame.getCurrentPlanes()
											.get(0).getY(), 2));
			nearestPlane = this.currentGame.getCurrentPlanes().get(0);

			// Loop through all the planes and find the nearest one
			for (int i = 0; i < this.currentGame.getCurrentPlanes().size(); i++)
			{ 															
				if (Math.sqrt(Math.pow(x
						- this.currentGame.getCurrentPlanes().get(i).getX(), 2)
						+ Math.pow(y - this.currentGame.getCurrentPlanes()
												.get(i).getY(), 2)) < distanceToPlane)
				{
					distanceToPlane = Math.sqrt(Math
							.pow(x - this.currentGame.getCurrentPlanes()
											.get(i).getX(), 2)
							+ Math.pow(y - this.currentGame.getCurrentPlanes()
											.get(i).getY(), 2));
					nearestPlane = this.currentGame.getCurrentPlanes().get(i);
				}
				
				// Checks if the nearest plane is close enough to be considered for picking
				if (distanceToPlane <= 50)
				{
					if (nearestPlane.equals(this.currentGame.getCurrentPlane()))
					{
						return null;
					}
					else
					{
						if (!nearestPlane.isLanding()
								&& !nearestPlane.isTakingOff())
						{
							return nearestPlane;
						}
					}
				}
			}
		}
		
		return null;
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
			throws SlickException
	{
		// Setup input
		this.setInput(gameContainer.getInput());

		// Load waypoint images
		InputStream waypointNormalStream = this.getClass().getResourceAsStream(
				"/resources/waypoints/WaypointRed.png");
		InputStream waypointNextStream = this.getClass().getResourceAsStream(
				"/resources/waypoints/WaypointWhite.png");
		InputStream waypointLastStream = this.getClass().getResourceAsStream(
				"/resources/waypoints/WaypointGreen.png");
		InputStream waypointExitStream = this.getClass().getResourceAsStream(
				"/resources/waypoints/WaypointBlue.png");
		InputStream waypointArrowStream = this.getClass().getResourceAsStream(
				"/resources/other/ArrowW.png");

		this.waypointNormal = new Image(waypointNormalStream,
				"Waypoint Normal Image", false);
		this.waypointNext = new Image(waypointNextStream,
				"Waypoint Next Image", false);
		this.waypointLast = new Image(waypointLastStream,
				"Waypoint Last Image", false);
		this.waypointExit = new Image(waypointExitStream,
				"Waypoint Exit Image", false);
		this.waypointArrow = new Image(waypointArrowStream,
				"Waypoint Arrow Image", false);

		// Load plane images
		InputStream planeNormalStream = this.getClass().getResourceAsStream(
				"/resources/planes/Plane.png");
		InputStream planeSelectedStream = this.getClass().getResourceAsStream(
				"/resources/planes/PlaneSelected.png");
		InputStream planeAlertStream = this.getClass().getResourceAsStream(
				"/resources/planes/PlaneAlert.png");
		InputStream planeAlertMaxStream = this.getClass().getResourceAsStream(
				"/resources/planes/PlaneAlertMax.png");
		// - Introducing Landing Alert Image
		InputStream planeNeedsLandingStream = this.getClass().getResourceAsStream(
				"/resources/planes/PlaneNeedsLanding.png");

		// - Introducing approach highlight image
		InputStream approachHighlightStream = this.getClass()
				.getResourceAsStream(
						"/resources/other/airspaceIndicatorGreen.png");
		this.landingApproachArea = new Image(approachHighlightStream,
				"Approach area Highlight Image", false);

		this.planeNormal = new Image(planeNormalStream, "Plane Normal Image",
				false).getScaledCopy(14, 18);
		this.planeNormalCur = this.planeNormal;
		
		this.planeSelected = new Image(planeSelectedStream,
				"Plane Selected Image", false).getScaledCopy(14, 18);
		this.planeSelectedCur = this.planeSelected;

		this.planeAlert = new Image(planeAlertStream, "Plane Alert Image",
				false);

		this.planeAlertMax = new Image(planeAlertMaxStream,
				"Plane Alert Max Image", false);

		// - Continued Landing Image Introduction
		this.planeNeedsLanding = new Image(planeNeedsLandingStream,
				"Plane Needs Landing Image", false).getScaledCopy(14, 18);
		this.planeNeedsLandingCur = this.planeNeedsLanding;

		// Load map images
		InputStream map1Stream = this.getClass().getResourceAsStream(
				"/resources/maps/Map1.png");
		InputStream map2Stream = this.getClass().getResourceAsStream(
				"/resources/maps/Map2.png");

		this.map1 = new Image(map1Stream, "Map 1 Image", false);
		this.map2 = new Image(map2Stream, "Map 2 Image", false);

		// Set the font (used for altitudes etc.)
		this.fontPrimitive = new Font("Lucida Sans", Font.PLAIN, 12);
		this.font = new TrueTypeFont(this.fontPrimitive, true);

		// Initialise Waypoint Sound
		checkpointSound = new Sound("resources/music/checkpointSound.ogg");

	}

	/**
	 * Sets properties which need resetting each time the state is accessed
	 * 
	 * <p>
	 * Specifically, this clears the game so that it restarts when exited and
	 * re-entered.
	 * </p>
	 * 
	 * @param gameContainer
	 *            the game container holding this state
	 * @param game
	 *            the game running this state
	 */
	@Override
	public void enter(GameContainer gameContainer, StateBasedGame game)
			throws SlickException
	{
		this.currentGameContainer = gameContainer;

		time = 0;
		
		//Screen size
		this.windowWidth = 1200;
		this.windowHeight = 600;

		((AppGameContainer) gameContainer).setDisplayMode(this.windowWidth,
				this.windowHeight, false);

		if (((WindowManager) game).getCurrentLevel() == 1)
		{
			// Play level 1
			this.currentGame = new Game(50, 100);
			this.map = this.map1;
			this.fontColor = Color.white;
			this.currentGame.setSpeedDifficulty(0.5);
			this.currentGame.setSpawnRate(12);
			this.currentGame.setSpawnCount(1);
		}
		else if (((WindowManager) game).getCurrentLevel() == 2)
		{
			// Play level 2
			this.currentGame = new Game(70, 100);
			this.map = this.map1;
			this.fontColor = Color.white;
			this.currentGame.setSpeedDifficulty(0.5);
			this.currentGame.setSpawnRate(4);
			this.currentGame.setSpawnCount(1);
		}

	}
	/**
	 * Plays a sound at each checkpoint if the plane is selected
	 */
	public void playCheckpointSound()
	{
		if (this.currentGame.getCurrentPlane() != null
				&& this.currentGame.getCurrentPlane().getFlightPlan()
							.getCurrentRoute().size() != 0
				  && this.currentGame.getCurrentPlane().checkIfFlightAtWaypoint(
							this.currentGame.getCurrentPlane().getFlightPlan()
							.getCurrentRoute().get(0), currentGame))
		{
			checkpointSound.play();
		}

	}

	/**
	 * Renders the state
	 * 
	 * @param gameContainer
	 *            the game container holding this state
	 * @param game
	 *            the game running this state
	 * @param g
	 *            the graphics container to display content in
	 */
	@Override
	public void render(GameContainer gameContainer, StateBasedGame game,
			Graphics g)
	{

		// Draw the game map
		this.map.draw(0, 0, this.windowWidth, this.windowHeight);

		g.setAntiAlias(true);
		g.setFont(this.font);
		g.setColor(this.fontColor);

		if (!this.currentGame.isEnding())
		{
			// Drawing Pause Screen if in pause menu
			if (currentGameContainer.isPaused())
			{
				new TrueTypeFont(this.fontPrimitive.deriveFont(15f), true)
						.drawString(this.getWindowWidth() / 2 - 30,
								this.getWindowHeight() / 2 - 100, "PAUSE");
				new TrueTypeFont(this.fontPrimitive.deriveFont(15f), true)
						.drawString(this.getWindowWidth() / 2 - 30 - 200,
								this.getWindowHeight() / 2 - 65,
								"Steer a plane by selecting it and using the key arrows/ right click");
				new TrueTypeFont(this.fontPrimitive.deriveFont(15f), true)
						.drawString(this.getWindowWidth() / 2 - 30 - 190,
								this.getWindowHeight() / 2 - 25,
								"Land a plane by lowering it to 2000ft when it needs to land");
				new TrueTypeFont(this.fontPrimitive.deriveFont(15f), true)
						.drawString(this.getWindowWidth() / 2 - 30 - 130,
								this.getWindowHeight() / 2 - 5,
								"and pressing L when in the airport zone");
				new TrueTypeFont(this.fontPrimitive.deriveFont(15f), true)
						.drawString(this.getWindowWidth() / 2 - 30 - 190,
								this.getWindowHeight() / 2 + 35,
								"Take off a plane by selecting an airport plane and pressing T");

				new TrueTypeFont(this.fontPrimitive.deriveFont(15f), true)
						.drawString(this.getWindowWidth() / 2 - 30 - 50,
								this.getWindowHeight() / 2 + 70,
								"Press p to unpause");
			}

			// Display the Game Information
			g.drawString(
					"Time : "
					+ ((int) this.time / 1000 / 60 < 10 ? "0"
							+ (int) (this.time / 1000) / 60
							: (int) (this.time / 1000) / 60)
					+ ":"
					+ ((int) (this.time / 1000) % 60 < 10 ? "0"
							+ (int) (this.time / 1000) % 60
							: (int) (this.time / 1000) % 60), 
					1050, 15);
			g.drawString(
					"Score : "
					+ ((int) (this.currentGame.getScore()))
					+ " pts", 1050, 35);
			g.drawString(
					"Multiplier :"
					+ ((int) (this.currentGame.getMultiplier())),
					1050, 55);
			g.drawString("Pause/Controls: P ",
					1050, 75);

			for (Plane plane : this.currentGame.getCurrentPlanes())
			{
				// Sets to display the number of points gained above the passed waypoint
				if (plane.getFlightPlan().getCurrentRoute().size() > 1)
				{
					if (plane.checkIfFlightAtWaypoint(plane.getFlightPlan().getCurrentRoute().get(0), this.currentGame)){
						// If plane is at the runway, more points apply
						if (plane.getFlightPlan().getCurrentRoute().get(0) == currentGame.getAirport().getEndOfRunway())
						{
							morePoints = true;
						}
						
						// Saves the passed waypoint coordinates to know where to display the text
						prevX = plane.getFlightPlan().getCurrentRoute().get(0).getX();
						prevY = plane.getFlightPlan().getCurrentRoute().get(0).getY();
						
						// How long it should display the text for
						synch = 100;
						
						display = true;
					}
					// Renders the bonus points
					if (display && synch > 0)
					{
						if (morePoints){
							g.drawString("+" + Integer.toString(this.getCurrentGame().getMultiplier() * 10), (float) prevX - 8, (float) prevY - 30);
							morePoints = synch <= 1 ? false : true;
						}
						else if (plane.getFlightPlan().getCurrentRoute().get(0).equals(currentGame.getAirport().getBeginningOfRunway())){
							synch = 0;
						}
						else {
							g.drawString("+" + Integer.toString(this.getCurrentGame().getMultiplier() * 5), (float) prevX - 8, (float) prevY - 30);
						}
						
						synch--;
					}
					else
					{
						display = false;
					}
				}
				
				// Plane has been landed for too long, and the user is prompted to take it off 
				if (currentGame.isTakeOffPenalty())
				{	
					// Alert message to inform the user that the plane should be taken off
					g.drawString("Take me off", 1122, 555);
					synchTakeOff--;
					
					// Points deduced from the user if the plane is still landed
					if (synchTakeOff < TAKE_OFF_PENALTY_TIME / 5)
					{
						g.setColor(Color.red);
						g.drawString("-" + 10 * currentGame.getMultiplier() , 1150, 570);
						g.setColor(Color.white);
						
						if (reducePoints)
						{
							if (currentGame.getScore() > 10 * currentGame.getMultiplier())
							{
								currentGame.setScore(currentGame.getScore() - 10 * currentGame.getMultiplier());
							}
							else
							{
								currentGame.setScore(0);
							
							}
							// Don't reduce more points for the same penalty
							reducePoints = false;
						}
						
						synchTakeOff--;
						
						// Counter for reducing points restarted 
						if (synchTakeOff < 0)
						{
							reducePoints = true;
							synchTakeOff = TAKE_OFF_PENALTY_TIME;
						}
					}
				}
				
				// If plane is within penalty distance, apply alert images
				if (plane.getAlertStatus())
				{
					this.planeAlert.getScaledCopy(
							this.currentGame.getPenaltyDistance(),
							this.currentGame.getPenaltyDistance())
							.drawCentered((float) plane.getX(),
									(float) plane.getY());

					this.planeAlertMax.getScaledCopy(
							this.currentGame.getSeparationDistance(),
							this.currentGame.getSeparationDistance())
							.drawCentered((float) plane.getX(),
									(float) plane.getY());
				}

				// Render each plane
				if (plane.equals(this.currentGame.getCurrentPlane()))
				{
					// Active flights in the airspace are reaching at least 2000ft altitude
					if (plane.getAltitude() >= 2000)
					{
						this.planeSelectedCur = this.planeSelected
								.getScaledCopy(1 + ((((float) (plane.getSize())) - 1) / 5));
					}
					this.planeSelectedCur.setRotation((float) plane
							.getBearing() - 90);
					this.planeSelectedCur.drawCentered((float) plane.getX(),
							(float) plane.getY());
				}
				else
				{
					// Planes under 2000ft are rendered smaller because they're landing/taking off
					if (plane.getAltitude() < 2000)
					{
						this.planeNormalCur = this.planeNormal
								.getScaledCopy((float) (1 + ((plane.getSize() - 2.5f + (float) plane
										.getAltitude() / 1000)) / 5));
					}
					else
					{
						this.planeNormalCur = this.planeNormal
								.getScaledCopy(1 + ((((float) (plane.getSize())) - 1) / 5));
					}
					this.planeNormalCur
							.setRotation((float) plane.getBearing() - 90);
					
					this.planeNormalCur.drawCentered((float) plane.getX(),
							(float) plane.getY());
				}
				
				
				// Ram- Reviews list of planes in airspace; if they need
				// landing...:
				// Highlights approach, Renders all planes that need landing as
				// green
				// Not currently selected plane rendered flashing green on odd
				// seconds
				landingApproachAreaDrawn = false;
				if (plane.equals(this.currentGame.getCurrentPlane()))
				{
					if (this.currentGame.getCurrentPlane().isNeedsToLand() == true
							&& landingApproachAreaDrawn == false)
					{
						landingApproachArea.draw(400, 344);
						landingApproachAreaDrawn = true;
					}
				}
				//Selected planes should be drawn to proportion (i.e. when selecting an airport plane)
				if (plane.equals(this.currentGame.getCurrentPlane()))
				{
					if (plane.getAltitude() < 2000)
					{
						this.planeSelectedCur = this.planeSelected
								.getScaledCopy((float) (1 + ((plane.getSize() - 2.5f + (float) plane
										.getAltitude() / 1000)) / 5));
					}
					else
					{
						this.planeSelectedCur = this.planeSelected
								.getScaledCopy(1 + ((((float) (plane.getSize())) - 1) / 5));
					}
					this.planeSelectedCur.setRotation((float) plane
							.getBearing() - 90);
					this.planeSelectedCur.drawCentered((float) plane.getX(),
							(float) plane.getY());
				}
				else
				{
					if (((int) (this.time / 1000)) % 2 == 0)
						{
						if (plane.isNeedsToLand() == true)
						{
							this.planeNeedsLandingCur = this.planeNeedsLanding
									.getScaledCopy(1 + ((((float) (plane
											.getSize())) - 1) / 5));
							this.planeNeedsLandingCur.setRotation((float) plane
									.getBearing() - 90);
							this.planeNeedsLandingCur.drawCentered(
									(float) plane.getX(), (float) plane.getY());
						}
					}
				}
				
				// Render plane's altitude. It doesn't render when planes are landed waiting to take off
				if (plane.getVelocity() > 0)
				{
					g.drawString(plane.getAltitude() + " ft",
							(float) plane.getX(), (float) plane.getY() + 15);
				}

				// Render Landing Information above flight
				if (plane.isNeedsToLand()
						&& !plane.equals(currentGame.getCurrentPlane())
						&& !currentGame.getAirport().isPlaneLanding())
				{
					g.drawString("Land Me!", (float) (plane.getX() - 5),
							(float) (plane.getY() - 30));
				}
				else if (plane.isNeedsToLand()
						&& currentGame.getAirport().isPlaneLanding())
				{
					g.drawString("Wait to Land Me!",
							(float) (plane.getX() - 5),
							(float) (plane.getY() - 30));
				}
				else if (plane.isNeedsToLand() && plane.getAltitude() > 2000)
				{
					g.drawString("Lower Me!", (float) (plane.getX() - 5),
							(float) (plane.getY() - 30));
				}
				else if (plane.isNeedsToLand()
						&& plane.getAltitude() <= 2000
						&& plane.getBearing() <= 225
						&& plane.getBearing() >= 135
						&& currentGame
								.getAirport()
								.getLandingApproachArea()
								.contains((float) plane.getX(),
										(float) plane.getY()))
				{
					g.drawString("'L' to Land", (float) (plane.getX() - 5),
							(float) (plane.getY() - 30));
				}

				else if (plane.isNeedsToLand() && plane.getAltitude() <= 2000)
				{
					g.drawString("Perfect Height!", (float) (plane.getX() - 5),
							(float) (plane.getY() - 30));
				}
				
				else if (plane.isNeedsToTakeOff() && (!currentGame.isTakeOffPenalty())){
					g.drawString("'T' to Takeoff!", 1115, 555);
				}
			}
			
			// Draws ExitPoints
			for (int i = 0; i < this.currentGame.getListOfExitPoints().size(); i++)
			{
				if (this.currentGame.getCurrentPlane() != null)
				{
					if (this.currentGame
							.getCurrentPlane()
							.getFlightPlan()
							.getCurrentRoute()
							.indexOf(
									this.currentGame.getListOfExitPoints().get(
											i)) == 0)
					{	//Draw the exitpoint when the selected flight has it as its next point in the plan
						this.waypointNext.drawCentered((int) this.currentGame
								.getListOfExitPoints().get(i).getX(),
								(int) this.currentGame.getListOfExitPoints()
								.get(i).getY());
					}
					else
					{  
						//Draw the exitpoint properly for the selected flight
						this.waypointLast.drawCentered((int) this.currentGame
								.getListOfExitPoints().get(i).getX(),
								(int) this.currentGame.getListOfExitPoints()
								.get(i).getY());
					}
				}
				else
				{	
					//Draw the exitpoints normally if no plane is selected
					this.waypointLast.drawCentered((int) this.currentGame
							.getListOfExitPoints().get(i).getX(),
							(int) this.currentGame.getListOfExitPoints().get(i)
							.getY());
				}
			}
			
			// Draws Waypoints
			for (int i = 0; i < this.currentGame.getListOfWaypoints().size(); i++)
			{
				if (this.currentGame.getCurrentPlane() != null)
				{
					if (this.currentGame.getCurrentPlane().getFlightPlan()
							.getCurrentRoute().size() > 0)
					{
						if (this.currentGame.getCurrentPlane().getFlightPlan()
								.getCurrentRoute().get(0) == this.currentGame
								.getListOfWaypoints().get(i))
						{
							// Highlights the next waypoint
							this.waypointNext.drawCentered(
									(int) this.currentGame.getListOfWaypoints()
									.get(i).getX(),
									(int) this.currentGame.getListOfWaypoints()
									.get(i).getY());
						}
						else
						{
							// Draws all other waypoints normally
							this.waypointNormal.drawCentered(
									(int) this.currentGame.getListOfWaypoints()
									.get(i).getX(),
									(int) this.currentGame.getListOfWaypoints()
									.get(i).getY());
						}
					}
					else
					{
						// Draw exitpoint 
						this.waypointNormal.drawCentered((int) this.currentGame
								.getListOfWaypoints().get(i).getX(),
								(int) this.currentGame.getListOfWaypoints()
								.get(i).getY());

					}

				}
				else
				{
					// Draw all waypoints normally when there's no selected plane
					this.waypointNormal.drawCentered((int) this.currentGame
							.getListOfWaypoints().get(i).getX(),
							(int) this.currentGame.getListOfWaypoints().get(i)
							.getY());
				}
			}

			
			// Draw arrows on top of the waypoints to give clues about the flight plan
			if (this.currentGame.getCurrentPlane() != null)
			{
				// Go through all the waypoints of the selected flight
				for (int j = 0; j < this.currentGame.getCurrentPlane()
						.getFlightPlan().getCurrentRoute().size() - 1; j++)
				{
					int headingToWaypoint;
					
					// Differences between X and Y coordinates of waypoints
					double deltaY = this.currentGame.getCurrentPlane()
							.getFlightPlan().getCurrentRoute().get(j + 1)
							.getY()
							- this.currentGame.getCurrentPlane()
									.getFlightPlan().getCurrentRoute().get(j)
									.getY();
					double deltaX = this.currentGame.getCurrentPlane()
							.getFlightPlan().getCurrentRoute().get(j + 1)
							.getX()
							- this.currentGame.getCurrentPlane()
									.getFlightPlan().getCurrentRoute().get(j)
									.getX();
					
					// Find the orientation of the arrow
					headingToWaypoint = (int) Math.round(Math.toDegrees(Math
							.atan2(deltaY, deltaX)));
					
					// Draw rotated arrow
					this.waypointArrow.setRotation(headingToWaypoint - 90);
					this.waypointArrow.drawCentered((int) this.currentGame
							.getCurrentPlane().getFlightPlan()
							.getCurrentRoute().get(j).getX(),
							(int) this.currentGame.getCurrentPlane()
									.getFlightPlan().getCurrentRoute().get(j)
									.getY());
					
					// Draw the arrows for the exit points properly so they do not go off screen
					if(j == this.currentGame.getCurrentPlane()
						.getFlightPlan().getCurrentRoute().size() - 2)
					{
						int exitPointX =(int)this.currentGame.getCurrentPlane()
								.getFlightPlan().getCurrentRoute().get(j+1).getX();
						int exitPointY =(int)this.currentGame.getCurrentPlane()
								.getFlightPlan().getCurrentRoute().get(j+1).getY();
						if(exitPointX == 0)
						{
							this.waypointArrow.drawCentered(exitPointX + 10, exitPointY);
						}
						else if(exitPointX == this.getWindowWidth())
						{
							this.waypointArrow.drawCentered(exitPointX-10, exitPointY);
						}
						else if(exitPointY == 0)
						{
							this.waypointArrow.drawCentered(exitPointX, exitPointY+10);
						}
					}
				}
			}
		}
		else
		{
			// Display the game duration (time)
			g.drawString("Time : "
					+ ((int) this.endTime / 1000 / 60 < 10 ? "0"
							+ (int) (this.endTime / 1000) / 60
							: (int) (this.endTime / 1000) / 60)
					+ ":"
					+ ((int) (this.endTime / 1000) % 60 < 10 ? "0"
							+ (int) (this.endTime / 1000) % 60
							: (int) (this.endTime / 1000) % 60), 1050, 15);
		}

		this.playCheckpointSound();

		// Drawing End Game Screen
		if (this.currentGame.isCollision())
		{
			if (this.currentGame.isEnding())
			{
				// Draw the two colliding planes
				for (Plane plane : this.currentGame.getCollidedPlanes())
				{
					this.planeNormal.setRotation((float) Math.toDegrees(plane
							.getBearing()) - 90);
					this.planeNormal.draw((float) plane.getX(),
							(float) plane.getY());
				}
				// Erase the extrapoints above the waypoints
				display = false;

				new TrueTypeFont(this.fontPrimitive.deriveFont(50f), true)
						.drawString(300f, 200f, "That didn't end well...");
				new TrueTypeFont(this.fontPrimitive.deriveFont(25f), true)
						.drawString(470f, 260f, "Score: "
								+ (int) this.currentGame.getScore());
				new TrueTypeFont(this.fontPrimitive.deriveFont(25f), true)
						.drawString(
								450f,
								310,
								"Return in: "
										+ (int) (5 - ((this.time - this.endTime) / 1000)));
				if (this.time > (this.endTime + (5 * 1000)))
				{
					game.closeRequested();
				}
			}
			else
			{
				this.endTime = this.time;
				this.currentGame.setEnding(true);
			}
		}
	}

	/**
	 * Updates the state
	 * 
	 * @param gameContainer
	 *            the game container holding this state
	 * @param game
	 *            the game running this state
	 * @param delta
	 *            the time change between calls
	 */
	@Override
	public void update(GameContainer gameContainer, StateBasedGame game,
			int delta)
	{

		// Update the time
		this.time += delta;
		if (!currentGameContainer.isPaused())
		{
			currentGame.update(gameContainer, game);
		}
	}

	/**
	 * Handles mouse click events
	 * 
	 * @param button
	 *            the button pressed: 0 = left mouse button, 1 = right mouse
	 *            button
	 * @param x
	 *            the x position of the mouse
	 * @param y
	 *            the y position of the mouse
	 * @param clickCount
	 *            the number of times the mouse was clicked
	 */
	@Override
	public void mouseClicked(int button, int x, int y, int clickCount)
	{
		if (currentGameContainer.isPaused())
		{
			return;
		}
		
		// Get mouse input
		if (!this.currentGame.isEnding())
		{
			// Select plane by left clicking
			if (button == 0)
			{
				Plane clickedPlane;
				clickedPlane = this.selectFlight(x, y);
				if (this.currentGame.getCurrentPlane() != null)
				{
					if (!this.currentGame.getCurrentPlane().isNeedsToLand())
					{
						this.currentGame.removeFromManual(this.currentGame
								.getCurrentPlane());
					}
				}
				
				this.currentGame.setCurrentPlane(clickedPlane);
			}
			
			// Give bearing by right clicking
			else if (button == 1)
			{
				if (this.currentGame.getCurrentPlane() != null)
				{
					// Do not allow change of heading to airport planes
					if (!currentGame.getCurrentPlane().isNeedsToTakeOff())
					{
						this.giveHeadingThroughMouse(
								this.currentGame.getCurrentPlane(), x, y);

					}
				}
			}
		}
	}

	/**
	 * Handles key press events
	 * 
	 * @param key
	 *            the key nuber pressed
	 * @param c
	 *            the key character pressed
	 */
	public void keyPressed(int key, char c)
	{
		// Handle game pausing on p and space
		if (key == 57 || key == 25)
		{
			if (this.currentGameContainer.isPaused())
			{
				this.currentGameContainer.resume();
			}
			else 
			{
				this.currentGameContainer.pause();
			}
		}
	}

	/**
	 * Sets properties which need resetting each time the state is accessed
	 * 
	 * <p>
	 * Specifically, this clears the game so that it restarts when exited and
	 * re-entered.
	 * </p>
	 * 
	 * @param gameContainer
	 *            the game container holding this state
	 * @param game
	 *            the game running this state
	 */
	@Override
	public void leave(GameContainer gameContainer, StateBasedGame game)
			throws SlickException
	{

		this.currentGame.setCollision(true);
		this.currentGame.setEnding(true);

		this.currentGame.setManualPlanes(new ArrayList<Plane>());
		this.currentGame.setCollidedPlanes(new ArrayList<Plane>());

		this.currentGame.setCurrentPlane(null);
	}

	/**
	 * @return the state's unique ID
	 */
	@Override
	public int getID()
	{
		return WindowManager.GAME_STATE;
	}

	// Accessors
	/**
	 * @return the current window width
	 */
	public int getWindowWidth()
	{
		return this.windowWidth;
	}

	/**
	 * @return the current window height
	 */
	public int getWindowHeight()
	{
		return this.windowHeight;
	}

	/**
	 * @return the time
	 */
	public double getTime()
	{
		return this.time;
	}

	/**
	 * @return the time the game ended at
	 */
	public double getEndTime()
	{
		return this.endTime;
	}

	/**
	 * @return the current game
	 */
	public Game getCurrentGame()
	{
		return this.currentGame;
	}

	/**
	 * @return the current map
	 */
	public Image getMap()
	{
		return this.map;
	}

	/**
	 * @return the map for level 1
	 */
	public Image getMap1()
	{
		return this.map1;
	}

	/**
	 * @return the map for level 2
	 */
	public Image getMap2()
	{
		return this.map2;
	}

	/**
	 * @return the normal waypoint image
	 */
	public Image getWaypointNormal()
	{
		return this.waypointNormal;
	}

	/**
	 * @return the next waypoint image
	 */
	public Image getWaypointNext()
	{
		return this.waypointNext;
	}

	/**
	 * @return the exit waypoint image
	 */
	public Image getWaypointExit()
	{
		return this.waypointExit;
	}

	/**
	 * @return the last waypoint image
	 */
	public Image getWaypointLast()
	{
		return this.waypointLast;
	}

	/**
	 * @return the normal plane image
	 */
	public Image getPlaneNormal()
	{
		return this.planeNormal;
	}

	/**
	 * @return the in-use plane image
	 */
	public Image getPlaneNormalCur()
	{
		return this.planeNormalCur;
	}

	/**
	 * @return the selected plane image
	 */
	public Image getPlaneSelected()
	{
		return this.planeSelected;
	}

	/**
	 * @return the in-use selected plane image
	 */
	public Image getPlaneSelectedCur()
	{
		return this.planeSelectedCur;
	}

	/**
	 * @return the plane alert range image
	 */
	public Image getPlaneAlert()
	{
		return this.planeAlert;
	}

	/**
	 * @return the plane collision range image
	 */
	public Image getPlaneAlertMax()
	{
		return this.planeAlertMax;
	}

	/**
	 * @return the base font
	 */
	public Font getFontPrimitive()
	{
		return this.fontPrimitive;
	}

	/**
	 * @return the font
	 */
	public TrueTypeFont getFont()
	{
		return this.font;
	}

	/**
	 * @return the font colour
	 */
	public Color getFontColor()
	{
		return this.fontColor;
	}

	/**
	 * @return a reference to the current game container
	 */
	public GameContainer getCurrentGameContainer()
	{
		return this.currentGameContainer;
	}

	// Mutators
	/**
	 * @param windowWidth
	 *            the new window width
	 */
	public void setWindowWidth(int windowWidth)
	{
		this.windowWidth = windowWidth;
	}

	/**
	 * @param windowHeight
	 *            the new window height
	 */
	public void setWindowHeight(int windowHeight)
	{
		this.windowHeight = windowHeight;
	}

	/**
	 * @param time
	 *            the time to set
	 */
	public void setTime(double time)
	{
		this.time = time;
	}

	/**
	 * @param endTime
	 *            the ending time to set
	 */
	public void setEndTime(double endTime)
	{
		this.endTime = endTime;
	}

	/**
	 * @param map
	 *            the map to set
	 */
	public void setMap(Image map)
	{
		this.map = map;
	}

	/**
	 * @param map1
	 *            the map to set for level 1
	 */
	public void setMap1(Image map1)
	{
		this.map1 = map1;
	}

	/**
	 * @param map2
	 *            the map to set for level 2
	 */
	public void setMap2(Image map2)
	{
		this.map2 = map2;
	}

	/**
	 * @param waypointNormal
	 *            the normal waypoint image to set
	 */
	public void setWaypointNormal(Image waypointNormal)
	{
		this.waypointNormal = waypointNormal;
	}

	/**
	 * @param waypointNext
	 *            the next waypoint image to set
	 */
	public void setWaypointNext(Image waypointNext)
	{
		this.waypointNext = waypointNext;
	}

	/**
	 * @param waypointExit
	 *            the exit waypoint image to set
	 */
	public void setWaypointExit(Image waypointExit)
	{
		this.waypointExit = waypointExit;
	}

	/**
	 * @param waypointLast
	 *            the last waypoint image to set
	 */
	public void setWaypointLast(Image waypointLast)
	{
		this.waypointLast = waypointLast;
	}

	/**
	 * @param planeNormal
	 *            the normal plane image to set
	 */
	public void setPlaneNormal(Image planeNormal)
	{
		this.planeNormal = planeNormal;
	}

	/**
	 * @param planeNormalCur
	 *            the in-use plane image to set
	 */
	public void setPlaneNormalCur(Image planeNormalCur)
	{
		this.planeNormalCur = planeNormalCur;
	}

	/**
	 * @param planeSelected
	 *            the selected plane image to set
	 */
	public void setPlaneSelected(Image planeSelected)
	{
		this.planeSelected = planeSelected;
	}

	/**
	 * @param planeSelectedCur
	 *            the in-use selected plane image to set
	 */
	public void setPlaneSelectedCur(Image planeSelectedCur)
	{
		this.planeSelectedCur = planeSelectedCur;
	}

	/**
	 * @param planeAlert
	 *            the plane alert range image to set
	 */
	public void setPlaneAlert(Image planeAlert)
	{
		this.planeAlert = planeAlert;
	}

	/**
	 * @param planeAlertMax
	 *            the plane collision range image to set
	 */
	public void setPlaneAlertMax(Image planeAlertMax)
	{
		this.planeAlertMax = planeAlertMax;
	}

	/**
	 * @param fontPrimitive
	 *            the new font primitive
	 */
	public void setFontPrimitive(Font fontPrimitive)
	{
		this.fontPrimitive = fontPrimitive;
	}

	/**
	 * @param font
	 *            the font derivative to set
	 */
	public void setFont(TrueTypeFont font)
	{
		this.font = font;
	}

	/**
	 * @param fontColor
	 *            the font color to set
	 */
	public void setFontColor(Color fontColor)
	{
		this.fontColor = fontColor;
	}

	/**
	 * @param currentGameContainer
	 *            a reference to the new game container to set
	 */
	public void setCurrentGameContainer(GameContainer currentGameContainer)
	{
		this.currentGameContainer = currentGameContainer;
	}
}
