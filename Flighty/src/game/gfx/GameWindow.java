package game.gfx;

import org.lwjgl.input.Keyboard;
import org.newdawn.slick.AppGameContainer;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.Music;
import org.newdawn.slick.Sound;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.TrueTypeFont;
import org.newdawn.slick.state.BasicGameState;
import org.newdawn.slick.state.StateBasedGame;
import org.newdawn.slick.gui.TextField;

import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.io.IOException;
import java.io.InputStream;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;

import game.struct.Airport;
import game.struct.Game;
import game.struct.Plane;
import game.struct.SaveFile;
import game.struct.SingleplayerGame;

/**
 * GameWindow class provides an interactive game
 */
public class GameWindow extends BasicGameState {
	/** Time between score penalities for not taking off */
	private static final int TAKE_OFF_PENALTY_TIME = 3000;

	/** Time to display the waypoints bonus points */
	private static final int WAYPOINTS_DISPLAY_TIME = 200;

	/** How fast should the planes in need of landing blink **/
	private static final int BLINK_FREQUENCY = 1000;

	/** The width the game is displayed at */
	public int windowWidth;

	/** The height the game is displayed at */
	public int windowHeight;
	
	/** Lowest leaderboard score*/
	
	private long lowScore;

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

	// private Graphics currentGraphics;

	/** The generic TrueType font */
	private TrueTypeFont font;

	private TrueTypeFont pauseFont;

	private TrueTypeFont endFont;

	private TextField textBox;

	/** The colour to display the font in */
	private Color fontColor;

	/** Boolean to stop multiple saves on crashing **/
	private boolean hasSaved = false;

	/** Reference to the game container */
	GameContainer currentGameContainer;

	/** Whether it should display extrapoints taken (e.g. above waypoints) **/
	boolean display = false;

	/** Whether it should display more points for some points (e.g. airport) **/
	boolean morePoints = false;

	/** How long should the waypoints display **/
	double synch = WAYPOINTS_DISPLAY_TIME;

	/** How often should the user be penalised for not taking off **/
	double synchTakeOff = TAKE_OFF_PENALTY_TIME;

	/** Boolean to make sure the points aren't reduce multiple times **/
	boolean reducePoints = true;

	/** Coordinates of last waypoint passed **/
	double prevX;
	double prevY;

	/** Sound used for indicating that current flight has gone through waypoint **/
	Sound checkpointSound;

	/** Background music **/
	Music gameMusic;

	public static SaveFile saveFile = new SaveFile();

	private boolean unlock2 = false;
	private boolean unlock3 = false;

	private boolean isTextBoxIni = false;

	/** The arrow icon */
	private Image arrowIcon;

	/** The shaded arrow icon */
	private Image arrowIconShaded;

	/**
	 * Give heading via cursor
	 * 
	 * @param currentPlane
	 *            - the plane that will execute the commands
	 * @param x
	 *            - the x coordinate of the cursor
	 * @param y
	 *            - the y coordinate of the cursor
	 */
	public void giveHeadingThroughMouse(Plane currentPlane, int x, int y) {
		this.currentGame.getCurrentPlane().setTurningLeft(false);
		this.currentGame.getCurrentPlane().setTurningRight(false);

		// Select the plane
		currentPlane.setManual();


		// Calculate new bearing
		double newBearing = Math.toDegrees(Math.atan2(this.currentGame
				.getCurrentPlane().getY() - y, this.currentGame
				.getCurrentPlane().getX() - x));

		// Reset bearing if less than 360
		if (newBearing < 0) {
			newBearing += 360;
		}

		// Execute the command
		this.currentGame.getCurrentPlane().setTargetBearing(newBearing);
	}

	/**
	 * Method to facilitate the selection of planes via cursors
	 * 
	 * @param x
	 *            - x coordinate of the cursor
	 * @param y
	 *            - y coordinate of the cursor
	 * @return - returns selected plane if any
	 */
	public Plane selectFlight(int x, int y) {
		Plane nearestPlane;

		// Distance from where the user clicked to the nearest plane
		double distanceToPlane;

		// If there is at least one plane in the airspace
		if (this.currentGame.getCurrentPlanes().size() > 0) {
			distanceToPlane = Math.sqrt(Math.pow(x
					- this.currentGame.getCurrentPlanes().get(0).getX(), 2)
					+ Math.pow(
							y
									- this.currentGame.getCurrentPlanes()
											.get(0).getY(), 2));
			nearestPlane = this.currentGame.getCurrentPlanes().get(0);

			// Loop through all the planes and find the nearest one
			for (int i = 0; i < this.currentGame.getCurrentPlanes().size(); i++) {
				if (Math.sqrt(Math.pow(x
						- this.currentGame.getCurrentPlanes().get(i).getX(), 2)
						+ Math.pow(
								y
										- this.currentGame.getCurrentPlanes()
												.get(i).getY(), 2)) < distanceToPlane) {
					distanceToPlane = Math.sqrt(Math.pow(
							x
									- this.currentGame.getCurrentPlanes()
											.get(i).getX(), 2)
							+ Math.pow(y
									- this.currentGame.getCurrentPlanes()
											.get(i).getY(), 2));
					nearestPlane = this.currentGame.getCurrentPlanes().get(i);
				}

				// Checks if the nearest plane is close enough to be considered
				// for picking
				if (distanceToPlane <= 55) {
					if (nearestPlane.equals(this.currentGame.getCurrentPlane())) {
						return null;
					} else {
						// The selected plane must not be landing or taking off
						if (!nearestPlane.isLanding()
								&& !nearestPlane.isTakingOff()) {
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
			throws SlickException {
		// Setup input
		this.setInput(gameContainer.getInput());
		gameContainer.setAlwaysRender(true);
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
		InputStream arrowStream = this.getClass().getResourceAsStream(
				"/resources/other/ArrowR.png");
		InputStream arrowShadedStream = this.getClass()
				.getResourceAsStream("resources/other/ArrowB.png");
		
		this.arrowIcon = new Image(arrowStream, "Arrow Image", false);
		this.arrowIconShaded = new Image(arrowShadedStream,
				"Arrow Shaded Image", false);
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
		InputStream planeNeedsLandingStream = this.getClass()
				.getResourceAsStream("/resources/planes/PlaneNeedsLanding.png");

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

		try {
			InputStream fontStream = getClass().getResourceAsStream(
					"/resources/fonts/8BitWonder.ttf");
			Font newFont = Font.createFont(Font.TRUETYPE_FONT, fontStream);
			GraphicsEnvironment.getLocalGraphicsEnvironment().registerFont(
					newFont);
			this.fontPrimitive = newFont;
		} catch (Exception e) {
			e.printStackTrace();
			this.fontPrimitive = new Font(Font.SERIF, Font.PLAIN, 12);
		}
		this.endFont = new TrueTypeFont(this.fontPrimitive.deriveFont(
				Font.CENTER_BASELINE, 20.0f), false);
		this.fontPrimitive = new Font("Lucida Sans", Font.PLAIN, 12);
		this.font = new TrueTypeFont(this.fontPrimitive, true);
		this.pauseFont = new TrueTypeFont(this.fontPrimitive.deriveFont(15f),
				true);

		// Initialise Waypoint Sound
		checkpointSound = new Sound("resources/music/checkpointSound.ogg");

		// Initialise music
		gameMusic = new Music(
				"resources/music/Galavanting_Through_Low_Rez_Forests.ogg");
		gameMusic.loop();

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
			throws SlickException {
		this.currentGameContainer = gameContainer;

		time = 0;

		// Screen size
		this.windowWidth = 1200;
		this.windowHeight = 600;

		((AppGameContainer) gameContainer).setDisplayMode(this.windowWidth,
				this.windowHeight, false);

		this.unlock2 = saveFile.getLevel2Unlock();
		this.unlock3 = saveFile.getLevel3Unlock();

		if (((WindowManager) game).getCurrentLevel() == 1) {
			// Play level 1
			try {
				this.currentGame = new SingleplayerGame(50, 100, 0);
			} catch (NoSuchAlgorithmException | IOException e) {
				e.printStackTrace();
			}
			this.map = this.map1;
			this.fontColor = Color.white;
			this.currentGame.setSpeedDifficulty(0.5);
			this.currentGame.setSpawnRate(12);
			this.currentGame.setSpawnCount(1);
		} else if (((WindowManager) game).getCurrentLevel() == 2) {
			// Play level 2
			try {
				this.currentGame = new SingleplayerGame(70, 100, 0);
			} catch (NoSuchAlgorithmException | IOException e) {
				e.printStackTrace();
			}
			this.map = this.map1;
			this.fontColor = Color.white;
			this.currentGame.setSpeedDifficulty(0.5);
			this.currentGame.setSpawnRate(4);
			this.currentGame.setSpawnCount(1);
		} else if (((WindowManager) game).getCurrentLevel() == 3) {
			// Play level 3
			try {
				this.currentGame = new SingleplayerGame(70, 100, 0);
			} catch (NoSuchAlgorithmException | IOException e) {
				e.printStackTrace();
			}
			this.map = this.map1;
			this.fontColor = Color.white;
			this.currentGame.setSpeedDifficulty(0.5);
			this.currentGame.setSpawnRate(2);
			this.currentGame.setSpawnCount(1);
		}

	}

	/**
	 * Plays a sound at each checkpoint if the plane is selected
	 */
	public void playCheckpointSound() {
		if (this.currentGame.getCurrentPlane() != null
				&& this.currentGame.getCurrentPlane().getFlightPlan()
						.getCurrentRoute().size() != 0
				&& this.currentGame.getCurrentPlane().checkIfFlightAtWaypoint(
						this.currentGame.getCurrentPlane().getFlightPlan()
								.getCurrentRoute().get(0), currentGame)) {
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
	 * @throws SlickException
	 */
	@Override
	public void render(GameContainer gameContainer, StateBasedGame game,
			Graphics g) throws SlickException {
		// Draw the game map
		this.map.draw(0, 0, this.windowWidth, this.windowHeight);

		// Setup the font
		g.setAntiAlias(true);
		g.setFont(this.font);
		g.setColor(this.fontColor);

		if (!this.currentGame.isEnding()) {

			// Display the Game Information
				g.drawString("Time : "
						+ ((int) this.time / 1000 / 60 < 10 ? "0"
								+ (int) (this.time / 1000) / 60
								: (int) (this.time / 1000) / 60)
						+ ":"
						+ ((int) (this.time / 1000) % 60 < 10 ? "0"
								+ (int) (this.time / 1000) % 60
								: (int) (this.time / 1000) % 60), 1050, 15);

			g.drawString(
					"Score : "
							+ ((int) (this.currentGame.getScore().getScore()))
							+ " pts", 1050, 35);
			g.drawString(
					"Multiplier :"
							+ ((int) (this.currentGame.getScore()
									.getMultiplier())), 1050, 55);
			g.drawString("Pause/Controls: P ", 1050, 75);

			// Loop through all the planes
			for (Plane plane : this.currentGame.getCurrentPlanes()) {
				// Sets to display the number of points gained above the passed
				// waypoint
				if (plane.getFlightPlan().getCurrentRoute().size() > 1) {
					if (plane.checkIfFlightAtWaypoint(plane.getFlightPlan()
							.getCurrentRoute().get(0), this.currentGame)) {

						// If plane is at the runway, more points apply
						if (plane.getFlightPlan().getCurrentRoute().get(0) == currentGame
								.getAirport().getEndOfRunway()) {
							morePoints = true;
						}

						// Saves the passed waypoint coordinates to know where
						// to display the text
						prevX = plane.getFlightPlan().getCurrentRoute().get(0)
								.getX();
						prevY = plane.getFlightPlan().getCurrentRoute().get(0)
								.getY();

						// How long it should display the text for
						synch = WAYPOINTS_DISPLAY_TIME;

						display = true;
					}
					// Renders the bonus points
					if (display && synch > 0) {
						// If it's a special waypoint, render how many points
						// were won
						if (morePoints) {
							g.drawString(
									"+"
											+ Integer
													.toString(this
															.getCurrentGame()
															.getScore()
															.getMultiplier() * 10),
									(float) prevX - 8, (float) prevY - 30);
							morePoints = synch <= 1 ? false : true;
						}
						// Beginning of runway provides no extra points
						else if (plane
								.getFlightPlan()
								.getCurrentRoute()
								.get(0)
								.equals(currentGame.getAirport()
										.getBeginningOfRunway())) {
							synch = 0;
						}
						// If it's not a special waypoint, render how many
						// points were won
						else {
							g.drawString(
									"+"
											+ Integer
													.toString(this
															.getCurrentGame()
															.getScore()
															.getMultiplier() * 5),
									(float) prevX - 8, (float) prevY - 30);
						}

						synch--;
					}
					// Don't display extra points if there were not won any
					else {
						display = false;
					}
				}

				// Plane has been landed for too long, and the user is prompted
				// to take it off
				if (currentGame.isTakeOffPenalty()) {
					// Alert message to inform the user that the plane should be
					// taken off
					g.drawString("Take me off", 1122, 555);
					synchTakeOff--;

					// Points deduced from the user if the plane is still landed
					if (synchTakeOff < TAKE_OFF_PENALTY_TIME / 5) {
						g.setColor(Color.red);
						g.drawString("-" + 10
								* currentGame.getScore().getMultiplier(), 1150,
								570);
						g.setColor(Color.white);

						// Making sure the subtraction of points was done only
						// once
						if (reducePoints) {
							currentGame
									.getScore()
									.planeLeftAirspaceOrWaitingToTakeOffMinusScore();

							// Don't reduce more points for the same penalty
							reducePoints = false;
						}

						// Decrement the timer
						synchTakeOff--;

						// Counter for reducing points restarted
						if (synchTakeOff < 0) {
							reducePoints = true;
							synchTakeOff = TAKE_OFF_PENALTY_TIME;
						}
					}
				}

				// If plane is within penalty distance, apply alert images
				if (plane.getAlertStatus()) {
					// Separation violation area
					this.planeAlert.getScaledCopy(
							this.currentGame.getPenaltyDistance(),
							this.currentGame.getPenaltyDistance())
							.drawCentered((float) plane.getX(),
									(float) plane.getY());

					// Collision area
					this.planeAlertMax.getScaledCopy(
							this.currentGame.getSeparationDistance(),
							this.currentGame.getSeparationDistance())
							.drawCentered((float) plane.getX(),
									(float) plane.getY());
				}

				// Selected plane
				if (plane.equals(this.currentGame.getCurrentPlane())) {
					/*
					 * Active flights in the airspace are reaching at least
					 * 2000ft altitude otherwise it means they're landing,
					 * taking off, or landed
					 */
					if (plane.getAltitude() >= 2000) {
						this.planeSelectedCur = this.planeSelected
								.getScaledCopy(1 + ((((float) (plane.getSize())) - 1) / 5));
					}
					// Draw the plane white when selected
					this.planeSelectedCur.setRotation((float) plane
							.getBearing() - 90);
					this.planeSelectedCur.drawCentered((float) plane.getX(),
							(float) plane.getY());
				} else {
					// Planes under 2000ft are rendered smaller because they're
					// landing/taking off
					if (plane.getAltitude() < 2000) {
						// Render unselected planes that are taking-off/landing
						// with a variable size
						this.planeNormalCur = this.planeNormal
								.getScaledCopy((float) (1 + ((plane.getSize() - 2.5f + (float) plane
										.getAltitude() / 1000)) / 5));
					} else {
						this.planeNormalCur = this.planeNormal
								.getScaledCopy(1 + ((((float) (plane.getSize())) - 1) / 5));
					}

					// Render unselected planes
					this.planeNormalCur
							.setRotation((float) plane.getBearing() - 90);

					this.planeNormalCur.drawCentered((float) plane.getX(),
							(float) plane.getY());
				}

				/*
				 * Reviews list of planes in airspace; if they need landing...:
				 * Highlights approach, Renders all planes that need landing as
				 * green Not currently selected plane rendered flashing green on
				 * odd seconds
				 */
				landingApproachAreaDrawn = false;
				if (plane.equals(this.currentGame.getCurrentPlane())) {
					if (this.currentGame.getCurrentPlane().getNeedsToLand() == true
							&& landingApproachAreaDrawn == false) {
						landingApproachArea.draw(
								Airport.getBeginningOfRunwayX()
										+ Airport.getTriangleSize()[0],
								Airport.getRunwayY()
										- (Airport.getTriangleSize()[1] / 2),
								-Airport.getTriangleSize()[0],
								Airport.getTriangleSize()[1]);
						landingApproachAreaDrawn = true;
					}
				}
				// Selected planes should be drawn to proportion (i.e. when
				// selecting an airport plane)
				if (plane.equals(this.currentGame.getCurrentPlane())) {
					if (plane.getAltitude() < 2000) {
						// Selected planes that are landing / taking off are
						// smaller
						this.planeSelectedCur = this.planeSelected
								.getScaledCopy((float) (1 + ((plane.getSize() - 2.5f + (float) plane
										.getAltitude() / 1000)) / 5));
					} else {
						this.planeSelectedCur = this.planeSelected
								.getScaledCopy(1 + ((((float) (plane.getSize())) - 1) / 5));
					}
				}
				// If plane needs to land, make the plane blink
				else {
					// Every number of frames, blink
					if (((int) (this.time / BLINK_FREQUENCY)) % 2 == 0) {
						if (plane.getNeedsToLand() == true) {
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

				// Render plane's altitude. It doesn't render when planes are
				// landed waiting to take off
				if (plane.getVelocity() > 0) {
					g.drawString(plane.getAltitude() + " ft",
							(float) plane.getX(), (float) plane.getY() + 15);
				}

				/* Render Landing Information above flight */

				// If plane needs to land and is not selected
				if (plane.getNeedsToLand()
						&& !plane.equals(currentGame.getCurrentPlane())
						&& !currentGame.getAirport().isPlaneLanding()) {
					g.drawString("Land Me!", (float) (plane.getX() - 5),
							(float) (plane.getY() - 30));
				}

				// If plane needs to land, but there is another plane landing at
				// the same time
				else if (plane.getNeedsToLand()
						&& currentGame.getAirport().isPlaneLanding()) {
					g.drawString("Wait to Land Me!",
							(float) (plane.getX() - 5),
							(float) (plane.getY() - 30));
				}

				// If plane is selected, but it's not at the landing height
				else if (plane.getNeedsToLand() && plane.getAltitude() > 2000) {
					g.drawString("Lower Me!", (float) (plane.getX() - 5),
							(float) (plane.getY() - 30));
				}

				// If plane is selected, at the right altitude, and within the
				// landing zone
				else if (plane.getNeedsToLand()
						&& plane.getAltitude() <= 2000
						&& plane.getBearing() <= 225
						&& plane.getBearing() >= 135
						&& currentGame
								.getAirport()
								.getLandingApproachArea()
								.contains((float) plane.getX(),
										(float) plane.getY())) {
					g.drawString("'L' to Land", (float) (plane.getX() - 5),
							(float) (plane.getY() - 30));
				}

				// If plane is selected, at right height, but not within the
				// landing zone
				else if (plane.getNeedsToLand() && plane.getAltitude() <= 2000) {
					g.drawString("Perfect Height!", (float) (plane.getX() - 5),
							(float) (plane.getY() - 30));
				}

				// If plane needs to take off, and it didn't violate the
				// allowance threshold of sitting landed
				else if (plane.getNeedsToTakeOff()
						&& (!currentGame.isTakeOffPenalty())) {
					g.drawString("'T' to Takeoff!", 1115, 555);
				}
			}

			// Draws ExitPoints
			for (int i = 0; i < this.currentGame.getListOfExitPoints().size(); i++) {

				if (this.currentGame.getCurrentPlane() != null) {
					// Draw the exitpoint when the selected flight has it as its
					// next point in the plan
					if (this.currentGame
							.getCurrentPlane()
							.getFlightPlan()
							.getCurrentRoute()
							.indexOf(
									this.currentGame.getListOfExitPoints().get(
											i)) == 0) {
						this.waypointNext.drawCentered((int) this.currentGame
								.getListOfExitPoints().get(i).getX(),
								(int) this.currentGame.getListOfExitPoints()
										.get(i).getY());
					}
					// Draw the exitpoint properly for the selected flight
					else {
						this.waypointLast.drawCentered((int) this.currentGame
								.getListOfExitPoints().get(i).getX(),
								(int) this.currentGame.getListOfExitPoints()
										.get(i).getY());
					}
				} else {
					// Draw the exitpoints normally if no plane is selected
					this.waypointLast.drawCentered((int) this.currentGame
							.getListOfExitPoints().get(i).getX(),
							(int) this.currentGame.getListOfExitPoints().get(i)
									.getY());
				}
			}

			// Go through all the waypoint to draw them
			for (int i = 0; i < this.currentGame.getListOfWaypoints().size(); i++) {
				// If a plane is selected
				if (this.currentGame.getCurrentPlane() != null) {
					// If the selected plane has at least a point in his flight
					// plan
					if (this.currentGame.getCurrentPlane().getFlightPlan()
							.getCurrentRoute().size() > 0) {
						// If the next waypoint is among the airspace waypoints
						if (this.currentGame.getCurrentPlane().getFlightPlan()
								.getCurrentRoute().get(0) == this.currentGame
								.getListOfWaypoints().get(i)) {
							// Highlights the next waypoint
							this.waypointNext.drawCentered(
									(int) this.currentGame.getListOfWaypoints()
											.get(i).getX(),
									(int) this.currentGame.getListOfWaypoints()
											.get(i).getY());
						} else {
							// Draws all other waypoints normally
							this.waypointNormal.drawCentered(
									(int) this.currentGame.getListOfWaypoints()
											.get(i).getX(),
									(int) this.currentGame.getListOfWaypoints()
											.get(i).getY());
						}
					} else {
						// Draws all other waypoints normally
						this.waypointNormal.drawCentered((int) this.currentGame
								.getListOfWaypoints().get(i).getX(),
								(int) this.currentGame.getListOfWaypoints()
										.get(i).getY());
					}
				} else {
					// Draw all waypoints normally when there's no selected
					// plane
					this.waypointNormal.drawCentered((int) this.currentGame
							.getListOfWaypoints().get(i).getX(),
							(int) this.currentGame.getListOfWaypoints().get(i)
									.getY());
				}
			}

			// Draw arrows on top of the waypoints to give clues about the
			// flight plan
			if (this.currentGame.getCurrentPlane() != null) {
				// Go through all the waypoints of the selected flight
				for (int j = 0; j < this.currentGame.getCurrentPlane()
						.getFlightPlan().getCurrentRoute().size() - 1; j++) {
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

					// Draw the arrows for the exit points properly so they do
					// not go off screen
					if (j == this.currentGame.getCurrentPlane().getFlightPlan()
							.getCurrentRoute().size() - 2) {
						int exitPointX = (int) this.currentGame
								.getCurrentPlane().getFlightPlan()
								.getCurrentRoute().get(j + 1).getX();
						int exitPointY = (int) this.currentGame
								.getCurrentPlane().getFlightPlan()
								.getCurrentRoute().get(j + 1).getY();

						// Set Rotation for the left hand side exit point arrow
						if (exitPointX == 0) {
							this.waypointArrow.setRotation(90);
							this.waypointArrow.drawCentered(exitPointX + 10,
									exitPointY);
						}

						// Set Rotation for the right hand side exit point arrow
						else if (exitPointX == this.getWindowWidth()) {
							this.waypointArrow.setRotation(270);
							this.waypointArrow.drawCentered(exitPointX - 10,
									exitPointY);
						}

						// Set Rotation for top exit point arrow
						else if (exitPointY == 0) {
							this.waypointArrow.setRotation(180);
							this.waypointArrow.drawCentered(exitPointX,
									exitPointY + 10);
						}
					}
				}
			}
		} else {
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
		// Play the sound for going though a waypoint
		this.playCheckpointSound();

		// If the game is still running
		if (!this.currentGame.isEnding()) {
			// Drawing Pause Screen if in pause menu.
			if (currentGameContainer.isPaused()) {
				g.setFont(this.pauseFont);
				g.drawString("PAUSE", this.getWindowWidth() / 2 - 30,
						this.getWindowHeight() / 2 - 100);

				g.drawString(
						"Steer a plane by selecting it and using the key arrows/ right click",
						this.getWindowWidth() / 2 - 30 - 200,
						this.getWindowHeight() / 2 - 65);

				g.drawString(
						"Land a plane by lowering it to 2000ft when it needs to land",
						this.getWindowWidth() / 2 - 30 - 190,
						this.getWindowHeight() / 2 - 25);

				g.drawString("and pressing L when in the airport zone",
						this.getWindowWidth() / 2 - 30 - 130,
						this.getWindowHeight() / 2 - 5);

				g.drawString(
						"Take off a plane by selecting an airport plane and pressing T",
						this.getWindowWidth() / 2 - 30 - 190,
						this.getWindowHeight() / 2 + 35);

				g.drawString("Press S to turn music on/off",
						this.getWindowWidth() / 2 - 30 - 80,
						this.getWindowHeight() / 2 + 70);

				g.drawString("Press P to unpause",
						this.getWindowWidth() / 2 - 30 - 50,
						this.getWindowHeight() / 2 + 100);
			}

		}

		/* Setting up the game over screen */
		// If the planes collided
		if (this.currentGame.isCollision()) {
			// If the game is ending
			if (this.currentGame.isEnding()) {
				// Draw the two collided planes rotated a bit so it looks like a
				// crash
				for (Plane plane : this.currentGame.getCollidedPlanes()) {
					this.planeNormal.setRotation((float) Math.toDegrees(plane
							.getBearing()) - 90);
					this.planeNormal.draw((float) plane.getX(),
							(float) plane.getY());
				}
				// Erase the extrapoints above the waypoints
				display = false;

				// Draw the game over text
				endFont.drawString(300, 200, "That didn't end well");
				endFont.drawString(400, 260, "Score: "+ (int) this.currentGame.getScore().getScore());

				int textHeight = this.font.getHeight();

				this.checkForSelection(gameContainer, game);

				this.arrowIconShaded.draw(247, gameContainer.getHeight() - 48
						- (textHeight / 4), 45, 35);

				this.arrowIcon.draw(245, gameContainer.getHeight() - 50
						- (textHeight / 4), 45, 35);
				
				if (saveFile.getLevel2UnlockScore() <= this.currentGame
						.getScore().getScore()
						&& ((WindowManager) game).getCurrentLevel() == 1
						&& unlock2 == false) {
					endFont.drawString(420, 350, "Level 2 Unlocked");
					if (!hasSaved) {
						saveFile.setLevel2Unlock(true);
						saveFile.saveStats();
						this.hasSaved = true;
					}
				} else if (saveFile.getLevel3UnlockScore() <= this.currentGame
						.getScore().getScore()
						&& ((WindowManager) game).getCurrentLevel() == 2
						&& unlock3 == false) {
					endFont.drawString(420, 350, "Level 3 Unlocked");
					if (!hasSaved) {
						saveFile.setLevel3Unlock(true);
						saveFile.saveStats();
						this.hasSaved = true;
					}
				}

				// Manages leaderboard entries

					// initializes text box

					if (isTextBoxIni == false) {
						textBox = new TextField(currentGameContainer, endFont,
								300, 350, 430, 50);
						textBox.setBorderColor(Color.black);
						textBox.setBackgroundColor(Color.white);
						textBox.setTextColor(Color.orange);
						textBox.setConsumeEvents(true);
						textBox.setAcceptingInput(true);
						textBox.setMaxLength(30);
						isTextBoxIni = true;
						WindowManager.leaderBoard.isConnected();
						lowScore = saveFile.getLowestScore();
					}
					
					
					
					//creates a text box to enter your name:
					if(LeaderBoard.connected){
						if(lowScore < currentGame.getScore().getScore()){
							endFont.drawString(300, 300, "Enter your name to the leaderboard");
							textBox.render(currentGameContainer, g);

							if (Keyboard.getEventKey() == Keyboard.KEY_RETURN && textBox.getText() != "") {
								saveFile.addLeaderboardScore(textBox.getText(),
										currentGame.getScore().getScore());
								textBox.setText("");
								game.enterState(WindowManager.MAIN_MENU_STATE);

							}else{ 
								if(Keyboard.getEventKey() == Keyboard.KEY_RETURN){
									game.enterState(WindowManager.MAIN_MENU_STATE);
								}
							}
						}
						
					}else {
						new TrueTypeFont(this.fontPrimitive.deriveFont(25f),
								true).drawString(305f, 300f,"No internet connection");
					}
					
				}// if the planes collided but the ending has not yet been set
				else {
					// Stop the timer
					this.endTime = this.time;
					// End the game
					this.currentGame.setEnding(true);
				}
			}
		}


	/**
	 * Updates the game state
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
			int delta) {

		// Update the time
		this.time += delta;
		if (!currentGameContainer.isPaused()) {
			try {
				currentGame.update(gameContainer, game);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	private void checkForSelection(GameContainer gameContainer,
			StateBasedGame game) {
		int x = gameContainer.getInput().getMouseX();
		int y = gameContainer.getInput().getMouseY();
		boolean clicked = gameContainer.getInput().isMousePressed(0);

		String mainMenuText = "Main Menu";

		int mainMenuWidth = this.endFont.getWidth(mainMenuText);
		int textHeight = this.endFont.getHeight();

		Color mainMenuColor = Color.orange;

		// If text is hovered
		if ((x >= (50 - 25)) && (y >= (gameContainer.getHeight() - 50 - 25))
				&& (x <= (50 - 25) + mainMenuWidth + 25)
				&& (y <= (gameContainer.getHeight() - 50 + textHeight + 25))) {
			if (clicked) {
				if (isTextBoxIni) {
					saveFile.addLeaderboardScore(textBox.getText(), currentGame
							.getScore().getScore());
					textBox.setText("");
				}
				game.enterState(WindowManager.MAIN_MENU_STATE);
			} else {
				// Change colour on hover
				mainMenuColor = Color.white;
			}
		} else {
			mainMenuColor = Color.orange;
		}
		// Draw the actual text
		endFont.drawString(50 + 2, gameContainer.getHeight() - 50 + 2,
				mainMenuText, Color.black);
		endFont.drawString(50, gameContainer.getHeight() - 50, mainMenuText,
				mainMenuColor);
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
	public void mouseClicked(int button, int x, int y, int clickCount) {
		// If game is paused, don't handle mouse controls
		if (currentGameContainer.isPaused()) {
			return;
		}

		/* Get mouse input */

		// Only get mouse input if the game is not ending
		if (!this.currentGame.isEnding()) {
			// Select plane by left clicking
			if (button == 0) {
				Plane clickedPlane;
				clickedPlane = this.selectFlight(x, y);

				// If there is no plane where the user click, deselect the
				// current plane
				if (this.currentGame.getCurrentPlane() != null) {
					if (!this.currentGame.getCurrentPlane().getNeedsToLand()) {
						/*
						 * When a plane gets deselected, it is removed from
						 * manual control, and it retakes the automatic control
						 */
						this.currentGame.removeFromManual(this.currentGame
								.getCurrentPlane());
						this.currentGame.getCurrentPlane().markForSyncing();
					}
				}

				this.currentGame.setCurrentPlane(clickedPlane);
			}

			// Give bearing by right clicking
			else if (button == 1) {
				// If a plane is selected
				if (this.currentGame.getCurrentPlane() != null) {
					// Do not allow change of heading to airport planes
					if (!currentGame.getCurrentPlane().getNeedsToTakeOff()) {
						this.giveHeadingThroughMouse(
								this.currentGame.getCurrentPlane(), x, y);
						this.currentGame.getCurrentPlane().markForSyncing();

					}
				}
			}
		}
	}

	/**
	 * Handles key press events
	 * 
	 * @param key
	 *            the key number pressed
	 * @param c
	 *            the key character pressed
	 */
	public void keyPressed(int key, char c) {
		// If the game is ending, do not take any key input
		if (currentGame.isEnding()) {
			return;
		}

		// Handle game pausing on "P" and space bar
		if (key == 57 || key == 25) {
			// Resume the game if "P" or space bar is pressed and the game is
			// paused
			if (this.currentGameContainer.isPaused()) {
				this.currentGameContainer.resume();
			}

			// Pause the game if the game is not paused and "P" or space bar is
			// pressed
			else {
				this.currentGameContainer.pause();
			}
		}

		/* Handle music options */

		// If the user presses "s" or "S", the music either pauses or resumes
		// looping depending on its state
		if (((c == 's') || (c == 'S')) && (gameMusic.playing())) {
			gameMusic.pause();
		} else if (((c == 's') || (c == 'S')) && (!gameMusic.playing())) {
			gameMusic.loop();
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
			throws SlickException {
		this.currentGameContainer.resume();
		this.currentGame.setCollision(true);
		this.currentGame.setEnding(true);

		this.currentGame.clearManualPlanes();
		this.currentGame.setCollidedPlanes(new ArrayList<Plane>());

		this.currentGame.setCurrentPlane(null);

	}

	/**
	 * @return the state's unique ID
	 */
	@Override
	public int getID() {
		return WindowManager.GAME_STATE;
	}

	// Accessors
	/**
	 * @return the current window width
	 */
	public int getWindowWidth() {
		return this.windowWidth;
	}

	/**
	 * @return the current window height
	 */
	public int getWindowHeight() {
		return this.windowHeight;
	}

	/**
	 * @return the time
	 */
	public double getTime() {
		return this.time;
	}

	/**
	 * @return the time the game ended at
	 */
	public double getEndTime() {
		return this.endTime;
	}

	/**
	 * @return the current game
	 */
	public Game getCurrentGame() {
		return this.currentGame;
	}

	/**
	 * @return the current map
	 */
	public Image getMap() {
		return this.map;
	}

	/**
	 * @return the map for level 1
	 */
	public Image getMap1() {
		return this.map1;
	}

	/**
	 * @return the map for level 2
	 */
	public Image getMap2() {
		return this.map2;
	}

	/**
	 * @return the normal waypoint image
	 */
	public Image getWaypointNormal() {
		return this.waypointNormal;
	}

	/**
	 * @return the next waypoint image
	 */
	public Image getWaypointNext() {
		return this.waypointNext;
	}

	/**
	 * @return the exit waypoint image
	 */
	public Image getWaypointExit() {
		return this.waypointExit;
	}

	/**
	 * @return the last waypoint image
	 */
	public Image getWaypointLast() {
		return this.waypointLast;
	}

	/**
	 * @return the normal plane image
	 */
	public Image getPlaneNormal() {
		return this.planeNormal;
	}

	/**
	 * @return the in-use plane image
	 */
	public Image getPlaneNormalCur() {
		return this.planeNormalCur;
	}

	/**
	 * @return the selected plane image
	 */
	public Image getPlaneSelected() {
		return this.planeSelected;
	}

	/**
	 * @return the in-use selected plane image
	 */
	public Image getPlaneSelectedCur() {
		return this.planeSelectedCur;
	}

	/**
	 * @return the plane alert range image
	 */
	public Image getPlaneAlert() {
		return this.planeAlert;
	}

	/**
	 * @return the plane collision range image
	 */
	public Image getPlaneAlertMax() {
		return this.planeAlertMax;
	}

	/**
	 * @return the base font
	 */
	public Font getFontPrimitive() {
		return this.fontPrimitive;
	}

	/**
	 * @return the font
	 */
	public TrueTypeFont getFont() {
		return this.font;
	}

	/**
	 * @return the font colour
	 */
	public Color getFontColor() {
		return this.fontColor;
	}

	/**
	 * @return a reference to the current game container
	 */
	public GameContainer getCurrentGameContainer() {
		return this.currentGameContainer;
	}

	// Mutators
	/**
	 * @param windowWidth
	 *            the new window width
	 */
	public void setWindowWidth(int windowWidth) {
		this.windowWidth = windowWidth;
	}

	/**
	 * @param windowHeight
	 *            the new window height
	 */
	public void setWindowHeight(int windowHeight) {
		this.windowHeight = windowHeight;
	}

	/**
	 * @param time
	 *            the time to set
	 */
	public void setTime(double time) {
		this.time = time;
	}

	/**
	 * @param endTime
	 *            the ending time to set
	 */
	public void setEndTime(double endTime) {
		this.endTime = endTime;
	}

	/**
	 * @param map
	 *            the map to set
	 */
	public void setMap(Image map) {
		this.map = map;
	}

	/**
	 * @param map1
	 *            the map to set for level 1
	 */
	public void setMap1(Image map1) {
		this.map1 = map1;
	}

	/**
	 * @param map2
	 *            the map to set for level 2
	 */
	public void setMap2(Image map2) {
		this.map2 = map2;
	}

	/**
	 * @param waypointNormal
	 *            the normal waypoint image to set
	 */
	public void setWaypointNormal(Image waypointNormal) {
		this.waypointNormal = waypointNormal;
	}

	/**
	 * @param waypointNext
	 *            the next waypoint image to set
	 */
	public void setWaypointNext(Image waypointNext) {
		this.waypointNext = waypointNext;
	}

	/**
	 * @param waypointExit
	 *            the exit waypoint image to set
	 */
	public void setWaypointExit(Image waypointExit) {
		this.waypointExit = waypointExit;
	}

	/**
	 * @param waypointLast
	 *            the last waypoint image to set
	 */
	public void setWaypointLast(Image waypointLast) {
		this.waypointLast = waypointLast;
	}

	/**
	 * @param planeNormal
	 *            the normal plane image to set
	 */
	public void setPlaneNormal(Image planeNormal) {
		this.planeNormal = planeNormal;
	}

	/**
	 * @param planeNormalCur
	 *            the in-use plane image to set
	 */
	public void setPlaneNormalCur(Image planeNormalCur) {
		this.planeNormalCur = planeNormalCur;
	}

	/**
	 * @param planeSelected
	 *            the selected plane image to set
	 */
	public void setPlaneSelected(Image planeSelected) {
		this.planeSelected = planeSelected;
	}

	/**
	 * @param planeSelectedCur
	 *            the in-use selected plane image to set
	 */
	public void setPlaneSelectedCur(Image planeSelectedCur) {
		this.planeSelectedCur = planeSelectedCur;
	}

	/**
	 * @param planeAlert
	 *            the plane alert range image to set
	 */
	public void setPlaneAlert(Image planeAlert) {
		this.planeAlert = planeAlert;
	}

	/**
	 * @param planeAlertMax
	 *            the plane collision range image to set
	 */
	public void setPlaneAlertMax(Image planeAlertMax) {
		this.planeAlertMax = planeAlertMax;
	}

	/**
	 * @param fontPrimitive
	 *            the new font primitive
	 */
	public void setFontPrimitive(Font fontPrimitive) {
		this.fontPrimitive = fontPrimitive;
	}

	/**
	 * @param font
	 *            the font derivative to set
	 */
	public void setFont(TrueTypeFont font) {
		this.font = font;
	}

	/**
	 * @param fontColor
	 *            the font color to set
	 */
	public void setFontColor(Color fontColor) {
		this.fontColor = fontColor;
	}

	/**
	 * @param currentGameContainer
	 *            a reference to the new game container to set
	 */
	public void setCurrentGameContainer(GameContainer currentGameContainer) {
		this.currentGameContainer = currentGameContainer;
	}
}