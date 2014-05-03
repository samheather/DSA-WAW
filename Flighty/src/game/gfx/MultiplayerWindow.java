package game.gfx;

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

import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.io.IOException;
import java.io.InputStream;
import java.net.UnknownHostException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;

import game.struct.Airport;
import game.struct.Game;
import game.struct.MultiplayerGame;
import game.struct.Plane;
import game.struct.Cloud;

/**
 * GameWindow class provides an interactive game
 */
public class MultiplayerWindow extends BasicGameState {

	/** Time between score penalties for not taking off */
	private static final int TAKE_OFF_PENALTY_TIME = 3000;

	/** Time to display the waypoints bonus points */
	private static final int WAYPOINTS_DISPLAY_TIME = 200;

	/** How fast should the planes in need of landing blink **/
	private static final int BLINK_FREQUENCY = 1000;

	/** Side bar width */
	private static final int sidebarWidth = 150;

	/** Centre line width */
	private static final float divideLineWidth = 2.0f;

	/** The width the game is displayed at */
	public int windowWidth;

	/** The height the game is displayed at */
	public int windowHeight;

	/** The time the game has been running for */
	private double time;

	/** The time the game ended at */
	private double endTime;

	private Game currentGame;

	/** The map */
	private Image map;

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

	/** Background image for side bar */
	private Image sidebarBackground;

	/** The images shown when a plane needs to land **/
	private Image planeNeedsLanding;
	private Image planeNeedsLandingCur;

	/**
	 * Images shown when a plane is coming in to land, and a boolean to track
	 * when this image has been displayed
	 **/
	private Image landingApproachArea;
	private boolean landingApproachAreaDrawn;

	/** The plane alert range image */
	private Image planeAlert;

	/** The plane collision range image */
	private Image planeAlertMax;

	// Clouds
	/** Cloud images **/
	private ArrayList<Image> cloudImages;

	/** Array of cloud objects **/
	private ArrayList<Cloud> clouds;

	private boolean cloudsInit = true;

	private boolean cloudsApeared = false;

	/** The Java font used to generate the fonts used */
	private Font fontPrimitive;

	/** The generic TrueType font */
	private TrueTypeFont font;

	private TrueTypeFont sidebarFont;

	private TrueTypeFont sidebarFontLarge;

	/** The colour to display the font in */
	private Color fontColor;

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

	// Variables to control auto-pilot off button

	/** Time that autopilot will be disabled for **/
	double autoPilotDuration = 4000;
	double offTime;

	boolean autoPilotOff = false;

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
		currentGame.getCurrentPlane().setTurningLeft(false);
		currentGame.getCurrentPlane().setTurningRight(false);

		// Select the plane
		currentPlane.setManual();

		// Calculate new bearing
		double newBearing = Math.toDegrees(Math.atan2(currentGame
				.getCurrentPlane().getY() - y, currentGame.getCurrentPlane()
				.getX() - x));

		// Reset bearing if less than 360
		if (newBearing < 0) {
			newBearing += 360;
		}

		// Execute the command
		currentGame.getCurrentPlane().setTargetBearing(newBearing);
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
		if (currentGame.getCurrentPlanes().size() > 0) {
			distanceToPlane = Math
					.sqrt(Math.pow(x
							- currentGame.getCurrentPlanes().get(0).getX(), 2)
							+ Math.pow(y
									- currentGame.getCurrentPlanes().get(0)
											.getY(), 2));
			nearestPlane = currentGame.getCurrentPlanes().get(0);

			// Loop through all the planes and find the nearest one
			for (int i = 0; i < currentGame.getCurrentPlanes().size(); i++) {
				if (Math.sqrt(Math.pow(x
						- currentGame.getCurrentPlanes().get(i).getX(), 2)
						+ Math.pow(y
								- currentGame.getCurrentPlanes().get(i).getY(),
								2)) < distanceToPlane) {
					distanceToPlane = Math.sqrt(Math.pow(x
							- currentGame.getCurrentPlanes().get(i).getX(), 2)
							+ Math.pow(y
									- currentGame.getCurrentPlanes().get(i)
											.getY(), 2));
					nearestPlane = currentGame.getCurrentPlanes().get(i);
				}

				// Checks if the nearest plane is close enough to be considered
				// for picking
				if (distanceToPlane <= 55) {
					if (nearestPlane.equals(currentGame.getCurrentPlane())) {
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
		InputStream sidebarBackgroundStream = this.getClass()
				.getResourceAsStream("/resources/other/Sidebar.png");

		waypointNormal = new Image(waypointNormalStream,
				"Waypoint Normal Image", false);
		waypointNext = new Image(waypointNextStream, "Waypoint Next Image",
				false);
		waypointLast = new Image(waypointLastStream, "Waypoint Last Image",
				false);
		waypointExit = new Image(waypointExitStream, "Waypoint Exit Image",
				false);
		waypointArrow = new Image(waypointArrowStream, "Waypoint Arrow Image",
				false);
		sidebarBackground = new Image(sidebarBackgroundStream,
				"Sidebar Background Image", false);

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
		InputStream mapStream = this.getClass().getResourceAsStream(
				"/resources/maps/Newmap.png");

		this.map = new Image(mapStream, "Map Image", false);

		// Load cloud images
		InputStream cloudStream1 = this.getClass().getResourceAsStream(
				"/resources/clouds/cloud2.png");
		InputStream cloudStream2 = this.getClass().getResourceAsStream(
				"/resources/clouds/cloud2.png");
		InputStream cloudStream3 = this.getClass().getResourceAsStream(
				"/resources/clouds/cloud1.png");
		InputStream cloudStream4 = this.getClass().getResourceAsStream(
				"/resources/clouds/cloud2.png");
		InputStream cloudStream5 = this.getClass().getResourceAsStream(
				"/resources/clouds/cloud2.png");
		InputStream cloudStream6 = this.getClass().getResourceAsStream(
				"/resources/clouds/cloud1.png");

		// Create ArrayList of images
		cloudImages = new ArrayList<Image>();

		this.cloudImages.add(new Image(cloudStream1, "Cloud Image1", false));
		this.cloudImages.add(new Image(cloudStream2, "Cloud Image2", false));
		this.cloudImages.add(new Image(cloudStream3, "Cloud Image3", false));
		this.cloudImages.add(new Image(cloudStream4, "Cloud Image4", false));
		this.cloudImages.add(new Image(cloudStream5, "Cloud Image5", false));
		this.cloudImages.add(new Image(cloudStream6, "Cloud Image6", false));

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
		this.sidebarFont = new TrueTypeFont(this.fontPrimitive.deriveFont(
				Font.CENTER_BASELINE, 16.0f), false);

		this.sidebarFontLarge = new TrueTypeFont(this.fontPrimitive.deriveFont(
				Font.CENTER_BASELINE, 20.0f), false);

		// Set the font (used for altitudes etc.)
		this.fontPrimitive = new Font("Lucida Sans", Font.PLAIN, 12);
		this.font = new TrueTypeFont(this.fontPrimitive, true);

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
		currentGameContainer = gameContainer;

		time = 0;

		// Screen size
		windowWidth = 1200;
		windowHeight = 600;

		((AppGameContainer) gameContainer).setDisplayMode(windowWidth,
				windowHeight, false);

		try {
			this.currentGame = new MultiplayerGame(50, 100, sidebarWidth);
		} catch (NoSuchAlgorithmException e) {
			e.printStackTrace();
		} catch (UnknownHostException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		fontColor = Color.white;
		currentGame.setSpeedDifficulty(0.5);
		currentGame.setSpawnRate(4);
		currentGame.setSpawnCount(1);

	}

	/**
	 * Plays a sound at each checkpoint if the plane is selected
	 */
	public void playCheckpointSound() {
		if (currentGame.getCurrentPlane() != null
				&& currentGame.getCurrentPlane().getFlightPlan()
						.getCurrentRoute().size() != 0
				&& currentGame.getCurrentPlane().checkIfFlightAtWaypoint(
						currentGame.getCurrentPlane().getFlightPlan()
								.getCurrentRoute().get(0), currentGame)) {
			checkpointSound.play();
		}

	}

	private boolean isInHitBox(int mouseX, int mouseY, int[] pos, int width,
			int height, int tolerance) {
		return ((mouseX >= (pos[0] - tolerance))
				&& (mouseX <= (pos[0] + width + tolerance))
				&& (mouseY >= (pos[1] - tolerance)) && (mouseY <= (pos[1]
				+ height + tolerance)));
	}

	// Turns on or off the autopilot for planes

	private void setAutoPilot(boolean auto) {
		for (Plane plane : currentGame.getCurrentPlanes()) {
			if (plane.ownedByCurrentPlayer) {
				plane.setAutoPilot(auto);
			}
		}
		return;
	}

	private void sendDebris() {
		/*
		 * TODO Add functionality
		 */
	}

	private void drawShadowedText(String text, Color color, int[] pos,
			Graphics graphics) {
		graphics.setColor(Color.black);
		graphics.drawString(text, pos[0] + 2, pos[1] + 1);
		graphics.setColor(color);
		graphics.drawString(text, pos[0], pos[1]);
	}

	public void handleSidebar(GameContainer gameContainer, Graphics graphics) {

		sidebarBackground.draw(0, 0, MultiplayerWindow.sidebarWidth,
				gameContainer.getHeight());

		// Get the height of the text
		int fontHeight = sidebarFont.getHeight();

		int cloudCost = 20;
		int autopilotCost = 40;
		int debrisCost = 60;

		String sidebarTitleText = "Shop";
		String pointsText = currentGame.getScore().getCredits() + " Cr";
		String cloudText1 = "(1) Send";
		String cloudText2 = "Clouds";
		String cloudText3 = cloudCost + " Cr";
		String autopilotText1 = "(2) Disable";
		String autopilotText2 = "Autopilot";
		String autopilotText3 = autopilotCost + " Cr";
		String debrisText1 = "(3) Send";
		String debrisText2 = "Debris";
		String debrisText3 = debrisCost + " Cr";
		String waitingText = "Waiting for opponent";

		int sidebarTitleTextWidth = sidebarFontLarge.getWidth(sidebarTitleText);
		int pointsTextWidth = sidebarFont.getWidth(pointsText);
		int cloudTextWidth1 = sidebarFont.getWidth(cloudText1);
		int cloudTextWidth2 = sidebarFont.getWidth(cloudText2);
		int cloudTextWidth3 = sidebarFont.getWidth(cloudText3);
		int autopilotTextWidth1 = sidebarFont.getWidth(autopilotText1);
		int autopilotTextWidth2 = sidebarFont.getWidth(autopilotText2);
		int autopilotTextWidth3 = sidebarFont.getWidth(autopilotText3);
		int debrisTextWidth1 = sidebarFont.getWidth(debrisText1);
		int debrisTextWidth2 = sidebarFont.getWidth(debrisText2);
		int debrisTextWidth3 = sidebarFont.getWidth(debrisText3);

		int[] sidebarTitleTextPos = {
				(MultiplayerWindow.sidebarWidth - sidebarTitleTextWidth) / 2,
				50 };
		int[] pointsTextPos = {
				(MultiplayerWindow.sidebarWidth - pointsTextWidth) / 2,
				sidebarTitleTextPos[1] + fontHeight + 15 };
		int[] cloudTextPos1 = {
				(MultiplayerWindow.sidebarWidth - cloudTextWidth1) / 2, 150 };
		int[] cloudTextPos2 = {
				(MultiplayerWindow.sidebarWidth - cloudTextWidth2) / 2,
				cloudTextPos1[1] + fontHeight + 5 };
		int[] cloudTextPos3 = {
				(MultiplayerWindow.sidebarWidth - cloudTextWidth3) / 2,
				cloudTextPos2[1] + fontHeight + 5 };
		int[] autopilotTextPos1 = {
				(MultiplayerWindow.sidebarWidth - autopilotTextWidth1) / 2, 250 };
		int[] autopilotTextPos2 = {
				(MultiplayerWindow.sidebarWidth - autopilotTextWidth2) / 2,
				autopilotTextPos1[1] + fontHeight + 5 };
		int[] autopilotTextPos3 = {
				(MultiplayerWindow.sidebarWidth - autopilotTextWidth3) / 2,
				autopilotTextPos2[1] + fontHeight + 5 };
		int[] debrisTextPos1 = {
				(MultiplayerWindow.sidebarWidth - debrisTextWidth1) / 2, 350 };
		int[] debrisTextPos2 = {
				(MultiplayerWindow.sidebarWidth - debrisTextWidth2) / 2,
				debrisTextPos1[1] + fontHeight + 5 };
		int[] debrisTextPos3 = {
				(MultiplayerWindow.sidebarWidth - debrisTextWidth3) / 2,
				debrisTextPos2[1] + fontHeight + 5 };
		int[] waitingTextPos = { 350, 275 };

		Color sidebarTitleTextColor = Color.orange;
		Color pointsTextColor = Color.orange;
		Color cloudTextColor = Color.orange;
		Color autopilotTextColor = Color.orange;
		Color debrisTextColor = Color.orange;
		Color waitingTextColor = Color.orange;

		// Hovering box tolerance in pixels
		int tolerance = 10;

		// Mouse coordinates
		int x = gameContainer.getInput().getMouseX();
		int y = gameContainer.getInput().getMouseY();

		// Mouse action
		boolean clicked = gameContainer.getInput().isMousePressed(0);

		graphics.setFont(this.sidebarFontLarge);
		drawShadowedText(sidebarTitleText, sidebarTitleTextColor,
				sidebarTitleTextPos, graphics);

		graphics.setFont(this.sidebarFont);
		// Draw the points text
		pointsText = currentGame.getScore().getCredits() + " Cr";
		pointsTextWidth = this.sidebarFont.getWidth(pointsText);
		pointsTextPos[0] = (MultiplayerWindow.sidebarWidth - pointsTextWidth) / 2;
		drawShadowedText(pointsText, pointsTextColor, pointsTextPos, graphics);

		// Init Clouds
		if (cloudsInit) {
			clouds = new ArrayList<Cloud>();
			clouds.add(new Cloud(1, 0, 0, currentGame));
			clouds.add(new Cloud(1, 240, 15, currentGame));
			clouds.add(new Cloud(1, 100, currentGame.windowHeight, currentGame));
			clouds.add(new Cloud(1, 675, 0, currentGame));
			clouds.add(new Cloud(1, 825, 15, currentGame));
			clouds.add(new Cloud(1, 675, currentGame.windowHeight, currentGame));
			cloudsInit = false;

		}
		// Renders clouds if you are sending them
		if (cloudsApeared) {
			for (int i = 3; i < cloudImages.size(); i++) {
				if (!clouds.get(i).moveCloud()) {
					cloudImages.get(i).draw(clouds.get(i).getX(),
							clouds.get(i).getY());
				} else {
					;
					cloudsApeared = false;
					WindowManager.canSendClouds = true;
					clouds.get(3).setY(0);
					clouds.get(4).setY(15);
					clouds.get(5).setY(currentGame.windowHeight);
					break;
				}
			}
		}
		// Renders clouds if they are sent to you by your opponent
		if (WindowManager.receivingClouds) {
			for (int i = 0; i < 3; i++) {
				if (!clouds.get(i).moveCloud()) {
					cloudImages.get(i).draw(clouds.get(i).getX(),
							clouds.get(i).getY());
				} else {
					WindowManager.receivingClouds = false;
					WindowManager.canReceiveClouds = true;
					clouds.get(0).setY(0);
					clouds.get(1).setY(15);
					clouds.get(2).setY(currentGame.windowHeight);
					break;
				}
			}
		}
		// Send Clouds button
		if (isInHitBox(x, y, cloudTextPos1, cloudTextWidth1, fontHeight,
				tolerance)
				|| isInHitBox(x, y, cloudTextPos2, cloudTextWidth2, fontHeight,
						tolerance)
				|| isInHitBox(x, y, cloudTextPos3, cloudTextWidth3, fontHeight,
						tolerance)) {
			if (clicked && currentGame.getScore().getCredits() >= cloudCost) {
				currentGame.getScore().updateCredits(-cloudCost);
				WindowManager.sendClouds = true;
				cloudsApeared = true;
			} else {
				// Change hover text and add waypoint next to text
				cloudTextColor = Color.white;
			}
		} else { // Default colour
			cloudTextColor = Color.orange;
		}

		graphics.setFont(sidebarFont);

		// Draw the cloud text
		drawShadowedText(cloudText1, cloudTextColor, cloudTextPos1, graphics);
		drawShadowedText(cloudText2, cloudTextColor, cloudTextPos2, graphics);
		drawShadowedText(cloudText3, cloudTextColor, cloudTextPos3, graphics);

		// Turns autopilot on after n seconds
		if (WindowManager.autopilotOff) {
			if (WindowManager.autopilotInit) {
				setAutoPilot(false);
				offTime = time;
				WindowManager.autopilotInit = false;
			} else if (offTime + autoPilotDuration < time) {
				WindowManager.autopilotOff = false;
				setAutoPilot(true);
			}
		}

		// autopilot button
		if (isInHitBox(x, y, autopilotTextPos1, autopilotTextWidth1,
				fontHeight, tolerance)
				|| isInHitBox(x, y, autopilotTextPos2, autopilotTextWidth2,
						fontHeight, tolerance)
				|| isInHitBox(x, y, autopilotTextPos3, autopilotTextWidth3,
						fontHeight, tolerance)) {

			if (clicked && currentGame.getScore().getCredits() >= autopilotCost) {
				currentGame.getScore().updateCredits(-autopilotCost);
				System.out.println("Start");
				WindowManager.turnOffAutopilot = true;
			} else {
				// Change hover text and add waypoint next to text
				autopilotTextColor = Color.white;
			}
		} else { // Default colour
			autopilotTextColor = Color.orange;
		}

		graphics.setFont(this.sidebarFont);

		// Draw the autopilot text
		drawShadowedText(autopilotText1, autopilotTextColor, autopilotTextPos1,
				graphics);
		drawShadowedText(autopilotText2, autopilotTextColor, autopilotTextPos2,
				graphics);
		drawShadowedText(autopilotText3, autopilotTextColor, autopilotTextPos3,
				graphics);

		// Debris button
		if (isInHitBox(x, y, debrisTextPos1, debrisTextWidth1, fontHeight,
				tolerance)
				|| isInHitBox(x, y, debrisTextPos2, debrisTextWidth2,
						fontHeight, tolerance)
				|| isInHitBox(x, y, debrisTextPos3, debrisTextWidth3,
						fontHeight, tolerance)) {
			if (clicked && currentGame.getScore().getCredits() >= debrisCost) {
				currentGame.getScore().updateCredits(-debrisCost);
				sendDebris();

			} else {
				// Change hover text and add waypoint next to text
				debrisTextColor = Color.white;
			}
		} else { // Default colour
			debrisTextColor = Color.orange;
		}

		graphics.setFont(sidebarFont);

		// Draw the debris text
		drawShadowedText(debrisText1, debrisTextColor, debrisTextPos1, graphics);
		drawShadowedText(debrisText2, debrisTextColor, debrisTextPos2, graphics);
		drawShadowedText(debrisText3, debrisTextColor, debrisTextPos3, graphics);

		// Draw waiting for opponent if there is no opponent
		if (WindowManager.opponentFound == false) {
			drawShadowedText(waitingText, waitingTextColor, waitingTextPos,
					graphics);
		} else if (WindowManager.endingText != "") {
			drawShadowedText(WindowManager.endingText, waitingTextColor,
					waitingTextPos, graphics);
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
			Graphics graphics) {
		// Draw the game map
		map.draw(sidebarWidth, 0, windowWidth - sidebarWidth, windowHeight);

		// Covering right hand side as it is opponents
		graphics.setColor(new Color(4, 175, 236, 100));
		graphics.fillRect((windowWidth + sidebarWidth) / 2, 0,
				(windowWidth - sidebarWidth) / 2, windowHeight);

		// Dividing line
		graphics.setColor(Color.gray);
		graphics.setLineWidth(divideLineWidth);
		graphics.drawLine((windowWidth + sidebarWidth) / 2, 0,
				(windowWidth + sidebarWidth) / 2, windowHeight);

		// Setup the font
		graphics.setAntiAlias(true);
		graphics.setFont(font);
		graphics.setColor(fontColor);

		if (!currentGame.isEnding()) {
			// Display the Game Information
			graphics.drawString(
					"Score : " + ((int) (currentGame.getScore().getScore()))
							+ " pts", 1050, 15);
			graphics.drawString(
					"Multiplier :"
							+ ((int) (currentGame.getScore().getMultiplier())),
					1050, 35);

			// Loop through all the planes
			for (Plane plane : currentGame.getCurrentPlanes()) {
				// Sets to display the number of points gained above the passed
				// waypoint

				if (plane.getFlightPlan().getCurrentRoute().size() > 1) {
					if (plane.checkIfFlightAtWaypoint(plane.getFlightPlan()
							.getCurrentRoute().get(0), currentGame)) {

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
						// If it's a special waypoint, render how many
						// points
						// were won
						if (morePoints) {
							if (plane.ownedByCurrentPlayer) {
								graphics.drawString(
										"+"
												+ Integer
														.toString(getCurrentGame()
																.getScore()
																.getMultiplier() * 10),
										(float) (game.getContainer().getWidth() - (prevX
												+ sidebarWidth - 8)),
										(float) (game.getContainer()
												.getHeight() - prevY - 30));
								morePoints = synch <= 1 ? false : true;
							}
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
							if (plane.ownedByCurrentPlayer) {
								graphics.drawString(
										"+"
												+ Integer
														.toString(getCurrentGame()
																.getScore()
																.getMultiplier() * 5),
										(float) prevX - 8, (float) prevY - 30);
							}
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
					if (plane.ownedByCurrentPlayer && plane.getNeedsToTakeOff()) {
						graphics.drawString("Take me off",
								(float) plane.getX(),
								(float) (plane.getY() + 20));
						synchTakeOff--;
					}

					// Points deduced from the user if the plane is still landed
					if (synchTakeOff < TAKE_OFF_PENALTY_TIME / 5) {
						graphics.setColor(Color.red);
						graphics.drawString("-" + 10
								* currentGame.getScore().getMultiplier(), 1150,
								570);
						graphics.setColor(Color.white);

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
					if (plane.ownedByCurrentPlayer) {
						// Separation violation area
						planeAlert.getScaledCopy(
								currentGame.getPenaltyDistance(),
								currentGame.getPenaltyDistance()).drawCentered(
								(float) plane.getX(), (float) plane.getY());

						// Collision area
						planeAlertMax.getScaledCopy(
								currentGame.getSeparationDistance(),
								currentGame.getSeparationDistance())
								.drawCentered((float) plane.getX(),
										(float) plane.getY());
					} else {
						// Mirrored
						// Separation violation area
						planeAlert
								.getScaledCopy(
										currentGame.getPenaltyDistance(),
										currentGame.getPenaltyDistance())
								.drawCentered(
										(float) (game.getContainer().getWidth() - (plane
												.getX() - sidebarWidth)),
										(float) (game.getContainer()
												.getHeight() - plane.getY()));

						// Collision area
						planeAlertMax
								.getScaledCopy(
										currentGame.getSeparationDistance(),
										currentGame.getSeparationDistance())
								.drawCentered(
										(float) (game.getContainer().getWidth() - (plane
												.getX() - sidebarWidth)),
										(float) (game.getContainer()
												.getHeight() - plane.getY()));
					}
				}

				// Selected plane
				if (plane.equals(currentGame.getCurrentPlane())) {
					/*
					 * Active flights in the airspace are reaching at least
					 * 2000ft altitude otherwise it means they're landing,
					 * taking off, or landed
					 */
					if (plane.getAltitude() >= 2000) {
						planeSelectedCur = planeSelected
								.getScaledCopy(1 + ((((float) (plane.getSize())) - 1) / 5));
					}
					// Draw the plane white when selected
					planeSelectedCur
							.setRotation((float) plane.getBearing() - 90);
					planeSelectedCur.drawCentered((float) plane.getX(),
							(float) plane.getY());
				} else {
					// Planes under 2000ft are rendered smaller because they're
					// landing/taking off
					if (plane.getAltitude() < 2000) {
						// Render unselected planes that are taking-off/landing
						// with a variable size
						planeNormalCur = planeNormal
								.getScaledCopy((float) (1 + ((plane.getSize() - 2.5f + (float) plane
										.getAltitude() / 1000)) / 5));
					} else {
						planeNormalCur = planeNormal
								.getScaledCopy(1 + ((((float) (plane.getSize())) - 1) / 5));
					}

					// Render unselected planes
					if (plane.ownedByCurrentPlayer) {
						this.planeNormalCur.setRotation((float) plane
								.getBearing() - 90);
						this.planeNormalCur.drawCentered((float) plane.getX(),
								(float) plane.getY());
					} else {
						this.planeNormalCur.setRotation((float) plane
								.getBearing() + 90);
						this.planeNormalCur
								.drawCentered(
										(float) (game.getContainer().getWidth() - (plane
												.getX() - sidebarWidth)),
										(float) (game.getContainer()
												.getHeight() - plane.getY()));
					}
				}

				/*
				 * Reviews list of planes in airspace; if they need landing...:
				 * Highlights approach, Renders all planes that need landing as
				 * green Not currently selected plane rendered flashing green on
				 * odd seconds
				 */
				landingApproachAreaDrawn = false;
				if (plane.equals(currentGame.getCurrentPlane())) {
					if (currentGame.getCurrentPlane().getNeedsToLand() == true
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
				if (plane.equals(currentGame.getCurrentPlane())) {
					if (plane.getAltitude() < 2000) {
						// Selected planes that are landing / taking off are
						// smaller
						planeSelectedCur = planeSelected
								.getScaledCopy((float) (1 + ((plane.getSize() - 2.5f + (float) plane
										.getAltitude() / 1000)) / 5));
					} else {
						planeSelectedCur = this.planeSelected
								.getScaledCopy(1 + ((((float) (plane.getSize())) - 1) / 5));
					}
				}
				// If plane needs to land, make the plane blink
				else {
					// Every number of frames, blink
					if (((int) (time / BLINK_FREQUENCY)) % 2 == 0) {
						if (plane.getNeedsToLand() == true) {
							planeNeedsLandingCur = planeNeedsLanding
									.getScaledCopy(1 + ((((float) (plane
											.getSize())) - 1) / 5));
							if (plane.ownedByCurrentPlayer) {
								planeNeedsLandingCur.setRotation((float) plane
										.getBearing() - 90);
								planeNeedsLandingCur.drawCentered(
										(float) plane.getX(),
										(float) plane.getY());
							} else { // mirrored
								planeNeedsLandingCur.setRotation((float) plane
										.getBearing() + 90);
								planeNeedsLandingCur
										.drawCentered(
												(float) (game.getContainer()
														.getWidth() - (plane
														.getX() - sidebarWidth)),
												(float) (game.getContainer()
														.getHeight() - plane
														.getY()));
							}
						}
					}
				}

				// Render plane's altitude. It doesn't render when planes are
				// landed waiting to take off
				if (!plane.getNeedsToTakeOff()) {
					if (plane.ownedByCurrentPlayer) {
						graphics.drawString(plane.getAltitude() + " ft",
								(float) plane.getX(), (float) plane.getY() + 15);
					} else {
						graphics.drawString(
								plane.getAltitude() + " ft",
								(float) (game.getContainer().getWidth() - (plane
										.getX() - sidebarWidth)),
								(float) (game.getContainer().getHeight()
										- plane.getY() + 15));
					}
				}

				/* Render Landing Information above flight */

				// If plane needs to land and is not selected
				if (plane.getNeedsToLand()
						&& !plane.equals(currentGame.getCurrentPlane())
						&& !currentGame.getAirport().isPlaneLanding()) {
					if (plane.ownedByCurrentPlayer) {
						graphics.drawString("Land Me!",
								(float) (plane.getX() - 5),
								(float) (plane.getY() - 30));
					}
				}

				// If plane needs to land, but there is another plane landing at
				// the same time
				else if (plane.getNeedsToLand()
						&& currentGame.getAirport().isPlaneLanding()) {
					if (plane.ownedByCurrentPlayer) {
						graphics.drawString("Wait to Land Me!",
								(float) (plane.getX() - 5),
								(float) (plane.getY() - 30));
					}
				}

				// If plane is selected, but it's not at the landing height
				else if (plane.getNeedsToLand() && plane.getAltitude() > 2000) {
					if (plane.ownedByCurrentPlayer) {
						graphics.drawString("Lower Me!",
								(float) (plane.getX() - 5),
								(float) (plane.getY() - 30));
					}
				}

				// If plane is selected, at the right altitude, and within the
				// landing zone
				else if (plane.getNeedsToLand()
						&& plane.getAltitude() == 2000
						&& ((plane.getBearing() >= plane
								.getTakeoffValueHighMulti() && plane
								.getBearing() <= 359) || (plane.getBearing() <= plane
								.getTakeoffValueLowMulti() && plane
								.getBearing() >= 0))
						&& currentGame
								.getAirport()
								.getLandingApproachArea()
								.contains((float) plane.getX(),
										(float) plane.getY())) {
					graphics.drawString("'L' to Land",
							(float) (plane.getX() - 5),
							(float) (plane.getY() - 30));
				}

				// If plane is selected, at right height, but not within the
				// landing zone
				else if (plane.getNeedsToLand() && plane.getAltitude() <= 2000) {
					graphics.drawString("Perfect Height!",
							(float) (plane.getX() - 5),
							(float) (plane.getY() - 30));
				}

				// If plane needs to take off, and it didn't violate the
				// allowance threshold of sitting landed
				else if (plane.getNeedsToTakeOff()
						&& (!currentGame.isTakeOffPenalty())) {
					if (plane.ownedByCurrentPlayer) {
						graphics.drawString("'T' to Takeoff!",
								(float) (plane.getX() - 5),
								(float) (plane.getY() - 30));
					}
				}
			}

			// Draws ExitPoints
			for (int i = 0; i < currentGame.getListOfExitPoints().size(); i++) {

				if (currentGame.getCurrentPlane() != null) {
					// Draw the exitpoint when the selected flight has it as its
					// next point in the plan
					if (currentGame.getCurrentPlane().getFlightPlan()
							.getCurrentRoute()
							.indexOf(currentGame.getListOfExitPoints().get(i)) == 0) {
						waypointNext.drawCentered((int) currentGame
								.getListOfExitPoints().get(i).getX(),
								(int) currentGame.getListOfExitPoints().get(i)
										.getY());
					}
					// Draw the exitpoint properly for the selected flight
					else {
						waypointLast.drawCentered((int) currentGame
								.getListOfExitPoints().get(i).getX(),
								(int) currentGame.getListOfExitPoints().get(i)
										.getY());
					}
				} else {
					// Draw the exitpoints normally if no plane is selected
					waypointLast.drawCentered((int) currentGame
							.getListOfExitPoints().get(i).getX(),
							(int) currentGame.getListOfExitPoints().get(i)
									.getY());
				}
			}

			// Go through all the waypoint to draw them
			for (int i = 0; i < currentGame.getListOfWaypoints().size(); i++) {
				// If a plane is selected
				if (currentGame.getCurrentPlane() != null) {
					// If the selected plane has at least a point in his flight
					// plan
					if (currentGame.getCurrentPlane().getFlightPlan()
							.getCurrentRoute().size() > 0) {
						// If the next waypoint is among the airspace waypoints
						if (currentGame.getCurrentPlane().getFlightPlan()
								.getCurrentRoute().get(0) == currentGame
								.getListOfWaypoints().get(i)) {
							// Highlights the next waypoint
							waypointNext.drawCentered(
									(int) currentGame.getListOfWaypoints()
											.get(i).getX(),
									(int) currentGame.getListOfWaypoints()
											.get(i).getY());
						} else {
							// Draws all other waypoints normally
							waypointNormal.drawCentered(
									(int) currentGame.getListOfWaypoints()
											.get(i).getX(),
									(int) currentGame.getListOfWaypoints()
											.get(i).getY());
						}
					} else {
						// Draws all other waypoints normally
						waypointNormal.drawCentered((int) currentGame
								.getListOfWaypoints().get(i).getX(),
								(int) currentGame.getListOfWaypoints().get(i)
										.getY());
					}
				} else {
					// Draw all waypoints normally when there's no selected
					// plane
					waypointNormal.drawCentered((int) currentGame
							.getListOfWaypoints().get(i).getX(),
							(int) currentGame.getListOfWaypoints().get(i)
									.getY());
				}
			}

			// Draw arrows on top of the waypoints to give clues about the
			// flight plan
			if (currentGame.getCurrentPlane() != null) {
				// Go through all the waypoints of the selected flight
				for (int j = 0; j < currentGame.getCurrentPlane()
						.getFlightPlan().getCurrentRoute().size() - 1; j++) {
					int headingToWaypoint;

					// Differences between X and Y coordinates of waypoints
					double deltaY = currentGame.getCurrentPlane()
							.getFlightPlan().getCurrentRoute().get(j + 1)
							.getY()
							- currentGame.getCurrentPlane().getFlightPlan()
									.getCurrentRoute().get(j).getY();
					double deltaX = currentGame.getCurrentPlane()
							.getFlightPlan().getCurrentRoute().get(j + 1)
							.getX()
							- currentGame.getCurrentPlane().getFlightPlan()
									.getCurrentRoute().get(j).getX();

					// Find the orientation of the arrow
					headingToWaypoint = (int) Math.round(Math.toDegrees(Math
							.atan2(deltaY, deltaX)));

					// Draw rotated arrow
					waypointArrow.setRotation(headingToWaypoint - 90);
					waypointArrow.drawCentered((int) currentGame
							.getCurrentPlane().getFlightPlan()
							.getCurrentRoute().get(j).getX(), (int) currentGame
							.getCurrentPlane().getFlightPlan()
							.getCurrentRoute().get(j).getY());

					// Draw the arrows for the exit points properly so they do
					// not go off screen
					if (j == currentGame.getCurrentPlane().getFlightPlan()
							.getCurrentRoute().size() - 2) {
						int exitPointX = (int) currentGame.getCurrentPlane()
								.getFlightPlan().getCurrentRoute().get(j + 1)
								.getX();
						int exitPointY = (int) currentGame.getCurrentPlane()
								.getFlightPlan().getCurrentRoute().get(j + 1)
								.getY();

						// Set Rotation for the left hand side exit point arrow
						if (exitPointX == 0) {
							waypointArrow.setRotation(90);
							waypointArrow.drawCentered(exitPointX + 10,
									exitPointY);
						}

						// Set Rotation for the right hand side exit point arrow
						else if (exitPointX == getWindowWidth()) {
							waypointArrow.setRotation(270);
							waypointArrow.drawCentered(exitPointX - 10,
									exitPointY);
						}

						// Set Rotation for top exit point arrow
						else if (exitPointY == 0) {
							waypointArrow.setRotation(180);
							waypointArrow.drawCentered(exitPointX,
									exitPointY + 10);
						}
					}
				}
			}
		} else {
			// Do nothing - don't display the time for multiplayer games.
		}

		// Play the sound for going though a waypoint
		playCheckpointSound();

		/* Setting up the game over screen */

		// If the planes collided
		if (currentGame.isCollision()) {
			// If the game is ending
			if (currentGame.isEnding()) {
				this.currentGame.endingRoutine();
				// Draw the two collided planes rotated a bit so it looks like a
				// crash
				for (Plane plane : currentGame.getCollidedPlanes()) {
					planeNormal.setRotation((float) Math.toDegrees(plane
							.getBearing()) - 90);
					planeNormal
							.draw((float) plane.getX(), (float) plane.getY());
				}

				// Erase the extrapoints above the waypoints
				display = false;

				// Draw the game over text
				new TrueTypeFont(fontPrimitive.deriveFont(50f), true)
						.drawString(300f, 200f, "That didn't end well...");
				new TrueTypeFont(fontPrimitive.deriveFont(25f), true)
						.drawString(470f, 260f, "Score: "
								+ (int) currentGame.getScore().getScore());

				// Countdown till game exists to main menu
				new TrueTypeFont(fontPrimitive.deriveFont(25f), true)
						.drawString(453f, 310, "Return in: "
								+ (int) (5 - ((time - endTime) / 1000)));

				// If return time elapsed, close game to let the user play again
				if (time > (endTime + (5 * 1000))) {
					game.closeRequested();
				}
			}

			// if the planes collided but the ending has not yet been set
			else {
				// Stop the timer
				endTime = time;

				// End the game
				currentGame.setEnding(true);
			}
		}

		handleSidebar(gameContainer, graphics);
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
		time += delta;
		try {
			currentGame.update(gameContainer, game);
		} catch (IOException e) {
			e.printStackTrace();
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
	public void mouseClicked(int button, int x, int y, int clickCount) {

		/* Get mouse input */

		// Only get mouse input if the game is not ending
		if (!currentGame.isEnding()) {

			// Select plane by left clicking
			if (button == 0) {
				Plane clickedPlane = selectFlight(x, y);
				System.out.println(clickedPlane);
				if (clickedPlane != null && clickedPlane.ownedByCurrentPlayer
						&& !clickedPlane.getNeedsToTakeOff()
						&& !clickedPlane.isTakingOff()) {
					// If there is no plane where the user click, deselect the
					// current plane
					if (currentGame.getCurrentPlane() != null) {
						if (!currentGame.getCurrentPlane().getNeedsToLand()) {
							/*
							 * When a plane gets deselected, it is removed from
							 * manual control, and it retakes the automatic
							 * control
							 */
							currentGame.getCurrentPlane().setAuto();
							currentGame.getCurrentPlane().markForSyncing();
						}
					}

					currentGame.setCurrentPlane(clickedPlane);
				}

				// Give bearing by right clicking
			} else if (button == 1) {
				// If a plane is selected
				if (currentGame.getCurrentPlane() != null) {
					// Do not allow change of heading to airport planes
					if (!currentGame.getCurrentPlane().getNeedsToTakeOff()) {
						giveHeadingThroughMouse(currentGame.getCurrentPlane(),
								x, y);
						currentGame.getCurrentPlane().markForSyncing();

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
		currentGameContainer.resume();
		currentGame.setCollision(true);
		currentGame.setEnding(true);

		currentGame.clearManualPlanes();
		currentGame.setCollidedPlanes(new ArrayList<Plane>());

		currentGame.setCurrentPlane(null);

		WindowManager.sendClouds = false;
		WindowManager.receivingClouds = false;
		WindowManager.canSendClouds = true;
		WindowManager.canReceiveClouds = true;
		cloudsApeared = false;
		cloudsInit = true;
	}

	// All general Accessors
	/**
	 * @return the state's unique ID
	 */
	@Override
	public int getID() {
		return WindowManager.MULTIPLAYER_GAME_STATE;
	}

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

	public static int getSidebarWidth() {
		return sidebarWidth;
	}

	// All general Mutators
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
