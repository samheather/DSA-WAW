package game.gfx;


import org.newdawn.slick.AppGameContainer;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.TrueTypeFont;
import org.newdawn.slick.state.BasicGameState;
import org.newdawn.slick.state.StateBasedGame;

import java.awt.Font;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Random;

import game.struct.Game;
import game.struct.Plane;
import game.struct.Waypoint;


/**
 * GameWindow class provides an interactive game
 */
public class GameWindow extends BasicGameState {

	/** The width the game is displayed at */
	public int windowWidth;
	
	/** The height the game is displayed at */
	public int windowHeight;
	
	/** The player's score*/
	private double score;
	
	/** The time the game has been running for */
	private double time;
	
	/** The time the game ended at */
	private double endTime;
	
	/** The amount of time before another plane will enter the game */
	private double countToNextPlane;
	
	/** The collision state - <code>true</code> if two planes have collided */
	private boolean collision;
	
	/** The ending state - <code>true</code> if the game is ending */
	private boolean ending;
	
	/** The speed modifier */
	private double speedDifficulty;
	
	/** The (plane) spawn rate modifier */
	private int spawnRate;
	
	/** The number of planes to spawn at a time */
	private int spawnCount;
	
	/** A list of planes under manual control */
	private ArrayList<Plane> manualPlanes;
	
	/** A list of planes which are colliding */
	private ArrayList<Plane> collidedPlanes;

	/** The game currently being played */
	private Game currentGame;
	
	/** The plane currently being controlled by the player */
	private Plane currentPlane;
	
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

	/** The normal plane image */
	private Image planeNormal;
	
	/** The edited plane image */
	private Image planeNormalCur;
	
	/** The plane image for the selected plane */
	private Image planeSelected;
	
	/** The edited plane image for the selected plane */
	private Image planeSelectedCur;
	
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


	

	
	// Other methods										(<- locator TODO)
	/**
	 * Moves a plane
	 * <p>
	 * If the plane is under manual control, it will follow the
	 * bearing specified by the player.
	 * </p>
	 * <p>
	 * If the plane is following its flight path, it will tend towards
	 * its next target.
	 * </p>
	 * 
	 * @param plane				the plane to move
	 */
	private void movePlane(Plane plane) {
		double angle = plane.getBearing();

		// Get the angle to the next waypoint
		if(plane.getTarget() != null) {
			if(!this.manualPlanes.contains(plane)) {
				angle = Math.atan2(plane.getY() - plane.getTarget().getY(),
						plane.getX() - plane.getTarget().getX());
			}

			// Set plane's bearing
			plane.setBearing(angle);

			// Move the plane
			plane.setX((float) (plane.getX()
					- (Math.cos(angle)
							* (this.speedDifficulty
									* plane.getVelocity() / 7000d))));
			plane.setY((float) (plane.getY()
					- (Math.sin(angle)
							* (this.speedDifficulty
									* plane.getVelocity() / 7000d))));
		}
	}
	
	/**
	 * Converts an altitude level to a height
	 * 
	 * @param altitude				the altitude level to convert
	 */
	private int getHeightFromAltitude(double altitude) {
		return (int) Math.round(18000 + (altitude * 2000));
	}
	
	/**
	 * Removes a plane from manual control
	 * 
	 * @param plane				the plane to remove from manual control
	 */
	private void removeFromManual(Plane plane) {
		while(this.manualPlanes.contains(plane)) {
			this.manualPlanes.remove(plane);
			plane.setTarget(plane.getFlightPlan().getCurrentRoute().get(0));
		}
	}
	
	private void deleteFromManual(Plane plane) {
		while(this.manualPlanes.contains(plane)) {
			this.manualPlanes.remove(plane);
		}
	}
	
	private void giveHeadingThroughMouse(Plane currentPlane, int x, int y){
		this.manualPlanes.add(this.currentPlane);
		this.currentPlane.setBearing(
				Math.atan2(this.currentPlane.getY() - y,
						this.currentPlane.getX() - x));
	}
	


	
	// Overrides
	/**
	 * Initialises the state
	 * 
	 * @param gameContainer		the game container holding this state
	 * @param game				the game running this state
	 */
	@Override
	public void init(GameContainer gameContainer,
			StateBasedGame game) throws SlickException {
		// Setup input
		this.setInput(gameContainer.getInput());
		
		// Load waypoint images
		InputStream waypointNormalStream = this.getClass()
				.getResourceAsStream("/resources/waypoints/WaypointRed.png");
		InputStream waypointNextStream = this.getClass()
				.getResourceAsStream("/resources/waypoints/WaypointWhite.png");
		InputStream waypointLastStream = this.getClass()
				.getResourceAsStream("/resources/waypoints/WaypointGreen.png");
		InputStream waypointExitStream = this.getClass()
				.getResourceAsStream("/resources/waypoints/WaypointBlue.png");
		
		this.waypointNormal = new Image(waypointNormalStream,
				"Waypoint Normal Image", false);
		this.waypointNext = new Image(waypointNextStream,
				"Waypoint Next Image", false);
		this.waypointLast = new Image(waypointLastStream,
				"Waypoint Last Image", false);
		this.waypointExit = new Image(waypointExitStream,
				"Waypoint Exit Image", false);
		
		// Load plane images
		InputStream planeNormalStream = this.getClass()
				.getResourceAsStream("/resources/planes/Plane.png");
		InputStream planeSelectedStream = this.getClass()
				.getResourceAsStream("/resources/planes/PlaneSelected.png");
		InputStream planeAlertStream = this.getClass()
				.getResourceAsStream("/resources/planes/PlaneAlert.png");
		InputStream planeAlertMaxStream = this.getClass()
				.getResourceAsStream("/resources/planes/PlaneAlertMax.png");
		
		this.planeNormal = new Image(planeNormalStream,
				"Plane Normal Image", false).getScaledCopy(14, 18);
		this.planeNormalCur = this.planeNormal;
		this.planeSelected = new Image(planeSelectedStream,
				"Plane Selected Image", false).getScaledCopy(14, 18);
		this.planeSelectedCur = this.planeSelected;
		this.planeAlert = new Image(planeAlertStream,
				"Plane Alert Image", false);
		this.planeAlertMax = new Image(planeAlertMaxStream,
				"Plane Alert Max Image", false);
		
		// Load map images
		InputStream map1Stream = this.getClass()
				.getResourceAsStream("/resources/maps/Map1.png");
		InputStream map2Stream = this.getClass()
				.getResourceAsStream("/resources/maps/Map2.png");
		
		this.map1 = new Image(map1Stream, "Map 1 Image", false);
		this.map2 = new Image(map2Stream, "Map 2 Image", false);
		
		// Set the font (used for altitudes etc.)
		this.fontPrimitive = new Font("Lucida Sans", Font.PLAIN, 12);
		this.font = new TrueTypeFont(this.fontPrimitive, true);
	}
	
	/**
	 * Sets properties which need resetting each time the state is accessed
	 * 
	 * <p>
	 * Specifically, this clears the game so that it restarts when
	 * exited and re-entered.
	 * </p>
	 * 
	 * @param gameContainer		the game container holding this state
	 * @param game				the game running this state
	 */
	@Override
	public void enter(GameContainer gameContainer,
			StateBasedGame game) throws SlickException {
		this.currentGameContainer = gameContainer;
		
		this.windowWidth = 1200;
		this.windowHeight = 600;
		
		((AppGameContainer) gameContainer).setDisplayMode(
				this.windowWidth, this.windowHeight, false);
		
		this.time = 0;
		this.score = 0;
		this.endTime = 0;
		this.countToNextPlane = 0;
		
		this.collision = false;
		this.ending = false;
		
		this.manualPlanes = new ArrayList<Plane>();
		this.collidedPlanes = new ArrayList<Plane>();

		if(((WindowManager) game).getCurrentLevel() == 1) {
			// Play level 1
			this.currentGame = new Game(50, 100, this);
			this.map = this.map1;
			this.fontColor = Color.white;
			this.speedDifficulty = 0.5;
			this.spawnRate = 40;
			this.spawnCount = 1;
		} else if(((WindowManager) game).getCurrentLevel() == 2) {
			// Play level 2
			this.currentGame = new Game(75, 200, this);
			this.map = this.map2;
			this.fontColor = Color.black;
			this.speedDifficulty = 0.5;
			this.spawnRate = 30;
			this.spawnCount = 1;
		} else {
			// ERROR
		}
		
		this.currentPlane = null;
	}

	/**
	 * Renders the state
	 * 
	 * @param gameContainer		the game container holding this state
	 * @param game				the game running this state
	 * @param g					the graphics container to display content in
	 */
	@Override
	public void render(GameContainer gameContainer, StateBasedGame game, Graphics g) {
		Waypoint tempWaypoint, tempNextVisibleWaypoint;
		
		// Draw the game map
		this.map.draw(0, 0, this.windowWidth, this.windowHeight);
		
		g.setAntiAlias(true);
		g.setFont(this.font);
		g.setColor(this.fontColor);

		if(!this.ending) {
			// Display the game duration (time)
			g.drawString("Time : " + ((int) (this.time / 1000)) + "s", 925, 15);
			g.drawString("Score : " + ((int) (this.score)) + " pts", 925, 35);

			
			for(Plane plane : this.currentGame.getCurrentPlanes()) {
				// If plane is within penalty distance, apply alert images
				if(plane.getAlertStatus()) {
					this.planeAlert
							.getScaledCopy(this.currentGame.getPenaltyDistance(),
									this.currentGame.getPenaltyDistance())
												.drawCentered((float) plane.getX(),
																(float) plane.getY());
					
					this.planeAlertMax
							.getScaledCopy(this.currentGame.getSeparationDistance(),
									this.currentGame.getSeparationDistance())
												.drawCentered((float) plane.getX(),
																(float) plane.getY());
				}
				
				// Render each plane
				if(plane.equals(this.currentPlane)) {
					this.planeSelectedCur = this.planeSelected.getScaledCopy(
							1 + ((((float) (plane.getSize())) - 1) / 5));
					this.planeSelectedCur.setRotation((float) Math.toDegrees(
														plane.getBearing()) - 90);
					this.planeSelectedCur.drawCentered((float)plane.getX(),
													(float)plane.getY());
				} else {
					this.planeNormalCur = this.planeNormal.getScaledCopy(
							1 + ((((float) (plane.getSize())) - 1) / 5));
					this.planeNormalCur.setRotation((float) Math.toDegrees(
														plane.getBearing()) - 90);
					this.planeNormalCur.drawCentered((float)plane.getX(), 
													(float)plane.getY());
				}
				
				// Render each plane's altitude
				g.drawString((this.getHeightFromAltitude(
						plane.getAltitude()) + " ft"),
								(float) plane.getX() + 5,
								(float) plane.getY() + 15);
				
				// Render each plane's ID
				g.drawString((plane.getID()),
								(float) (plane.getX() + 5),
								(float) (plane.getY() - 30));

				// Render each waypoint
				boolean testing = false;
				

				

				
				
				
			}
			
			// Set next Waypoint images
			
			for (int i = 0; i < this.currentGame.getListOfWaypoints().size(); i++) { // Draws waypoints
				if (this.currentPlane != null){
					if (this.currentPlane.getFlightPlan().getCurrentRoute().indexOf(this.currentGame.getListOfWaypoints().get(i)) ==0){
						this.waypointNext.drawCentered((int)this.currentGame.getListOfWaypoints().get(i).getX(),(int) this.currentGame.getListOfWaypoints().get(i).getY());
					}
					else{
						this.waypointNormal.drawCentered((int)this.currentGame.getListOfWaypoints().get(i).getX(),(int) this.currentGame.getListOfWaypoints().get(i).getY());
					}
					
				}
				
				else{
					this.waypointNormal.drawCentered((int)this.currentGame.getListOfWaypoints().get(i).getX(),(int) this.currentGame.getListOfWaypoints().get(i).getY());
				}
			}
			
			
			for (int i = 0; i < this.currentGame.getListOfExitPoints().size(); i++) { // Draws waypoints
				if (this.currentPlane != null){
					if (this.currentPlane.getFlightPlan().getCurrentRoute().indexOf(this.currentGame.getListOfExitPoints().get(i)) ==0){
						this.waypointNext.drawCentered((int)this.currentGame.getListOfExitPoints().get(i).getX(),(int) this.currentGame.getListOfExitPoints().get(i).getY());
					}
					else{
						this.waypointLast.drawCentered((int)this.currentGame.getListOfExitPoints().get(i).getX(),(int) this.currentGame.getListOfExitPoints().get(i).getY());
					}
					
				}
				
				else{
					this.waypointLast.drawCentered((int)this.currentGame.getListOfExitPoints().get(i).getX(),(int) this.currentGame.getListOfExitPoints().get(i).getY());
				}
			}
			
			
			
			
			
			
		} else {
			// Display the game duration (time)
			g.drawString("Time : " + ((int) (this.endTime / 1000)) + "s", 925, 15);
		}
		

		// End game
		if(this.collision) {
			if(this.ending) {
				// Draw the two colliding planes
				for(Plane plane : this.collidedPlanes) {
					this.planeNormal.setRotation((float) Math.toDegrees(
							plane.getBearing()) - 90);
					this.planeNormal.draw((float) plane.getX(),
											(float) plane.getY());
				}
				
				new TrueTypeFont(this.fontPrimitive.deriveFont(50f), true)
									.drawString(375f, 200f, "Game Over");
				new TrueTypeFont(this.fontPrimitive.deriveFont(25f), true)
									.drawString(470f, 260f, "Score: " + (int) this.score);
				new TrueTypeFont(this.fontPrimitive.deriveFont(25f), true)
									.drawString(450f, 310,
											"Return in: " + (int)
											(5 - ((this.time - this.endTime)
						/ 1000)));
				if(this.time > (this.endTime + (5 * 1000))) {
					game.closeRequested();
				}
			} else {
				this.endTime = this.time;
				this.ending = true;
			}
		}
	}
	
	/**
	 * Updates the state
	 * 
	 * @param gameContainer		the game container holding this state
	 * @param game				the game running this state
	 * @param delta				the time change between calls
	 */
	@Override
	public void update(GameContainer gameContainer,
			StateBasedGame game, int delta) {
		ArrayList<Plane> planesToRemove = new ArrayList<Plane>();
		Waypoint tempNextVisibleTarget;
		
		// Update the time
		this.time += delta;
		
		// Spawn more planes when no planes present
		if(this.currentGame.getCurrentPlanes().size() == 0) {
			this.countToNextPlane = 0;
		}
		
		if(!this.collision && !gameContainer.isPaused()
				&& gameContainer.hasFocus()) {
			// Create planes			
			if (this.countToNextPlane == 0) {
				for(int i = 0; i < this.spawnCount; i++) {
					this.currentGame.createPlane();
				}

				if(this.spawnRate == 0) {
					this.countToNextPlane = -1;
				} else {
					this.countToNextPlane = (60 * ((new Random()).nextInt(
							this.spawnRate / 2) + this.spawnRate));
				}
			}

			// Handle directional controls
			if(this.currentPlane != null) {
				// Action on 'a' and 'left' keys
				if(gameContainer.getInput().isKeyDown(203)
						|| gameContainer.getInput().isKeyDown(30)) {
					if(!this.manualPlanes.contains(this.currentPlane)) {
						this.manualPlanes.add(this.currentPlane);
					}
					
					this.currentPlane.decrementBearing();
				}

				// Action on 'd' and 'right' keys
				if(gameContainer.getInput().isKeyDown(205)
						|| gameContainer.getInput().isKeyDown(32)) {
					if(!this.manualPlanes.contains(this.currentPlane)) {
						this.manualPlanes.add(this.currentPlane);
					}
					
					this.currentPlane.incrementBearing();
				}

				// Action on 'w' and 'up' keys
				if(gameContainer.getInput().isKeyPressed(200)
						|| gameContainer.getInput().isKeyPressed(17)) {
					this.currentPlane.incrementTargetAltitude();
				}
				
				// Action on 's' and 'down' keys
				if(gameContainer.getInput().isKeyPressed(208)
						|| gameContainer.getInput().isKeyPressed(31)) {
					this.currentPlane.decrementTargetAltitude();
				}
			}
			
			
			
			// Action on TAB key
			if(gameContainer.getInput().isKeyPressed(15)) {
				if(currentPlane != null) {
					int index = 0;
					int planeCount = this.currentGame
							.getCurrentPlanes().size();
				
					for(int i = 0; i < planeCount; i++) {
						if(this.currentGame.getCurrentPlanes()
								.get(i) == this.currentPlane) {
							index = ((i + 1) % planeCount);
						}
					}
					
					this.currentPlane = this.currentGame
							.getCurrentPlanes().get(index);
				} else {
					this.currentPlane = this.currentGame
							.getCurrentPlanes().get(0);
				}
			}
			
			// Update planes
			for(Plane plane : this.currentGame.getCurrentPlanes()) {
				System.out.println(plane.getX());
				System.out.println(plane.getY());
				// Check plane still in game area
				if(this.manualPlanes.contains(plane)
						&& ((plane.getX() > this.windowWidth)
						|| (plane.getX() < 0)
						|| (plane.getY() > this.windowHeight)
						|| (plane.getY() < 0))) {
					planesToRemove.add(plane);
				}

				// Check if colliding with another plane
				if(this.currentGame.collision(plane)) {
					this.currentPlane = null;
					this.collidedPlanes.add(plane);
					this.collision = true;
				}
				
				// If plane has no more waypoints, remove it
				if(plane.getFlightPlan().getCurrentRoute().size() == 0) {
					planesToRemove.add(plane);
					System.out.println("Removed");
					
				} else {
					// Check if plane at waypoint
					
					

					if(plane.checkIfFlightAtWaypoint(plane.getFlightPlan().getCurrentRoute().get(0))) {
							
						plane.getFlightPlan().getCurrentRoute().remove(0);
						if(plane.getFlightPlan().getCurrentRoute().size()!= 0){
							plane.setTarget(plane.getFlightPlan().getCurrentRoute().get(0));
						}
						this.score +=10;
					}
				}

				// Change altitude
				if(!(plane.getAltitude() > (plane.getTargetAltitude() - 0.001))
						|| !(plane.getAltitude() < (plane.getTargetAltitude() + 0.001))) {
					if(plane.getAltitude() > plane.getTargetAltitude()) {
						plane.decrementAltitude();
					} else {
						plane.incrementAltitude();
					}
				}
				
				this.movePlane(plane);
			}
			
			// Remove planes
			for(Plane plane : planesToRemove) {
				this.deleteFromManual(plane);
				this.currentGame.getCurrentPlanes().remove(plane);
				this.score += 20;
			}

			this.countToNextPlane--;
		}
	}
	
	/**
	 * Handles mouse click events
	 * 
	 * @param button			the button pressed: 0 = left mouse
	 * 							button, 1 = right mouse button
	 * @param x					the x position of the mouse
	 * @param y					the y position of the mouse
	 * @param clickCount		the number of times the mouse was clicked
	 */
	@Override
	public void mouseClicked(int button, int x, int y, int clickCount) 
	{
		if(!this.ending) {
			if(button == 0) {
				boolean clickedPlane = false;
				double planeX, planeY;

				for(Plane plane : this.currentGame.getCurrentPlanes()) {
					planeX = plane.getX();
					planeY = plane.getY();

					if(Math.sqrt(Math.pow(x - planeX, 2)
							+ Math.pow(y - planeY, 2)) < 50) {
						if(plane.equals(this.currentPlane)) {
							if(this.manualPlanes.contains(plane)) {
								this.removeFromManual(plane);
							}

							this.currentPlane = null;
						} else {
							this.currentPlane = plane;
						}

						clickedPlane = true;
						break;
					}
				}

				if(!clickedPlane && (this.currentPlane != null)) {
					this.removeFromManual(this.currentPlane);
					this.currentPlane = null;
				}
			} else {
				if(this.currentPlane != null) {
						
					this.giveHeadingThroughMouse(currentPlane, x, y);
				}
			}
		}
	}
	
	/**
	 * Handles mouse wheel events
	 * 
	 * @param change			the amount the wheel was turned by
	 */
	@Override
	public void mouseWheelMoved(int change) {
		if(!this.ending) {
			if((this.currentPlane != null) && (change > 0)) {
				this.currentPlane.incrementTargetAltitude();
			} else if((this.currentPlane != null) && (change < 0)){
				this.currentPlane.decrementTargetAltitude();
			}
		}
	}
	
	/**
	 * Handles key press events
	 * 
	 * @param key				the key nuber pressed
	 * @param c					the key character pressed
	 */
	public void keyPressed(int key, char c) {
		// Handle game pausing
		if(key == 57) {
			if(this.currentGameContainer.isPaused()) {
				this.currentGameContainer.resume();
			} else {
				this.currentGameContainer.pause();
			}
		}
	}
	
	/**
	 * Sets properties which need resetting each time the state is accessed
	 * 
	 * <p>
	 * Specifically, this clears the game so that it restarts when
	 * exited and re-entered.
	 * </p>
	 * 
	 * @param gameContainer		the game container holding this state
	 * @param game				the game running this state
	 */
	@Override
	public void leave(GameContainer gameContainer,
			StateBasedGame game) throws SlickException {
		
		this.collision = true;
		this.ending = true;
		
		this.manualPlanes = new ArrayList<Plane>();
		this.collidedPlanes = new ArrayList<Plane>();
		
		this.currentPlane = null;
	}

	/**
	 * @return					the state's unique ID
	 */
	@Override
	public int getID() {
		return WindowManager.GAME_STATE;
	}
	
	// Accessors
		/**
		 * @return					the current window width
		 */
		public int getWindowWidth() {
			return this.windowWidth;
		}
		
		/**
		 * @return					the current window height
		 */
		public int getWindowHeight() {
			return this.windowHeight;
		}
		
		/**
		 * @return					the time
		 */
		public double getTime() {
			return this.time;
		}

		/**
		 * @return					the time the game ended at
		 */
		public double getEndTime() {
			return this.endTime;
		}

		/**
		 * @return					the time until the next plane spawns
		 */
		public double getCountToNextPlane() {
			return this.countToNextPlane;
		}

		/**
		 * @return					the collision state
		 */
		public boolean isCollision() {
			return this.collision;
		}

		/**
		 * @return					the ending state
		 */
		public boolean isEnding() {
			return this.ending;
		}

		/**
		 * @return					the speed modifier
		 */
		public double getSpeedDifficulty() {
			return this.speedDifficulty;
		}

		/**
		 * @return					the spawn rate modifier
		 */
		public int getSpawnRate() {
			return this.spawnRate;
		}

		/**
		 * @return					the list of planes under manual control
		 */
		public ArrayList<Plane> getManualPlanes() {
			return this.manualPlanes;
		}

		/**
		 * @return					the list of planes which have collided
		 */
		public ArrayList<Plane> getCollidedPlanes() {
			return this.collidedPlanes;
		}

		/**
		 * @return					the current game
		 */
		public Game getCurrentGame() {
			return this.currentGame;
		}

		/**
		 * @return					the currently selected plane
		 */
		public Plane getCurrentPlane() {
			return this.currentPlane;
		}

		/**
		 * @return					the current map
		 */
		public Image getMap() {
			return this.map;
		}

		/**
		 * @return					the map for level 1
		 */
		public Image getMap1() {
			return this.map1;
		}

		/**
		 * @return					the map for level 2
		 */
		public Image getMap2() {
			return this.map2;
		}

		/**
		 * @return					the normal waypoint image
		 */
		public Image getWaypointNormal() {
			return this.waypointNormal;
		}

		/**
		 * @return					the next waypoint image
		 */
		public Image getWaypointNext() {
			return this.waypointNext;
		}

		/**
		 * @return					the exit waypoint image
		 */
		public Image getWaypointExit() {
			return this.waypointExit;
		}

		/**
		 * @return					the last waypoint image
		 */
		public Image getWaypointLast() {
			return this.waypointLast;
		}

		/**
		 * @return					the normal plane image
		 */
		public Image getPlaneNormal() {
			return this.planeNormal;
		}

		/**
		 * @return					the in-use plane image
		 */
		public Image getPlaneNormalCur() {
			return this.planeNormalCur;
		}

		/**
		 * @return					the selected plane image
		 */
		public Image getPlaneSelected() {
			return this.planeSelected;
		}

		/**
		 * @return					the in-use selected plane image
		 */
		public Image getPlaneSelectedCur() {
			return this.planeSelectedCur;
		}

		/**
		 * @return					the plane alert range image
		 */
		public Image getPlaneAlert() {
			return this.planeAlert;
		}

		/**
		 * @return					the plane collision range image
		 */
		public Image getPlaneAlertMax() {
			return this.planeAlertMax;
		}

		/**
		 * @return					the base font
		 */
		public Font getFontPrimitive() {
			return this.fontPrimitive;
		}

		/**
		 * @return					the font
		 */
		public TrueTypeFont getFont() {
			return this.font;
		}
		
		/**
		 * @return					the font colour
		 */
		public Color getFontColor() {
			return this.fontColor;
		}
		
		/**
		 * @return					a reference to the current game container
		 */
		public GameContainer getCurrentGameContainer() {
			return this.currentGameContainer;
		}

		
		// Mutators
		/**
		 * @param windowWidth		the new window width
		 */
		public void setWindowWidth(int windowWidth) {
			this.windowWidth = windowWidth;
		}
		
		/**
		 * @param windowHeight		the new window height
		 */
		public void setWindowHeight(int windowHeight) {
			this.windowHeight = windowHeight;
		}
		
		/**
		 * @param time				the time to set
		 */
		public void setTime(double time) {
			this.time = time;
		}

		/**
		 * @param endTime			the ending time to set
		 */
		public void setEndTime(double endTime) {
			this.endTime = endTime;
		}

		/**
		 * @param countToNextPlane	the new time until the next plane spawns
		 */
		public void setCountToNextPlane(double countToNextPlane) {
			this.countToNextPlane = countToNextPlane;
		}

		/**
		 * @param collision			the collision state to set
		 */
		public void setCollision(boolean collision) {
			this.collision = collision;
		}

		/**
		 * @param ending			the ending state to set
		 */
		public void setEnding(boolean ending) {
			this.ending = ending;
		}

		/**
		 * @param speedDifficulty	the speed modifier to set
		 */
		public void setSpeedDifficulty(double speedDifficulty) {
			this.speedDifficulty = speedDifficulty;
		}

		/**
		 * @param spawnRate	the spawn rate modifier to set
		 */
		public void setSpawnRate(int spawnRate) {
			this.spawnRate = spawnRate;
		}

		/**
		 * @param manualPlanes		the list of manual planes to set
		 */
		public void setManualPlanes(ArrayList<Plane> manualPlanes) {
			this.manualPlanes = manualPlanes;
		}

		/**
		 * @param collidedPlanes	the list of collided planes to set
		 */
		public void setCollidedPlanes(ArrayList<Plane> collidedPlanes) {
			this.collidedPlanes = collidedPlanes;
		}

		/**
		 * @param currentGame		the new current game
		 */
		public void setCurrentGame(Game currentGame) {
			this.currentGame = currentGame;
		}

		/**
		 * @param currentPlane		the new current plane
		 */
		public void setCurrentPlane(Plane currentPlane) {
			this.currentPlane = currentPlane;
		}

		/**
		 * @param map				the map to set
		 */
		public void setMap(Image map) {
			this.map = map;
		}

		/**
		 * @param map1				the map to set for level 1
		 */
		public void setMap1(Image map1) {
			this.map1 = map1;
		}

		/**
		 * @param map2				the map to set for level 2
		 */
		public void setMap2(Image map2) {
			this.map2 = map2;
		}

		/**
		 * @param waypointNormal	the normal waypoint image to set
		 */
		public void setWaypointNormal(Image waypointNormal) {
			this.waypointNormal = waypointNormal;
		}

		/**
		 * @param waypointNext		the next waypoint image to set
		 */
		public void setWaypointNext(Image waypointNext) {
			this.waypointNext = waypointNext;
		}

		/**
		 * @param waypointExit		the exit waypoint image to set
		 */
		public void setWaypointExit(Image waypointExit) {
			this.waypointExit = waypointExit;
		}

		/**
		 * @param waypointLast		the last waypoint image to set
		 */
		public void setWaypointLast(Image waypointLast) {
			this.waypointLast = waypointLast;
		}

		/**
		 * @param planeNormal		the normal plane image to set
		 */
		public void setPlaneNormal(Image planeNormal) {
			this.planeNormal = planeNormal;
		}

		/**
		 * @param planeNormalCur	the in-use plane image to set
		 */
		public void setPlaneNormalCur(Image planeNormalCur) {
			this.planeNormalCur = planeNormalCur;
		}

		/**
		 * @param planeSelected		the selected plane image to set
		 */
		public void setPlaneSelected(Image planeSelected) {
			this.planeSelected = planeSelected;
		}

		/**
		 * @param planeSelectedCur	the in-use selected plane image to set
		 */
		public void setPlaneSelectedCur(Image planeSelectedCur) {
			this.planeSelectedCur = planeSelectedCur;
		}

		/**
		 * @param planeAlert		the plane alert range image to set
		 */
		public void setPlaneAlert(Image planeAlert) {
			this.planeAlert = planeAlert;
		}

		/**
		 * @param planeAlertMax		the plane collision range image to set
		 */
		public void setPlaneAlertMax(Image planeAlertMax) {
			this.planeAlertMax = planeAlertMax;
		}

		/**
		 * @param fontPrimitive		the new font primitive
		 */
		public void setFontPrimitive(Font fontPrimitive) {
			this.fontPrimitive = fontPrimitive;
		}

		/**
		 * @param font				the font derivative to set
		 */
		public void setFont(TrueTypeFont font) {
			this.font = font;
		}
		
		/**
		 * @param fontColor			the font color to set
		 */
		public void setFontColor(Color fontColor) {
			this.fontColor = fontColor;
		}
		
		/**
		 * @param currentGameContainer	a reference to the new game container to set
		 */
		public void setCurrentGameContainer(
				GameContainer currentGameContainer) {
			this.currentGameContainer = currentGameContainer;
		}
}
