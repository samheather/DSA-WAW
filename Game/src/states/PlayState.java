package states;

import java.awt.Font;
import java.io.InputStream;
import java.math.*;

import logicClasses.Airspace;

import org.lwjgl.input.Mouse;
import org.newdawn.slick.*;
import org.newdawn.slick.state.*;
import org.newdawn.slick.util.ResourceLoader;
import org.newdawn.slick.Color;
import org.newdawn.slick.Input;
import org.newdawn.slick.TrueTypeFont;
import org.newdawn.slick.Image;


public class PlayState extends BasicGameState {

	private Airspace airspace;
	private int i;
	Image cursorImg;
	public static float time;
	private Sound endOfGameSound;
	private Music gameplayMusic;
	public static TrueTypeFont font;
	private Image controlBarImage, clockImage, backgroundImage, difficultyBackground, easyButton, easyHover, mediumButton, mediumHover, hardButton, hardHover;
	private String stringTime;
	private boolean settingDifficulty, gameEnded;

	public PlayState(int state) {
		

	}

	public void init(GameContainer gc, StateBasedGame sbg) throws SlickException {
		
		gameEnded = false;
		settingDifficulty = true;
		time = 0;
		airspace = new Airspace();
		i = 1;
		this.stringTime="";
		
		gc.setAlwaysRender(true);
		gc.setUpdateOnlyWhenVisible(true);
		gc.setMouseCursor("res/graphics/cross.png",12,12);
	
		
		// Font
		
		try{
			InputStream inputStream = ResourceLoader.getResourceAsStream("res/blue_highway_font/bluehigh.ttf");
			Font awtFont= Font.createFont(Font.TRUETYPE_FONT, inputStream);
			awtFont = awtFont.deriveFont(20f);
			font = new TrueTypeFont(awtFont, true);
			
			
		}catch(Exception e){
			e.printStackTrace();
		}
		
		// Music
		
		gameplayMusic = new Music("res/music/Jarvic 8.ogg");
		endOfGameSound = new Sound("res/music/175385__digitaldominic__scream.wav");
	
		
		
		//Images
		
		controlBarImage = new Image("res/graphics/control_bar_vertical.png");
		clockImage = new Image("res/graphics/clock.png");
		backgroundImage = new Image("res/graphics/background.png");
		difficultyBackground = new Image("res/menu_graphics/difficulty.jpg");
		easyButton = new Image("res/menu_graphics/easy.png");
		easyHover = new Image("res/menu_graphics/easy_hover.png");
		mediumButton = new Image("res/menu_graphics/medium.png");
		mediumHover = new Image("res/menu_graphics/medium_hover.png");
		hardButton = new Image("res/menu_graphics/hard.png");
		hardHover = new Image("res/menu_graphics/hard_hover.png");
		
		//initialise the airspace object;
		
		
    	//Waypoints
    	airspace.newWaypoint(350, 150, "A");
    	airspace.newWaypoint(400, 470, "B");
    	airspace.newWaypoint(700, 60,  "C");
    	airspace.newWaypoint(800, 320, "D");
    	airspace.newWaypoint(600, 418, "E");
    	airspace.newWaypoint(500, 220, "F");
    	airspace.newWaypoint(950, 188, "G");
    	airspace.newWaypoint(1050, 272,"H");
    	airspace.newWaypoint(900, 420, "I");
    	airspace.newWaypoint(240, 250, "J");
    	//EntryPoints
    	airspace.newEntryPoint(150, 400);
    	airspace.newEntryPoint(1200, 200);
    	airspace.newEntryPoint(600, 0);
    	// Exit Points
    	airspace.newExitPoint(800, 0, "1");
    	airspace.newExitPoint(150, 200, "2");
    	airspace.newExitPoint(1200, 300, "3");
    	airspace.init(gc);
		

	}

	public void render(GameContainer gc, StateBasedGame sbg, Graphics g)
			throws SlickException {
		
		// Checks whether the user is still choosing the difficulty
		
		if(settingDifficulty){
			
			int posX = Mouse.getX();
			int flippedposY=Mouse.getY();
			//Fixing posY to reflect graphics coords
			int posY = 600 - flippedposY;
			
			difficultyBackground.draw(0,0);

		if (posX>100 && posX<216 && posY>300 && posY<354){
			easyHover.draw(100,300);
		} else {
			easyButton.draw(100,300);
		}

		if (posX>100 && posX<284 && posY>400 && posY<454){
			mediumHover.draw(100,400);
		} else {
			mediumButton.draw(100,400);
		}
		
		if (posX>100 && posX<227 && posY>500 && posY<554){
			hardHover.draw(100,500);
		} else {
			hardButton.draw(100,500);
		}		
		}
		
		else{
		
			g.setFont(font);
			
			
			// Drawing Side Images
			backgroundImage.draw(150,0);
			controlBarImage.draw(0,0);
			
			// Drawing Airspace and elements within it
			g.setColor(Color.white);
			airspace.render(g, gc);
			
			
			// Drawing Clock and Time
			g.setColor(Color.white);
			clockImage.draw(0,5);
			g.drawString(this.stringTime, 25, 11);
		
		}
		

		
		
		

	}

	public void update(GameContainer gc, StateBasedGame sbg, int delta)
			throws SlickException {
		
		// Checks if the game has been retried and if it has resets the airspace
		
		if (gameEnded){
			
			airspace.resetAirspace();
	    	time = 0;
	    	gameEnded = false;
	    	settingDifficulty = true;
			
		}
		
		// Checks whether the user is still choosing the difficulty
		
		if(settingDifficulty){
		
			int posX = Mouse.getX();
			int posY = Mouse.getY();
			
			posY = 600-posY;
			
			if((posX>100&&posX<216) && (posY>300&&posY<354) && Mouse.isButtonDown(0)) {
				
				airspace.setDifficultyValueOfGame(1);
				airspace.getControls().setDifficultyValueOfGame(1);
				airspace.createAndSetSeparationRules();
				settingDifficulty = false;
				
				
			}
			
			
			if((posX>100&&posX<284) && (posY>400&&posY<454) && Mouse.isButtonDown(0)) {
				
				airspace.setDifficultyValueOfGame(2);
				airspace.getControls().setDifficultyValueOfGame(2);
				airspace.createAndSetSeparationRules();
				settingDifficulty = false;
				
			}
			
			
			if((posX>100&&posX<227) && (posY>500&&posY<554) && Mouse.isButtonDown(0)) {
				
				airspace.setDifficultyValueOfGame(3);
				airspace.getControls().setDifficultyValueOfGame(3);
				airspace.createAndSetSeparationRules();
				settingDifficulty = false;
				
			}
			
		
		}
		
		else{
			
			// Updating Clock and Time
			
			time += delta;
			float decMins=time/1000/60;
			int mins = (int) decMins;
			float decSecs=decMins-mins;
				
			int secs = Math.round(decSecs*60);
				
			String stringMins="";
			String stringSecs="";
			if(secs==60){
				secs=0;
				mins+=1;
			}
			if(mins<10) {
				stringMins="0"+mins;
			}
			else {
				stringMins=String.valueOf(mins);
			}
			if(secs<10) {
				stringSecs="0"+secs;
			}
			else {
				stringSecs=String.valueOf(secs);
			}
						
			this.stringTime=stringMins+":"+stringSecs;
						
						
			// Updating Airspace
						
			airspace.newFlight(gc);
			airspace.update(gc);
			if (airspace.getSeparationRules().getGameOverViolation() == true){
				airspace.getSeparationRules().setGameOverViolation(false);
				airspace.resetAirspace();
				gameplayMusic.stop();
				endOfGameSound.play();
				sbg.enterState(2);
				gameEnded = true;
							
			}
						
						
			Input input = gc.getInput();
						
			// Checking For Pause Screen requested in game
						
			if (input.isKeyPressed(Input.KEY_P)) {
				sbg.enterState(3);
			}
			
						
			if (!gameplayMusic.playing()){
				//Loops gameplay music based on random number created in init
							
				gameplayMusic.loop(1.0f, 0.5f);
			}
			
		}
		
		
		
		

	}


	public int getID() {
		return 1;
	}

	public Airspace getAirspace() {
		return airspace;
	}

	public void setAirspace(Airspace airspace) {
		this.airspace = airspace;
	}
	


}