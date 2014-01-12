package states;

import java.awt.Font;
import java.io.InputStream;
import java.math.*;

import logicClasses.Airspace;

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
	private Music gameplayMusic, gameplayMusic2;
	public static TrueTypeFont font;
	private Image controlBarImage, clockImage, backgroundImage;
	private String stringTime;
	private double randomMusicGen;

	public PlayState(int state) {
		

	}

	public void init(GameContainer gc, StateBasedGame sbg) throws SlickException {
		
		time = 0;
		airspace = new Airspace();
		i = 1;
		this.stringTime="";
		
		gc.setAlwaysRender(true);
		gc.setUpdateOnlyWhenVisible(false);
		gc.setMouseCursor("graphics/graphics/cross.png",12,12);
	
		
		// Font
		
		try{
			InputStream inputStream = ResourceLoader.getResourceAsStream("res/blue_highway font/bluehigh.ttf");
			Font awtFont= Font.createFont(Font.TRUETYPE_FONT, inputStream);
			awtFont = awtFont.deriveFont(20f);
			font = new TrueTypeFont(awtFont, true);
			
			
		}catch(Exception e){
			e.printStackTrace();
		}
		
		// Music
		
		gameplayMusic = new Music("Cold Funk.ogg");
		gameplayMusic2 = new Music("Jarvic 8.ogg");
		endOfGameSound = new Sound("res/175385__digitaldominic__scream.wav");
		
		//Setting random_music_gen to select music 
		setMusic();
		
		
		//Images
		
		controlBarImage = new Image("/res/graphics/graphics/control_bar_vertical.png");
		clockImage = new Image("/res/graphics/graphics/clock.PNG");
		backgroundImage = new Image("/res/graphics/graphics/background.png");
		
		//initialise the airspace object;
		
		
    	//Waypoints
    	airspace.newWaypoint(350, 150);
    	airspace.newWaypoint(400, 470);
    	airspace.newWaypoint(700, 60);
    	airspace.newWaypoint(800, 320);
    	airspace.newWaypoint(600, 418);
    	airspace.newWaypoint(500, 220);
    	airspace.newWaypoint(950, 188);
    	airspace.newWaypoint(1050, 272);
    	airspace.newWaypoint(900, 420);
    	airspace.newWaypoint(240, 250);
    	//EntryPoints
    	airspace.newEntryPoint(150, 400);
    	airspace.newEntryPoint(1200, 200);
    	airspace.newEntryPoint(600, 0);
    	// Exit Points
    	airspace.newExitPoint(800, 0);
    	airspace.newExitPoint(150, 200);
    	airspace.newExitPoint(1200, 300);
    	airspace.init(gc);
		

	}

	public void render(GameContainer gc, StateBasedGame sbg, Graphics g)
			throws SlickException {
		
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
		
		//Loops gameplay music based on random number created in init
		if( randomMusicGen >= 0.5 ){
		if(!gameplayMusic.playing()){
			gameplayMusic.loop(1.0f, 0.5f);}
		} else {
		if(!gameplayMusic2.playing()){
			gameplayMusic2.loop(1.0f, 0.5f);}
		}
		
		
		

	}

	public void update(GameContainer gc, StateBasedGame sbg, int delta)
			throws SlickException {
		
		
		
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
			gameplayMusic2.stop();
			endOfGameSound.play();
			sbg.enterState(3);
			
		}
		
		
		Input input = gc.getInput();
		
		// Checking For Pause Screen requested in game
		
		if (input.isKeyPressed(Input.KEY_P)) {
			sbg.enterState(4);
		}
		if(!gc.hasFocus()) {
			sbg.enterState(4);
		}
		
		
		

	}


	public int getID() {
		return 2;
	}

	public Airspace getAirspace() {
		return airspace;
	}

	public void setAirspace(Airspace airspace) {
		this.airspace = airspace;
	}
	
	private void setMusic(){
		randomMusicGen = Math.random();
	}

}