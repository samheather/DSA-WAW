package states;

import java.awt.Font;
import java.io.InputStream;

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
	private Sound end_of_game_sound;
	public static TrueTypeFont font;
	private Image control_bar_image, clock_image, background_image;
	private String string_time;

	public PlayState(int state) {
		

	}

	public void init(GameContainer gc, StateBasedGame sbg) throws SlickException {
		
		time = 0;
		airspace = new Airspace();
		i = 1;
		this.string_time="";
		
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
		

		end_of_game_sound = new Sound("res/175385__digitaldominic__scream.wav");
		
		
		//Images
		
		control_bar_image = new Image("/res/graphics/graphics/control_bar_vertical.png");
		clock_image = new Image("/res/graphics/graphics/clock.PNG");
		background_image = new Image("/res/graphics/graphics/background.png");
		
		//initialise the airspace object;
		
		
    	//Waypoints
    	airspace.new_waypoint(350, 150);
    	airspace.new_waypoint(400, 470);
    	airspace.new_waypoint(700, 60);
    	airspace.new_waypoint(800, 320);
    	airspace.new_waypoint(600, 418);
    	airspace.new_waypoint(500, 220);
    	airspace.new_waypoint(950, 188);
    	airspace.new_waypoint(1050, 272);
    	airspace.new_waypoint(900, 420);
    	airspace.new_waypoint(240, 250);
    	//EntryPoints
    	airspace.new_entry_point(150, 400);
    	airspace.new_entry_point(1200, 200);
    	airspace.new_entry_point(600, 0);
    	// Exit Points
    	airspace.new_exit_point(800, 0);
    	airspace.new_exit_point(150, 200);
    	airspace.new_exit_point(1200, 300);
    	airspace.init(gc);
		

		

	}

	public void render(GameContainer gc, StateBasedGame sbg, Graphics g)
			throws SlickException {
		
		g.setFont(font);
		
		
		// Drawing Side Images
		background_image.draw(150,0);
		control_bar_image.draw(0,0);
		
		// Drawing Airspace and elements within it
		g.setColor(Color.white);
		airspace.render(g, gc);
		
		
		// Drawing Clock and Time
		g.setColor(Color.white);
		clock_image.draw(0,5);
		g.drawString(this.string_time, 25, 11);
		g.drawString("FPS:" + Integer.toString(gc.getFPS()), 48, 400);
		
		
		
		

	}

	public void update(GameContainer gc, StateBasedGame sbg, int delta)
			throws SlickException {
		
		// Updating Clock and Time
		
		time += delta;
		float dec_mins=time/1000/60;
		int mins = (int) dec_mins;
		float dec_secs=dec_mins-mins;

		int secs = Math.round(dec_secs*60);

		String string_mins="";
		String string_secs="";
		if(secs==60){
			secs=0;
			mins+=1;
		}
		if(mins<10) {
			string_mins="0"+mins;
		}
		else {
			string_mins=String.valueOf(mins);
		}
		if(secs<10) {
			string_secs="0"+secs;
		}
		else {
			string_secs=String.valueOf(secs);
		}
		
		this.string_time=string_mins+":"+string_secs;
		
		
		// Updating Airspace
		
		airspace.new_flight(gc);
		airspace.update(gc);
		if (airspace.get_separation_rules().getGameOverViolation() == true){
			airspace.get_separation_rules().setGameOverViolation(false);
			airspace.reset_airspace();
			end_of_game_sound.play();
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

}