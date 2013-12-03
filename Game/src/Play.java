import java.awt.Cursor;
import java.awt.Font;
import java.io.InputStream;

import org.lwjgl.input.Mouse;
import org.newdawn.slick.*;
import org.newdawn.slick.state.*;
import org.newdawn.slick.util.ResourceLoader;
import org.newdawn.slick.Color;
import org.newdawn.slick.Input;
import org.newdawn.slick.TrueTypeFont;
import org.newdawn.slick.Image;

public class Play extends BasicGameState {

	private Airspace a;
	private int i;
	Image cursorImg;
	public static float time;
	private Music main_game_music;
	private Sound end_of_game_sound;
	public static TrueTypeFont font;
	private Image bottom_bar_image, control_bar_image, clock_image, background_image;
	private String string_time;

	public Play(int state) {
		a = new Airspace();
		i = 1;

	}

	public void init(GameContainer arg0, StateBasedGame arg1)
			throws SlickException {
		// TODO Auto-generated method stub

		/*
		 * cursorImg= new Image("res/cursor.png"); gc.setMouseCursor(cursorImg, 16, 16); if someone can make a decent cursor image we can have a
		 * better cursor
		 */
		this.string_time="";
		arg0.setAlwaysRender(true);
		a.init(arg0);
		main_game_music = new Music("res/Love Song In My Mind.wav");
		main_game_music.loop();
		main_game_music.setVolume(0.2f);
		end_of_game_sound = new Sound("res/175385__digitaldominic__scream.wav");
		
		//bottom_bar_image = new Image("/res/graphics/graphics/flight_menu.jpg");
		control_bar_image = new Image("/res/graphics/graphics/control_bar_vertical.png");
		clock_image = new Image("/res/graphics/graphics/clock.PNG");
		background_image = new Image("/res/graphics/graphics/background.png");
		
		
		
		a.new_waypoint(350, 150);
		a.new_waypoint(400, 470);
		a.new_waypoint(700, 60);
		a.new_waypoint(800, 320);
		a.new_waypoint(600, 418);
		a.new_waypoint(500, 220);
		a.new_waypoint(950, 188);
		a.new_waypoint(1050, 272);
		a.new_waypoint(900, 420);
		a.new_waypoint(240, 250);
		a.new_entry_point(150, 400);
		a.new_entry_point(1200, 200);
		a.new_entry_point(600, 0);
		a.new_exit_point(800, 0);
		a.new_exit_point(150, 200);
		a.new_exit_point(1200, 300);
		
		try{
			InputStream inputStream = ResourceLoader.getResourceAsStream("res/blue_highway font/bluehigh.ttf");
			Font awtFont= Font.createFont(Font.TRUETYPE_FONT, inputStream);
			awtFont = awtFont.deriveFont(20f);
			font = new TrueTypeFont(awtFont, false);
			
			
		}catch(Exception e){
			e.printStackTrace();
		}
		

	}

	public void render(GameContainer gc, StateBasedGame sbj, Graphics g)
			throws SlickException {
		g.setFont(font);
		background_image.draw(150,0);
		//bottom_bar_image.draw(0,530);
		control_bar_image.draw(0,0);
		g.setColor(Color.white);
		a.render(g, gc);
		
		g.setColor(Color.white);
		clock_image.draw(0,5);
		g.drawString(this.string_time, 30, 10);

	}

	public void update(GameContainer gc, StateBasedGame sbg, int delta)
			throws SlickException {
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
		
		
		
		if (a.new_flight2(i, gc)) {
			i++;
		}
		a.update(gc);
		if (a.get_separation_rules().getGameOverViolation() == true){
			main_game_music.stop();
			end_of_game_sound.play();
			sbg.enterState(3);
		}
		
		Input input = gc.getInput();
		if (input.isKeyPressed(Input.KEY_P)) {
			sbg.enterState(4);
		}
		if(!gc.hasFocus()) {
			sbg.enterState(4);
		}
		gc.setUpdateOnlyWhenVisible(false);
		
		
		int posX = Mouse.getX();
		int posY = Mouse.getY();
		
		
		
		posX = Mouse.getX();
		posY = Mouse.getY();
		if(Mouse.isButtonDown(1)){
			if (a.get_selected_flight()!= null){
			Calculations.give_heading_with_mouse(posX, posY,a );
			}
		}
	

	}

	public int getID() {
		return 2;
	}

}