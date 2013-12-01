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

public class Play extends BasicGameState {

	private Airspace a;
	private int i;
	Image cursorImg;
	public static int time;
	private Music main_game_music;
	public static TrueTypeFont font;

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
		arg0.setAlwaysRender(true);
		a.init(arg0);
		main_game_music = new Music("res/Love Song In My Mind.wav");
		main_game_music.loop();
		main_game_music.setVolume(0.2f);
		a.new_waypoint(150, 150);
		a.new_waypoint(400, 470);
		a.new_waypoint(700, 60);
		a.new_waypoint(800, 320);
		a.new_waypoint(600, 418);
		a.new_waypoint(500, 220);
		a.new_waypoint(950, 188);
		a.new_waypoint(1050, 272);
		a.new_waypoint(900, 420);
		a.new_waypoint(240, 250);
		a.new_entry_point(1200, 400);
		a.new_entry_point(1200, 200);
		a.new_entry_point(600, 0);
		a.new_exit_point(800, 0);
		a.new_exit_point(100, 200);
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
		g.setColor(Color.black);
		g.fillRect(0, 0, (float) 1200, (float) 600);
		g.setColor(Color.lightGray);
		g.fillRect(0, 500, 1200, 100);
		g.fillRect(0, 0, 100, 600);
		g.setColor(Color.white);
		g.drawLine(100, 0, 100, 500);
		g.drawLine(100, 500, 1200, 500);
		a.render(g, gc);
		g.setColor(Color.black);
		g.drawString("Time: " + time/1000, 0, 20);

	}

	public void update(GameContainer gc, StateBasedGame sbg, int delta)
			throws SlickException {
		time += delta;
		if (a.new_flight2(i, gc)) {
			i++;
		}
		a.update(gc);
		if (a.get_separation_rules().getGameOverViolation() == true){
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
		if(Mouse.isButtonDown(0)){
			Calculations.check_selected(posX, posY, a);
		}
		
		
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