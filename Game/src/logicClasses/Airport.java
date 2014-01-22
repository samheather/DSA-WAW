package logicClasses;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;



public class Airport {
	
	//FIELDS
	
	Image airportImage;
	
	//CONSTRUCTOR
	
	Airport() {
		
	}
	
	public void init(GameContainer gc) throws SlickException {
		airportImage = new Image("res/graphics/airport.png");
	}
	
	public void render(Graphics g, GameContainer gc) throws SlickException { 
		
		airportImage.draw(572,197); // Airport image centred in middle of airspace
	} 
	
	
	
	

}
