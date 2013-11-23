import org.newdawn.slick.*;
import org.newdawn.slick.state.*;
import org.lwjgl.input.Mouse;
import java.awt.Font;

public class Menu extends BasicGameState {
	private TrueTypeFont font;
	private TrueTypeFont font2;
	private Color colorBtn1 = Color.green;
	private Color colorBtn2 = Color.green;
	private Image img;
	private int imgX = 700;
	private int imgY = 550;
	private int velX=5;
	private int velY=-10;
	
	
	
	public Menu(int state) {
		
	}
	
	public void init(GameContainer gc, StateBasedGame sbj) throws SlickException {
		Font awtFont = new Font("Courier",Font.BOLD,30);
		font = new TrueTypeFont(awtFont, false);
		Font awtFont2 = new Font("Courier",Font.BOLD,25);
		font2 = new TrueTypeFont(awtFont2, false);
		gc.setShowFPS(true);
		img = new Image("res/plane.png");
		
	}
	
	public void render(GameContainer gc, StateBasedGame sbj, Graphics g) throws SlickException {
		g.setColor(Color.white);
		g.setFont(font);
		g.drawString("Pretty Fly For A Flight Guy", 10, 40);
		g.setColor(colorBtn1);
		g.fillRoundRect(100, 100, 200, 70, 15);
		g.setColor(colorBtn2);
		g.fillRoundRect(100, 200, 200, 70, 15);
		img.draw(imgX,imgY);
		g.setColor(Color.black);
		g.setFont(font2);
		g.drawString("Play Game",135,115);
		g.drawString("Exit Game", 135, 215);
		g.setColor(Color.white);
		
	}
	
	public void update(GameContainer gc, StateBasedGame sbg, int delta) throws SlickException {
		Input input = gc.getInput();
		if(input.isKeyDown(Input.KEY_SPACE)) {
			int currVelX = velX;
			int currVelY = velY;
			velX=0;
			velY=0;
			imgX+=velX;
			imgY+=velY;
			velX=currVelX;
			velY=currVelY;
		}
		else {
			imgX+=velX;
			imgY+=velY;
			if(imgX>=gc.getWidth()-12) {
				velX=-(velX);
			}
			if(imgX<=600) {
				velX=-(velX);
			}
			if(imgY<=560) {
				velY=-(velY);
			}
			if(imgY>=0) {
				velY=-(velY);
			}
		}
		int posX = Mouse.getX();
		int posY = Mouse.getY();
		
		if((posX>100&&posX<300)&&(posY>430&&posY<500)) {
			colorBtn1=Color.white;
			if(Mouse.isButtonDown(0)) {
				sbg.enterState(1);
			}
			
		}
		else {
			colorBtn1=Color.green;
		}
		if((posX>100&&posX<300)&&(posY>330&&posY<400)) {
			colorBtn2=Color.white;
			if(Mouse.isButtonDown(0)) {
				System.exit(0);
			}
			
		}
		else {
			colorBtn2=Color.green;
		}
	}
	public int getID() {
		return 0;
	}
	
}
