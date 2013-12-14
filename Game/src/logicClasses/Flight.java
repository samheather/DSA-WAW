package logicClasses;
import java.util.Random;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.SlickException;

public class Flight {

	// FIELDS
	private double x, y, target_altitude, current_heading, target_heading;
	private int current_altitude, flight_num;
	private boolean turning_right, turning_left;
	private String flight_name;
	private FlightPlan flight_plan;
	private Image img, selected_img, slow_flight_img, fast_flight_img;
	private Color color;
	private boolean selected;
	
	private EntryPoint entryPoint;
	private Airspace airspace;
	

	// CONSTRUCTOR
	public Flight(Airspace airspace) {
		this.x = 0;
		this.y = 0;
		this.target_altitude = 0;
		this.current_altitude = generate_altitude();
		this.target_heading = 0;
		this.current_heading = 0;
		this.turning_right = false;
		this.turning_left = false;
		this.airspace = airspace;
		this.entryPoint = generate_entry_point();
		this.flight_plan = new FlightPlan(airspace, this.entryPoint);
		this.color = Color.white;
		this.selected = false;

	}

	// METHODS
	
	public EntryPoint generate_entry_point(){
		
		Random rand = new Random();
		int random_number = rand.nextInt(3);
		
		if (this.airspace.getList_of_entry_points().size() == 3) { // if we have all three entrypoints
			this.airspace.getList_of_entrypoints().get(random_number);
			this.x = this.airspace.getList_of_entrypoints().get(random_number).getX();// choose one a get the x and y values
			this.y = this.airspace.getList_of_entrypoints().get(random_number).getY();
		} else { // if all entrypoints are not there then just assign some values
			this.x = 0;
			this.y = 0;

		}
		
		return this.airspace.getList_of_entrypoints().get(random_number);
		
	}

	public int generate_altitude() {
		Random rand = new Random();
		int check = rand.nextInt(3);
		switch(check) {
		case 0:
			return 28000;
		case 1:
			return 29000;
		case 2:
			return 30000;
		}
		return 27000;
	}

//	public boolean check_other_flight_selection(Airspace a) {
//		for (int i = 0; i < a.getList_of_flights().size(); i++) {
//			if (a.getList_of_flights().get(i) != this) {
//				if (a.getList_of_flights().get(i).isSelected()) {
//					return true;
//				}
//			}
//		}
//		return false;
//	}

	public double calculate_heading_to_first_waypoint(double destination_x, double destination_y) {
		double deltaX;
		double deltaY;
		deltaY = destination_y - this.y;
		deltaX = destination_x - this.x;
		double angle = Math.toDegrees(Math.atan2(deltaY, deltaX));
		angle += 90;
		if (angle < 0) {
			angle += 360;
		}
		return angle;
	}
	
	public void turn_flight_left(int degree_turned_by) {

		this.turning_right = false;
		this.turning_left = true;

		this.target_heading = Math.round(this.current_heading) - degree_turned_by;
		if(this.target_heading < 0){
			this.target_heading = 360 +this.target_heading;
		}
	}
	
	
	public void turn_flight_right(int degree_turned_by) {

		this.turning_left = false;
		this.turning_right = true;
		
		this.target_heading = Math.round(this.current_heading) + degree_turned_by;
		if(this.target_heading >= 360){
			this.target_heading = this.target_heading - 360;
		}


	}

	public void give_heading(int new_heading) {
		this.turning_right = false;
		this.turning_left = false;
		new_heading = new_heading % 360;
		this.target_heading = new_heading;
	}
	
	
	public boolean check_if_flight_at_waypoint(Point waypoint) {
		
		if (((Math.abs(Math.round(this.x) - Math.round(waypoint.getX()))) <= 15)
				&& (Math.abs(Math.round(this.y) - Math.round(waypoint.getY()))) <= 15) {
			return true;
		}

		return false;
	}

	
	
	// DRAWING METHODS
	
	public void draw_flight(Graphics g, GameContainer gc ){
		
				g.setColor(color);
				g.setWorldClip(150, 0, 1200, 600);
				
				if(this.flight_plan.getVelocity() <= 275){
					
					slow_flight_img.setRotation((int) current_heading);
					slow_flight_img.draw((int) this.x-10, (int) this.y-10);
					
					
				}
				
				else if(this.flight_plan.getVelocity() > 270 && this.flight_plan.getVelocity() < 340){
					
					img.setRotation((int) current_heading);
					img.draw((int) this.x-10, (int) this.y-10);
				}
				
				else{
					fast_flight_img.setRotation((int) current_heading);
					fast_flight_img.draw((int) this.x-10, (int) this.y-10);
				}
				
				
				if (this.selected){
					
					g.setColor(Color.yellow);
					g.drawString(this.flight_name, (int) this.x-24, (int) this.y-44);
					g.drawString(Math.round(this.current_altitude) + "ft",(int) this.x-30, (int) this.y + 10);
					g.drawString(Math.round(this.current_heading) + " dg",(int) this.x - 22, (int) this.y + 25);//-15,20
					
					if (this.flight_plan.getWaypoints().size() > 0) {
						g.drawString("Aim: "+this.flight_plan.getPointByIndex(0).getPointRef(),(int) this.x -26, (int)this.y-28);
					}
					
					g.drawOval((int) this.x - 50, (int) this.y - 50, 100, 100);
					g.setColor(color);
					
				}
				
				else{
					g.drawString(this.flight_name, (int) this.x-24, (int) this.y-44);
					g.drawString(Math.round(this.current_altitude) + "ft",(int) this.x-30, (int) this.y + 10);
					
					if (this.flight_plan.getWaypoints().size() > 0) {
						g.drawString("Aim: "+this.flight_plan.getPointByIndex(0).getPointRef(),(int) this.x -26, (int)this.y-28);
					}
					g.drawOval((int) this.x - 50, (int) this.y - 50, 100, 100);
				}
				

				
				g.setWorldClip(0, 0, 1200, 600);
		
	}
	
	public void draw_selected_flight_information(Graphics g, GameContainer gc) {
		
		this.selected_img.draw(0,450);
		g.setColor(Color.white);
		g.drawString(this.flight_name,  10, 460);
		g.drawString("Plan: ",  10, 480);
		String plan = "";
		
		for(int i=0; i<this.flight_plan.getWaypoints().size(); i++) {
			plan += this.flight_plan.getWaypoints().get(i).getPointRef()+", ";
		}
		
		
		g.setColor(Color.yellow);
		g.drawString(plan, 10, 500);
		g.setColor(Color.white);
		g.drawString(Math.round(this.current_altitude) + " ft",
			 10, 520);
		g.drawString(Math.round(this.current_heading) + " deg",
			10, 540);
		g.drawString(Math.round(this.getFlight_plan().getVelocity()) + " MPH",
			10, 560);
		
	}
	
	// UPDATE METHODS
	

	public void update_x_y_coordinates() {
		double velocity = (this.flight_plan.getVelocity()) / 1000;

		this.x += velocity * Math.sin(Math.toRadians(this.current_heading));

		this.y -= velocity * Math.cos(Math.toRadians(this.current_heading));

	}

	public void update_altitude() {
		if (this.current_altitude > this.target_altitude) {
			this.current_altitude -= 1;
		}

		else if (this.current_altitude < this.target_altitude) {
			this.current_altitude += 1;
		}
	}

	public void update_current_heading() {
	
		double rate = 0.5;
		if (Math.round(this.target_heading) != Math.round(this.current_heading)) {
			if (this.turning_right == true) {// If plane is already turning
												// right or user has told it to
												// turn right
				this.current_heading += rate;
				if (Math.round(this.current_heading) == 360
						&& this.target_heading != 360) {
					this.current_heading = 0;
				}
			}

			// if plane is already turning left or user has told it to turn left
			else if (this.turning_left == true) {
				this.current_heading -= rate;
				if (Math.round(this.current_heading) == 0
						&& this.target_heading != 0) {
					this.current_heading = 360;
				}
			}

			// If plane has been given a heading so no turning direction specified
			// Below works out whether it should turn left or right to that heading.
			else {

				if (this.target_heading - this.current_heading == 180) {
					this.turning_right = true;
					this.current_heading += rate;
				} else if ((this.current_heading + 180) >= 359) {

					if (this.target_heading > this.current_heading) {
						this.turning_right = true;
						this.current_heading += rate;
						if (Math.round(this.current_heading) == 360) {
							this.current_heading = 0;
						}
					} else if ((180 - (360 - this.current_heading)) > this.target_heading) {
						this.turning_right = true;
						this.current_heading += rate;
						if (Math.round(this.current_heading) == 360) {
							this.current_heading = 0;
						}
					} else {
						this.turning_left = true;
						this.current_heading -= rate;
						if (Math.round(this.current_heading) == 0) {
							this.current_heading = 360;
						}
					}
				} else {
					if ((this.target_heading > this.current_heading)
							&& (this.target_heading < this.current_heading + 180)) {
						this.turning_right = true;
						this.current_heading += rate;
						if (Math.round(this.current_heading) == 360) {
							this.current_heading = 0;
						}
					} else {
						this.turning_left = true;
						this.current_heading -= rate;
						if (Math.round(this.current_heading) == 0) {
							this.current_heading = 360;
						}
					}
				}
			}
		}
	}
	

	
	public void update_flight_plan(){
		
		if (this.flight_plan.getWaypoints().size() > 0) {
			if (this.check_if_flight_at_waypoint(flight_plan.getWaypoints()
					.get(0))) {
				this.flight_plan.getWaypoints().remove(0);
			}
		}
		
	}



	// UPDATE, RENDER, INIT
	
	
	public void init(GameContainer gc) throws SlickException {
		this.img = new Image("/res/graphics/graphics/flight.png");
		this.slow_flight_img = new Image("/res/graphics/graphics/flight_slow.png");
		this.fast_flight_img = new Image("/res/graphics/graphics/flight_fast.png");
		this.selected_img = new Image("res/graphics/graphics/selected_flight2.jpg");
		
	

	}
	
	


	public void update() {


		this.update_current_heading();
		this.update_x_y_coordinates();
		this.update_altitude();
		this.update_flight_plan();
		

	}
	

	public void render(Graphics g, GameContainer gc) throws SlickException {
		
		this.draw_flight(g,  gc);
		

		if(this.selected) {
			
			this.draw_selected_flight_information(g, gc);
			
			
		
		}
		
	}
	

	// MUTATORS AND ACCESSORS
	

	public double getX() {
		return this.x;
	}

	public void setX(double x) {
		this.x = x;
	}

	public double getY() {
		return this.y;
	}

	public void setY(double y) {
		this.y = y;
	}

	public double getCurrent_heading() {
		return this.current_heading;
	}

	public void setCurrent_heading(double current_heading) {
		this.current_heading = current_heading;
	}

	public double getTarget_heading() {
		return this.target_heading;
	}

	public void setTarget_heading(double target_heading) {
		this.target_heading = target_heading;
	}

	public double getTarget_altitude() {
		return this.target_altitude;
	}

	public void setTarget_altitude(double target_altitude) {
		this.target_altitude = target_altitude;
	}

	public int getAltitude() {
		return this.current_altitude;
	}

	public void setAltitude(int altitude) {
		this.current_altitude = altitude;
	}

	public boolean isTurning_right() {
		return this.turning_right;
	}

	public void setTurning_right(boolean turning_right) {
		this.turning_right = turning_right;
	}

	public boolean isTurning_left() {
		return this.turning_left;
	}

	public void setTurning_left(boolean turning_left) {
		this.turning_left = turning_left;
	}

	public void setFlight_num(int i) {
		this.flight_num = i;
	}

	public int getFlight_num() {
		return flight_num;
	}

	public String getFlight_name() {
		return flight_name;
	}

	public void setFlight_name(String flight_name) {
		this.flight_name = flight_name;
	}

	public boolean isSelected() {
		return selected;
	}

	public void setSelected(boolean selected) {
		this.selected = selected;
	}

	// tostring function to display a flight object so we can read it
	@Override
	public String toString() {
		return "X: " + this.x + " Y: " + this.y + " Flight Number: "
				+ this.flight_num;
	}

	public int getCurrent_altitude() {
		return current_altitude;
	}

	public void setCurrent_altitude(int current_altitude) {
		this.current_altitude = current_altitude;
	}

	public FlightPlan getFlight_plan() {
		return flight_plan;
	}
	
	public EntryPoint getEntryPoint(){
		return this.entryPoint;
	}
	

}
