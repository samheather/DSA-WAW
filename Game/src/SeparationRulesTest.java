import static org.junit.Assert.*;


import org.junit.Test;


public class SeparationRulesTest {
	
	SeparationRules testing = new SeparationRules();
	Airspace testAir = new Airspace();
	
	@Test
	public void testSetGOLat() {
		
		testing.setGameOverLateralSeparation(1000);
		System.out.println(testing.getGameOverLateralSeparation());
	}
	
	@Test
	public void testSetGOVert(){
		
		testing.setGameOverVerticalSeparation(1500);
		System.out.println(testing.getGameOverVerticalSeparation());
	}
	
	@Test
	public void testCheckViolation(){
		
		testing.checkViolation()
	}
}
