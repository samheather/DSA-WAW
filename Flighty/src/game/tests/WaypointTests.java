package game.tests;


import org.junit.Test;
import static org.junit.Assert.*;

import game.struct.Waypoint;


public class WaypointTests {

	@Test
	public void testCreateWaypointValues() {
		// Pre: 	wp = test waypoint
		//			wp.prev = prevWp
		//			wp.next = nextWp
		//
		// Post:	wp.prev = newWp
		//			wp.next = nextWp
		//			newWp.prev = prevWp
		//			newWp.next = wp

		// Create and set wp, prevWp and NextWp
		Waypoint wp = new Waypoint(0d, 0d);
		Waypoint prevWp = new Waypoint(0d, 0d);
		Waypoint nextWp = new Waypoint(0d, 0d);

		assertTrue("Waypoint instance 'Current' could not be created",
				wp instanceof Waypoint);
		assertTrue("Waypoint instance 'Previous' could not be created",
				prevWp instanceof Waypoint);
		assertTrue("Waypoint instance 'Next' could not be created",
				nextWp instanceof Waypoint);

		wp.setPrev(prevWp);
		wp.setNext(nextWp);

		// Create newWp
		Waypoint newWp = wp.createWaypoint(0d, 0d);

		assertTrue("Waypoint instance 'New' could not be created",
				newWp instanceof Waypoint);

		// Check added correctly
		assertTrue("Current Waypoint's previous Waypoint not set correctly",
				wp.getPrev().equals(newWp));
		assertTrue("Current Waypoint's next Waypoint not set correctly",
				wp.getNext().equals(nextWp));
		assertTrue("New Waypoint's previous Waypoint not set correctly",
				newWp.getPrev().equals(prevWp));
		assertTrue("New Waypoint's next Waypoint not set correctly",
				newWp.getNext().equals(wp));
	}

	public void testCreateWaypointPrevNull() {
		// Pre: 	wp = test waypoint
		//			wp.prev = prevWp
		//			wp.next = nextWp
		//
		// Post:	wp.prev = newWp
		//			wp.next = nextWp
		//			newWp.prev = prevWp
		//			newWp.next = wp

		// Create and set wp, prevWp and NextWp
		Waypoint wp = new Waypoint(0d, 0d);
		Waypoint prevWp = null;
		Waypoint nextWp = new Waypoint(0d, 0d);

		assertTrue("Waypoint instance 'Current' could not be created",
				wp instanceof Waypoint);
		assertTrue("Waypoint instance 'Next' could not be created",
				nextWp instanceof Waypoint);

		wp.setPrev(prevWp);
		wp.setNext(nextWp);

		// Create newWp
		Waypoint newWp = wp.createWaypoint(0d, 0d);

		assertTrue("Waypoint instance 'New' could not be created",
				newWp instanceof Waypoint);

		// Check added correctly
		assertTrue("Current Waypoint's previous Waypoint not set correctly",
				wp.getPrev().equals(newWp));
		assertTrue("Current Waypoint's next Waypoint not set correctly",
				wp.getNext().equals(nextWp));
		assertTrue("New Waypoint's previous Waypoint not set correctly",
				newWp.getPrev().equals(prevWp));
		assertTrue("New Waypoint's next Waypoint not set correctly",
				newWp.getNext().equals(wp));
	}

	public void testCreateWaypointNextNull() {
		// Pre: 	wp = test waypoint
		//			wp.prev = prevWp
		//			wp.next = nextWp
		//
		// Post:	wp.prev = newWp
		//			wp.next = nextWp
		//			newWp.prev = prevWp
		//			newWp.next = wp

		// Create and set wp, prevWp and NextWp
		Waypoint wp = new Waypoint(0d, 0d);
		Waypoint prevWp = new Waypoint(0d, 0d);
		Waypoint nextWp = null;

		assertTrue("Waypoint instance 'Current' could not be created",
				wp instanceof Waypoint);
		assertTrue("Waypoint instance 'Previous' could not be created",
				prevWp instanceof Waypoint);

		wp.setPrev(prevWp);
		wp.setNext(nextWp);

		// Create newWp
		Waypoint newWp = wp.createWaypoint(0d, 0d);

		assertTrue("Waypoint instance 'New' could not be created",
				newWp instanceof Waypoint);

		// Check added correctly
		assertTrue("Current Waypoint's previous Waypoint not set correctly",
				wp.getPrev().equals(newWp));
		assertTrue("Current Waypoint's next Waypoint not set correctly",
				wp.getNext().equals(nextWp));
		assertTrue("New Waypoint's previous Waypoint not set correctly",
				newWp.getPrev().equals(prevWp));
		assertTrue("New Waypoint's next Waypoint not set correctly",
				newWp.getNext().equals(wp));
	}

	public void testCreateWaypointBothNull() {
		// Pre: 	wp = test waypoint
		//			wp.prev = prevWp
		//			wp.next = nextWp
		//
		// Post:	wp.prev = newWp
		//			wp.next = nextWp
		//			newWp.prev = prevWp
		//			newWp.next = wp

		// Create and set wp, prevWp and NextWp
		Waypoint wp = new Waypoint(0d, 0d);
		Waypoint prevWp = null;
		Waypoint nextWp = null;

		assertTrue("Waypoint instance 'Current' could not be created",
				wp instanceof Waypoint);

		wp.setPrev(prevWp);
		wp.setNext(nextWp);

		// Create newWp
		Waypoint newWp = wp.createWaypoint(0d, 0d);

		assertTrue("Waypoint instance 'New' could not be created",
				newWp instanceof Waypoint);

		// Check added correctly
		assertTrue("Current Waypoint's previous Waypoint not set correctly",
				wp.getPrev().equals(newWp));
		assertTrue("Current Waypoint's next Waypoint not set correctly",
				wp.getNext().equals(nextWp));
		assertTrue("New Waypoint's previous Waypoint not set correctly",
				newWp.getPrev().equals(prevWp));
		assertTrue("New Waypoint's next Waypoint not set correctly",
				newWp.getNext().equals(wp));
	}

	@Test
	public void testDeleteWaypoint() {
		// Pre:		wp = test waypoint
		//			wp.prev = prevWp
		//			wp.next = nextWp
		//			prevWp != null
		//			nextWp != null
		//
		// Post:	prevWp.next = nextWp
		//			nextWp.prev = prevWp

		// Create and set wp, prevWp, nextWp
		Waypoint wp = new Waypoint(0d, 0d);
		Waypoint prevWp = new Waypoint(0d, 0d);
		Waypoint nextWp = new Waypoint(0d, 0d);
		
		assertTrue("Waypoint instance 'Current' could not be created",
				wp instanceof Waypoint);
		assertTrue("Waypoint instance 'Previous' could not be created",
				prevWp instanceof Waypoint);
		assertTrue("Waypoint instance 'Next' could not be created",
				nextWp instanceof Waypoint);

		wp.setPrev(prevWp);
		wp.setNext(nextWp);

		// Delete wp
		wp.deleteWaypoint();

		// Check deleted correctly
		assertTrue("Previous Waypoint's next Waypoint not set correctly",
				prevWp.getNext().equals(nextWp));
		assertTrue("Next Waypoint's next previous not set correctly",
				nextWp.getPrev().equals(prevWp));
	}

	@Test
	public void testDeleteWaypointPrevNull() {
		// Pre:		wp = test waypoint
		//			wp.prev = prevWp
		//			wp.next = nextWp
		//			prevWp = null
		//			nextWp != null
		//
		// Post:	nextWp.prev = null

		// Create and set wp, prevWp, nextWp
		Waypoint wp = new Waypoint(0d, 0d);
		Waypoint prevWp = null;
		Waypoint nextWp = new Waypoint(0d, 0d);
		
		assertTrue("Waypoint instance 'Current' could not be created",
				wp instanceof Waypoint);
		assertTrue("Waypoint instance 'Next' could not be created",
				nextWp instanceof Waypoint);

		wp.setPrev(prevWp);
		wp.setNext(nextWp);

		// Delete wp
		wp.deleteWaypoint();

		// Check deleted correctly
		assertTrue("Next Waypoint's next previous not set correctly",
				nextWp.getPrev() == null);
	}

	@Test
	public void testDeleteWaypointNextNull() {
		// Pre:		wp = test waypoint
		//			wp.prev = prevWp
		//			wp.next = nextWp
		//			prevWp != null
		//			nextWp = null
		//
		// Post:	prevWp.next = null

		// Create and set wp, prevWp, nextWp
		Waypoint wp = new Waypoint(0d, 0d);
		Waypoint prevWp = new Waypoint(0d, 0d);
		Waypoint nextWp = null;
		
		assertTrue("Waypoint instance 'Current' could not be created",
				wp instanceof Waypoint);
		assertTrue("Waypoint instance 'Previous' could not be created",
				prevWp instanceof Waypoint);

		wp.setPrev(prevWp);
		wp.setNext(nextWp);

		// Delete wp
		wp.deleteWaypoint();

		// Check deleted correctly
		assertTrue("Previous Waypoint's next Waypoint not set correctly",
				prevWp.getNext() == nextWp);
	}

	@Test
	public void testDeleteWaypointBothNull() {
		// Pre:		wp = test waypoint
		//			wp.prev = prevWp
		//			wp.next = nextWp
		//			prevWp = null
		//			nextWp = null
		//
		// Post:	prevWp.next = nextWp
		//			nextWp.prev = prevWp

		// Create and set wp, prevWp, nextWp
		Waypoint wp = new Waypoint(0d, 0d);
		Waypoint prevWp = null;
		Waypoint nextWp = null;
		
		assertTrue("Waypoint instance 'Current' could not be created",
				wp instanceof Waypoint);

		wp.setPrev(prevWp);
		wp.setNext(nextWp);

		// Delete wp
		wp.deleteWaypoint();

		// Check deleted correctly
		assertTrue("Assert true",
				true);
	}
}
