package game.tests;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({ FlightPlanTests.class, PlaneTests.class, GameTests.class,
		ScoreTests.class, MultiplayerTests.class

})
public class AllTests {
	// Runs all tests
}
