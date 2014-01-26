package game.tests;


import org.junit.runner.RunWith;
import org.junit.runners.Suite;


@RunWith(Suite.class)
@Suite.SuiteClasses({GameTests.class,
						PlaneTests.class,
						WaypointTests.class,
						})
public class AllTests {
	// Runs all tests
}
