package game.struct;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

public class SaveFile {

	private int level2UnlockScore = 2;

	private int level3UnlockScore = 300;

	private boolean level2Unlocked = false;

	private boolean level3Unlocked = false;

	private String statsFileName = "stats.txt";

	public void readStats() {
		try {
			ObjectInputStream ois = new ObjectInputStream(new FileInputStream(
					statsFileName));
			level2Unlocked = ((boolean)ois.readObject());
			level3Unlocked = ((boolean)ois.readObject());
			ois.close();
			System.out.println("Read");
			System.out.println(level2Unlocked);
		} catch (Exception ex) {
			System.out.println("Saving stats raised exception.");
		}
	}

	public void saveStats() {
		try {
			// create a new file with an ObjectOutputStream
			FileOutputStream out = new FileOutputStream(statsFileName);
			ObjectOutputStream oout = new ObjectOutputStream(out);

			// write something in the file
			oout.writeObject(level2Unlocked);
			oout.writeObject(level3Unlocked);
			oout.close();
			readStats();
		} catch (Exception ex) {
			System.out.println("Saving stats raised exception.");
		}
	}

	// ACCESSORS

	public boolean getLevel2Unlock() {
		return level2Unlocked;
	}

	public boolean getLevel3Unlock() {
		return level3Unlocked;
	}

	public int getLevel2UnlockScore() {
		return level2UnlockScore;
	}

	public int getLevel3UnlockScore() {
		return level3UnlockScore;
	}

	// SETTERS
	public void setLevel2Unlock(boolean unlocked) {
		level2Unlocked = unlocked;
	}

	public void setLevel3Unlock(boolean unlocked) {
		level3Unlocked = unlocked;
	}

	// Initialise stats in game
	public SaveFile() {
		File statsCheckFile = new File(statsFileName);
		// If game run before and stats exist:
		if (statsCheckFile.exists()) {
			readStats(); // DO NOT change the stats before this call
		} else {
			saveStats();
		}
	}

}
