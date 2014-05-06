package game.struct;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.URL;
import java.net.URLConnection;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import game.gfx.WindowManager;

public class SaveFile {

	/** Score needed to unlock level 2*/
	private int level2UnlockScore = 200;

	/** Score needed to unlock level 3*/
	private int level3UnlockScore = 300;

	/** Stores if level 2 is unlocked*/
	private boolean level2Unlocked = false;

	/** Stores if level 3 is unlocked*/
	private boolean level3Unlocked = false;

	/** Name of file to store the stats in*/
	private String statsFileName = "stats.txt";

	/** Gets leaderboard reading from our PHP page in JSON format. */
	public String getLeaderboardScores() {
		try {
			URL url = new URL("http://atcga.me/Leaderboard.php");
			URLConnection con = url.openConnection();
			InputStream is = con.getInputStream();
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			byte[] buf = new byte[1024];
			int len = 0;
			while ((len = is.read(buf)) != -1) {
				baos.write(buf, 0, len);
			}
			String body = new String(baos.toByteArray(), "UTF-8");
			return body;
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}

	/** Decodes JSON and updates leaderboard */
	public void decodeLeaderboardScores(String s) {
		String[] scores = s.split("}");
		JSONParser parser = new JSONParser();
		String name;
		long score;
		for (int i = 0; i < scores.length; i++) {
			scores[i] = scores[i] + "}";
			try {
				Object obj = parser.parse(scores[i]);
				JSONObject jsonObject = (JSONObject) obj;
				score = ((Long) jsonObject.get("score")).longValue();
				if (WindowManager.leaderBoard.leaderboardEntries[4].getScore() <= score) {
					name = (String) jsonObject.get("name");
					WindowManager.leaderBoard.addLeaderboardEntry(name, score);
				}

			} catch (ParseException e) {
				e.printStackTrace();
			}
		}

	}

	/** Adds score to our online leaderboard */
	public String addLeaderboardScore(String name, int score) {
		try {
			URL url = new URL("http://atcga.me/Leaderboard.php?name=" + name
					+ "&score=" + score);
			URLConnection con = url.openConnection();
			InputStream is = con.getInputStream();
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			byte[] buf = new byte[1024];
			int len = 0;
			while ((len = is.read(buf)) != -1) {
				baos.write(buf, 0, len);
			}
			String body = new String(baos.toByteArray(), "UTF-8");

			return body;
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * Gets lowest score in leaderboard
	 * @return Long of the lowest score in the top 5 leaderboard
	 */
	public Long getLowestScore() {
		String s = getLeaderboardScores();
		String[] scores = s.split("}");
		JSONParser parser = new JSONParser();
		scores[4] = scores[4] + "}";
		try {
			Object obj = parser.parse(scores[4]);
			JSONObject jsonObject = (JSONObject) obj;
			return (((Long) jsonObject.get("score")).longValue());
		} catch (ParseException e) {
			e.printStackTrace();
			return (long) 0;
		}
	}

	/**
	 * Read information about level unlocks from file
	 */
	public void readStats() {
		try {
			ObjectInputStream ois = new ObjectInputStream(new FileInputStream(
					statsFileName));
			level2Unlocked = ((Boolean) ois.readObject()).booleanValue();
			level3Unlocked = ((Boolean) ois.readObject()).booleanValue();
			ois.close();
		} catch (Exception ex) {
			System.out.println("Saving stats raised exception.");
		}
	}

	/** Saves level unlocks to file */
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

	/** Initialise stats in game */
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
