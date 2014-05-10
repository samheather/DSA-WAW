package game.vision;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfRect;
import org.opencv.core.Rect;
import org.opencv.core.Size;
import org.opencv.highgui.VideoCapture;
//import org.opencv.highgui.Highgui;
import org.opencv.objdetect.CascadeClassifier;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.channels.FileChannel;
import java.util.Arrays;

/**
 * Class for providing a constant stream of positions of a users face with the
 * option for advanced error detection of outliers and error correction.
 * 
 * @author Samuel Heather (sam@heather.sh) © 2014 Samuel Heather - All Rights
 *         Reserved.
 */
public class LocateFace {

	private VideoCapture vc;
	private CascadeClassifier cascaseFaceDetectorProfile;
	private MatOfRect faces;

	/**
	 * Rect used to represent the first face found, that is returned when a face
	 * is requested via getLocation().
	 */
	private Rect mainFace;

	/** Minimum size that will be recognised as a face */
	private Size minSize;

	/** Maximum size that will be recognised as a face */
	private Size maxSize;

	/** Width of the camera image */
	private int imgWidth;

	/** Height of the camera image */
	private int imgHeight;

	/**
	 * Holds the last five detected X positions of the face for error
	 * detection/correction.
	 */
	private float lastFiveX[] = new float[5];

	/**
	 * Holds the last five detected Y positions of the face for error
	 * detection/correction.
	 */
	private float lastFiveY[] = new float[5];

	/**
	 * Holds the last five detected measurements of distance between the face
	 * and camera for error detection/correction.
	 */
	private float lastFiveDistance[] = new float[5];

	/** Initial value for lastFive arrays */
	private float initialValue = 9999.9f;

	/**
	 * Counter for concurrent outliers or errors. When a pre-determined val is
	 * reached, it is assumed the face position changed significantly and all
	 * past position history will be reset.
	 */
	private int outliersOrErrors = 0;
	
	private boolean webcamExists = true;
	public static void copyFile(File sourceFile, File destFile) throws IOException {
	    if(!destFile.exists()) {
	        destFile.createNewFile();
	    }

	    FileChannel source = null;
	    FileChannel destination = null;

	    try {
	        source = new FileInputStream(sourceFile).getChannel();
	        destination = new FileOutputStream(destFile).getChannel();
	        destination.transferFrom(source, 0, source.size());
	    }
	    finally {
	        if(source != null) {
	            source.close();
	        }
	        if(destination != null) {
	            destination.close();
	        }
	    }
	}
	/**
	 * Constructor for LocateFace - sets up OpenCV, sets min and max face sizes
	 * and pre-initialises the Last Datapoints Arrays.
	 */
	public LocateFace() {
		System.out.println("Constructor of face detector.");
		
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
		vc = new VideoCapture(0);
		
		if (!vc.isOpened()) {
			System.out.println("Camera stream could not open.");
			webcamExists = false;
		}
		
		cascaseFaceDetectorProfile = new CascadeClassifier(
				"/tmp/haarcascade_frontalface_default.xml");

		// Set min and max sizes of face to detect. May vary for System.
		minSize = new Size(125, 125);
		maxSize = new Size(700, 700);

		// Initialise the lastFive Arrays to a value that can never be generated
		// from Face Tracking
		Arrays.fill(lastFiveX, initialValue);
		Arrays.fill(lastFiveY, initialValue);
		Arrays.fill(lastFiveDistance, initialValue);
	}

	/**
	 * Called whenever a new face location is requested by the Vision class.
	 * 
	 * @return boolean successful
	 */
	public boolean updateFacePosition() {
		
		if (!webcamExists) {
			return false;
		}
		
		// Read image from camera into matrix.
		Mat img = new Mat();
		if (!vc.read(img)) {
			System.out
					.println("Error capturing image from CameraCapture - no camera detected.");
			return false;
		}
		imgWidth = img.width();
		imgHeight = img.height();

		// TODO(samheather) Convert image to gray+make smaller for ++er fps?
		faces = new MatOfRect();
		cascaseFaceDetectorProfile.detectMultiScale(img, faces, 1.05, 6, 0,
				minSize, maxSize);

		if (!faces.empty()) {
			mainFace = faces.toArray()[0];

			// Add to Average Arrays
			addToLastFive(getImmediateDistance(false), lastFiveDistance);
			addToLastFive(getImmediateHorizontalAngle(false), lastFiveX);
			addToLastFive(getImmediateVerticalAngle(false), lastFiveY);
			return true;
		} else {
			outliersOrErrors++;
			return false;
		}
	}

	/**
	 * Adds a new value to an array of the previous 5 values for a parameter,
	 * after shuffling the previous elements to accommodate this.
	 * 
	 * @param newValue
	 * @param lastFive
	 */
	public void addToLastFive(float newValue, float[] lastFive) {
		// Check if resetting outliers
		if (outliersOrErrors > 100) {
			outliersOrErrors = 0;
			Arrays.fill(lastFive, 9999.9f);
			System.out.println("\n\n\n\n\nResetting - Too many outliers\n\n");
		}
		// Check not an outlier - if last face position more than 10% different
		// to previous average.
		if (Math.abs(meanOfLastFive(lastFive) - newValue) > imgWidth / 10
				&& meanOfLastFive(lastFive) != 0) {
			System.out.println("Outlier detected");
			outliersOrErrors += 10;
			return;
		}
		outliersOrErrors = 0;
		for (int i = 3; i >= 0; i--) {
			lastFive[i + 1] = lastFive[i];
		}
		lastFive[0] = newValue;
	}

	/**
	 * Returns the mean of the 5 objects in the Last Datapoints arrays, ignoring
	 * (and accommodating for) unfilled spaces in the array.
	 * 
	 * @param lastFive
	 * @return
	 */
	public float meanOfLastFive(float[] lastFive) {
		float count = 0;
		int divisor = 0;
		for (float f : lastFive) {
			if (f != initialValue) {
				count += f;
				divisor++;
			}
		}
		if (divisor == 0) {
			System.out.println("Divisor is 0, returning 0");
			return 0f;
		}
		return count / divisor;
	}

	/**
	 * Returns a representation of how far the face is from the camera.
	 * 
	 * @return float distance
	 */
	public float getImmediateDistance(boolean errorCorrectUsingMean) {
		if (errorCorrectUsingMean) {
			return meanOfLastFive(lastFiveDistance);
		}
		return (float) mainFace.height;
	}

	/**
	 * Returns a representation of the horizontal angle between the face and the
	 * line perpendicular to the camera.
	 * 
	 * @return float horizontalAngle
	 */
	public float getImmediateHorizontalAngle(boolean errorCorrectUsingMean) {
		if (errorCorrectUsingMean) {
			return meanOfLastFive(lastFiveX);
		}
		return mainFace.x + (mainFace.width / 2) - (imgWidth / 2);
	}

	/**
	 * Returns a representation of the vertical angle between the face and the
	 * line perpendicular to the camera.
	 * 
	 * @return float verticalAngle
	 */
	public float getImmediateVerticalAngle(boolean errorCorrectUsingMean) {
		if (errorCorrectUsingMean) {
			return meanOfLastFive(lastFiveY);
		}
		return mainFace.y + (mainFace.height / 2) - (imgHeight / 2);
	}

}
