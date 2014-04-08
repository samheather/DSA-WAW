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

public class LocateFace {
	
	private VideoCapture vc;
	private CascadeClassifier cascaseFaceDetectorProfile;
	private MatOfRect faces;
	/**
	 * Rect used to represent the first face found, that is returned when a face
	 * is requested via getLocation().
	 */
	private Rect mainFace;
	private Size minSize;
	private Size maxSize;
	private int imgWidth;
	private int imgHeight;

	public LocateFace() {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
		vc = new VideoCapture(0);
		
		// Load the face detection haarcascade file in
		File cascadeFile = new File("res/haarcascade_frontalface_default.xml");
		cascaseFaceDetectorProfile = new CascadeClassifier(
				cascadeFile.getAbsolutePath());
		
		// Set min and max sizes of face to detect.  May vary for System.
		minSize = new Size(125,125);
		maxSize = new Size(700,700);
	}
	
	/**
	 * Called whenever a new face location is requested by the Vision class.
	 * @return boolean successful
	 */
	public boolean getLocation() {
		// Read image from camera into matrix.
		Mat img = new Mat();
		if(!vc.read(img)) {
			System.out.println("Error capturing image from CameraCapture");
			return false;
		}
		imgWidth = img.width();
		imgHeight = img.height();
		
		//ToDo(samheather) Convert image to gray+make smaller for ++er fps?
		faces = new MatOfRect();
		cascaseFaceDetectorProfile.detectMultiScale(img, faces, 1.05, 6, 0,
				minSize, maxSize);
		
		if (!faces.empty()) {
			mainFace = faces.toArray()[0];
			System.out.println(mainFace);
			return true;
		}
		else {
			return false;
		}
	}
	
	/**
	 * Returns a representation of how far the face is from the camera.
	 * @return float distance
	 */
	public float getDistance() {
		return (float)mainFace.height;
	}
	
	/**
	 * Returns a representation of the horizontal angle between the face and the
	 * line perpendicular to the camera.
	 * @return float horizontalAngle
	 */
	public float getHorizontalAngle() {
		return mainFace.x+(mainFace.width/2)-(imgWidth/2);
	}
	
	/**
	 * Returns a representation of the vertical angle between the face and the
	 * line perpendicular to the camera.
	 * @return float verticalAngle
	 */
	public float getVerticalAngle() {
		return mainFace.y+(mainFace.height/2)-(imgHeight/2);
	}

}
