package game.struct;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.util.concurrent.*;

public class SyncReceiver implements Runnable {

	private ConcurrentLinkedQueue<AbstractPlane> queue;
	private InputStream is;

	public SyncReceiver(ConcurrentLinkedQueue<AbstractPlane> queue, InputStream is)
			throws IOException {
		this.queue = queue;
		this.is = is;

		// TODO Auto-generated constructor stub
	}

	@Override
	public void run() {

		try {
			ObjectInputStream ois = new ObjectInputStream(is);
			for (;;) {
				AbstractPlane p = (AbstractPlane) ois.readObject();
				System.out.println("After plane read");
				if (p != null)
					queue.offer(p);
			}
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			System.exit(5);

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			System.exit(5);
		}

		/*
		 * byte[] buffer = null; byte[] expectedSizeBuffer = new byte[4]; int
		 * expectedSize = 0; for(;;) { try { for(int i = 0; i < 4; ) { i +=
		 * is.read(buffer, i, 4 - i); } expectedSize =
		 * Array.getInt(expectedSizeBuffer, 0); buffer = new byte[expectedSize];
		 * for(int i = 0; i < expectedSize; ){ i += is.read(buffer, i,
		 * expectedSize - i); } } catch (IOException e) {
		 * System.out.println("Fatal error occured"); System.exit(5); }
		 * 
		 * Plane p = new Plane(); ByteBuffer b =
		 * ByteBuffer.allocate(expectedSize).put(buffer, 0, expectedSize);
		 * b.rewind(); p.deserialize(b); System.out.println(p);
		 * System.out.println(b); System.out.println(buffer);
		 * System.out.println(p.getUniqueNetworkObjectID()); queue.offer(p); }
		 */
	}

}
