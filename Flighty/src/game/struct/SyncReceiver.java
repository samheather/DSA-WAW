package game.struct;


import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.util.concurrent.*;

public class SyncReceiver implements Runnable {

	
	private ConcurrentLinkedQueue<Plane> queue;
	private InputStream is;
	public SyncReceiver(ConcurrentLinkedQueue<Plane> queue, InputStream is) {
		this.queue = queue;
		this.is = is;
		// TODO Auto-generated constructor stub
	}

	@Override
	public void run() {
		byte[] buffer = new byte[102];
		for(;;) {
			try {
				for(int i = 0; i < 102; )
					i += is.read(buffer, i, 102 - i);
			} catch (IOException e) {
				System.out.println("Fatal error occured");
				System.exit(5);
			}
			
			Plane p = new Plane();
			ByteBuffer b = ByteBuffer.allocate(100).put(buffer, 0, 100);
			b.rewind();
			p.deserialize(b);
			System.out.println(p);
			System.out.println(b);
			System.out.println(buffer);
			System.out.println(p.getUniqueNetworkObjectID());
			queue.offer(p);
		}
	}

}
