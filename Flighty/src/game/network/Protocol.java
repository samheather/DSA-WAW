package game.network;

import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.LinkedTransferQueue;

import com.esotericsoftware.kryo.Kryo;

/**
 * This class represents a connection to a server implementing the Sepr
 * Server-Client communication protocol
 */
public class Protocol implements Closeable {

	private static int INITIAL_CLASS_ID = 102; // randomly picked number to
												// begin class ids from when
												// serializing

	/**
	 * The states a connection can be in
	 */
	public static enum State {
		Game, MM, Idle
	}

	/**
	 * Attempt to connect to a server using the Sepr Server-Client communication
	 * protocol
	 * 
	 * @param hostname
	 *            The hostname of the server
	 * @param i
	 *            The port to connect on
	 * @param toRegister
	 *            A list of classes that will be registered for serialization.
	 *            Any object that will be sent using this connection must be
	 *            registered.
	 */
	public Protocol(String hostname, int i,
			@SuppressWarnings("rawtypes") List<Class> toRegister) {
		try {
			// attempt to create a TCP connection to the specified hostname and
			// port
			try {
				socket = new Socket(hostname, i);
			} catch (Exception e) {
				e.printStackTrace();
				received.offer(new Message.LocalError(
						"Could not connect to game server:\n" + e.getMessage()));
				return;
			}
			try {
				// attempt to create an outputstream
				os = socket.getOutputStream();
			} catch (Exception e) {
				socket.close();
				e.printStackTrace();
				received.offer(new Message.LocalError(
						"Internal error when opening output stream to server:\n"
								+ e.getMessage()));
				return;
			}
			try {
				// attempt to create an inputstream
				is = socket.getInputStream();
			} catch (Exception e) {
				os.close();
				socket.close();
				e.printStackTrace();
				received.offer(new Message.LocalError(
						"Internal error when opening input stream from server:\n"
								+ e.getMessage()));
				return;
			}
		} catch (IOException e) {
			e.printStackTrace();
			received.offer(new Message.LocalError(
					"Internal error when releasing resources:\n"
							+ e.getMessage()));
			return;
		}
		// register classes for serialization
		int classId = INITIAL_CLASS_ID;
		for (@SuppressWarnings("rawtypes") Class c : toRegister) {
			sendKryo.register(c, classId);
			receiveKryo.register(c, classId);
			classId++;
		}

		// create and start sending and receiving threads
		sender = new Thread(this.new Sender());
		receiver = new Thread(this.new Receiver());

		sender.start();
		receiver.start();
	}

	// serializers: one for the sender thread, one for the receiver
	private Kryo sendKryo = new Kryo();
	private Kryo receiveKryo = new Kryo();

	// Buffers to store messages that need to be sent or have been received
	private ConcurrentLinkedQueue<Message.Receivable> received = new ConcurrentLinkedQueue<Message.Receivable>();
	private LinkedTransferQueue<Message.Sendable> toSend = new LinkedTransferQueue<Message.Sendable>();
	private Socket socket;
	private OutputStream os;
	private InputStream is;
	private Thread sender;
	private Thread receiver;
	// whether or not the connection has been deinitialized
	private boolean closed = false;

	/**
	 * @return A message from the server, if one is available, otherwise null
	 */
	public Message.Receivable getMessage() {
		return received.poll();
	}

	/**
	 * 
	 * @param m
	 *            A message to be sent to the server
	 */
	public void putMessage(Message.Sendable m) {
		toSend.offer(m);
	}

	@Override
	protected void finalize() {
		if (closed)
			return;
		sender.interrupt();
		receiver.interrupt();
		closed = true;
	}

	@Override
	public void close() {
		finalize();
	}

	/**
	 * This inner class represents a thread which will receive data from the tcp
	 * connection to the server and convert it to message objects
	 */
	private class Receiver implements Runnable {

		@Override
		public void run() {
			Message.Receivable msg = null;
			try {
				// deserialize network data into a message, and push it onto the
				// queue of received messages
				// repeat forever
				for (;;) {
					msg = Message.receive(is, receiveKryo);
					if (msg == null)
						throw new Exception(
								"Message receipt error: Message.read() returned null.");
					else
						received.offer(msg);
				}
			} catch (InterruptedException e) {
				received.offer(new Message.LocalError(
						"Receiver thread interrupted:\n" + e.getMessage()));
			} catch (Exception e) {
				received.offer(new Message.LocalError(
						"Internal error in Receiver thread:\n" + e.getMessage()));
				e.printStackTrace();
			} finally {
				// cleanup the connection if it hasnt already been cleaned up by
				// the sender thread
				boolean interrupted = sender.isInterrupted();
				if (!interrupted && sender.isAlive()) {
					sender.interrupt();
					return;
				} else {
					try {
						is.close();
						os.close();
						socket.close();
					} catch (Exception e) {
						received.offer(new Message.LocalError(
								"Internal error when releasing resources:\n"
										+ e.getMessage()));
					}
				}
			}
		}
	}

	/**
	 * This inner class represents a thread which will receive message objects
	 * from the queue, serialise them and send them to the server over the tcp
	 * connection
	 */
	private class Sender implements Runnable {

		@Override
		public void run() {
			Message.Sendable msg = null;
			try {
				// get a message from the queue of messages, serialize it, and
				// send it over the network
				// repeat forever
				for (;;) {
					msg = toSend.take();
					if (msg == null)
						throw new Exception(
								"Queueing Error in toSend: take() returned null.");
					else
						msg.send(os, sendKryo);
				}
			} catch (InterruptedException e) {
				received.offer(new Message.LocalError(
						"Sender thread interrupted:\n" + e.getMessage()));
			} catch (Exception e) {
				received.offer(new Message.LocalError(
						"Internal error in Sender thread:\n" + e.getMessage()));
				e.printStackTrace();
			} finally {
				// cleanup the connection if it hasnt already been cleaned up by
				// the receiver thread
				boolean interrupted = receiver.isInterrupted();
				if (!interrupted && receiver.isAlive()) {
					receiver.interrupt();
				} else {
					return;
				}
				try {
					is.close();
					os.close();
					socket.close();
					closed = true;
				} catch (Exception e) {
					received.offer(new Message.LocalError(
							"Internal error when releasing resources:\n"
									+ e.getMessage()));
				}
			}
		}
	}
}
