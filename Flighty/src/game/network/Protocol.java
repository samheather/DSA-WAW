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

public class Protocol implements Closeable {
	
	private static int INITIAL_CLASS_ID = 102; //randomly picked
	
	public static enum State {
		Game,
		MM,
		Idle
	}

	public Protocol(String hostname, int i, List<Class> toRegister) {
		try {
			try {
				socket = new Socket(hostname, i);
			} catch (Exception e) {
				e.printStackTrace();
				received.offer(new Message.LocalError(
						"Could not connect to game server:\n" + e.getMessage()));
				return;
			}
			try {
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
		
		int classId = INITIAL_CLASS_ID;
		for (Class c : toRegister) {
			sendKryo.register(c, classId);
			receiveKryo.register(c, classId);
			classId ++;
		}
		

		sender = new Thread(this.new Sender());
		receiver = new Thread(this.new Receiver());

		sender.start();
		receiver.start();
	}
	
	private Kryo sendKryo = new Kryo();
	private Kryo receiveKryo = new Kryo();

	private ConcurrentLinkedQueue<Message.Receivable> received = new ConcurrentLinkedQueue<Message.Receivable> ();
	private LinkedTransferQueue<Message.Sendable> toSend = new LinkedTransferQueue<Message.Sendable> ();
	private Socket socket;
	private OutputStream os;
	private InputStream is;
	private Thread sender;
	private Thread receiver;
	private boolean closed = false;

	public Message.Receivable getMessage() {
		return received.poll();
	}

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

	private class Receiver implements Runnable {

		@Override
		public void run() {
			Message.Receivable msg = null;
			try {
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

	private class Sender implements Runnable {

		@Override
		public void run() {
			Message.Sendable msg = null;
			try {
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
