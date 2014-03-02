package game.network;

import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.LinkedTransferQueue;

public class Protocol implements Closeable {

	public Protocol(String hostname, short port) {
		try {
			try {
				socket = new Socket(hostname, port);
			} catch (Exception e) {
				e.printStackTrace();
				received.offer(new Message.Error.Fatal(
						"Could not connect to game server:\n" + e.getMessage()));
				return;
			}
			try {
				os = socket.getOutputStream();
			} catch (Exception e) {
				socket.close();
				e.printStackTrace();
				received.offer(new Message.Error.Fatal(
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
				received.offer(new Message.Error.Fatal(
						"Internal error when opening input stream from server:\n"
								+ e.getMessage()));
				return;
			}
		} catch (IOException e) {
			e.printStackTrace();
			received.offer(new Message.Error.Fatal(
					"Internal error when releasing resources:\n"
							+ e.getMessage()));
			return;
		}

		sender = new Thread(this.new Sender());
		receiver = new Thread(this.new Receiver());

		sender.start();
		receiver.start();
	}

	private ConcurrentLinkedQueue<Message> received;
	private LinkedTransferQueue<Message> toSend;
	private Socket socket;
	private OutputStream os;
	private InputStream is;
	private Thread sender;
	private Thread receiver;
	private boolean closed = false;

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
			Message msg = null;
			try {
				for (;;) {
					msg = Message.read(is);
					if (msg == null)
						throw new Exception(
								"Message receipt error: Message.read() returned null.");
					else
						received.offer(msg);
				}
			} catch (InterruptedException e) {
				received.offer(new Message.Error.Warning(
						"Receiver thread interrupted:\n" + e.getMessage()));
			} catch (Exception e) {
				toSend.offer(new Message.Error.Fatal("Local Error"));
				received.offer(new Message.Error.Fatal(
						"Internal error in Receiver thread:\n" + e.getMessage()));
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
						received.offer(new Message.Error.Fatal(
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
			Message msg = null;
			try {
				for (;;) {
					msg = toSend.take();
					if (msg == null)
						throw new Exception(
								"Queueing Error in toSend: take() returned null.");
					else
						msg.write(os);
				}
			} catch (InterruptedException e) {
				received.offer(new Message.Error.Warning(
						"Sender thread interrupted:\n" + e.getMessage()));
			} catch (Exception e) {
				received.offer(new Message.Error.Fatal(
						"Internal error in Sender thread:\n" + e.getMessage()));
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
					received.offer(new Message.Error.Fatal(
							"Internal error when releasing resources:\n"
									+ e.getMessage()));
				}
			}
		}
	}
}
