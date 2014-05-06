package game.network;

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;

/**
 * Class contains all types of message that can be sent or recieved between a
 * client and the server
 **/
public abstract class Message {

	public static class LocalError extends Message implements Error, Receivable {
		private final String message;

		public LocalError(String message) {
			this.message = message;
		}

		@Override
		public void log() {
			System.out.println("Encountered local error because " + message);
		}

	}

	/**
	 * @author Samuel Reveives objects from the server and an instance of Kryo
	 *         to deserialize the object
	 */
	public static Receivable receive(InputStream in, Kryo s)
			throws IOException, Receivable.ReceiveException {
		byte type = (byte) in.read();
		switch (type) {
		case 0:
			return ServerClient.receive(in);
		case 1:
			throw new Receivable.ReceiveException(new byte[] { type },
					"Received unreceivable message type: ClientServer");
		case 2:
			return ClientClient.receive(in, s);
		default:
			throw new Receivable.ReceiveException(new byte[] { type },
					"Unknown Message type");
		}
	}

	/**
	 * Handles a received object from the server before deserialisation
	 * 
	 * @author Samuel
	 * 
	 */
	public static interface Receivable {
		public static class ReceiveException extends Exception implements Error {
			private static final long serialVersionUID = 4571778938645843455L;
			final byte[] object;
			final String message;

			public ReceiveException(byte[] object, String message) {
				this.object = object;
				this.message = message;
			}

			@Override
			public void log() {
				System.out.println("Encountered serialization error on object "
						+ object.toString() + " because " + message);
			}
		}
	}

	/**
	 * Handles a sendable object before transmission to the server. Sets the
	 * Kryo serialisation ID
	 * 
	 * @author Samuel
	 * 
	 */
	public static interface Sendable {
		public void send(OutputStream out, Kryo serializer) throws IOException,
				SendException;

		public static class SendException extends Exception implements Error {

			final Sendable object;
			final String message;

			public SendException(Sendable object, String message) {
				this.object = object;
				this.message = message;
			}

			private static final long serialVersionUID = 4553964614941096768L;

			@Override
			public void log() {
				System.out.println("Encountered serialization error on object "
						+ object.toString() + " because " + message);
			}
		}
	}

	/**
	 * Log an error if received for debug purposes.
	 * 
	 * @author Samuel
	 * 
	 */
	public static interface Error {
		void log();
	}

	/**
	 * All cases of message types that can be received or sent. This is the top
	 * level of the client.
	 * 
	 * @author Samuel
	 * 
	 */
	public static abstract class ServerClient extends Message implements
			Receivable {
		public static ServerClient receive(InputStream in) throws IOException,
				Receivable.ReceiveException {
			byte type = (byte) in.read();
			switch (type) {
			case 0:
				return new AckBeginMM();
			case 1:
				return new AckCancelMM();
			case 2:
				return new FoundGame();
			case 3:
				return new OpponentQuit();
			case 4:
				return new AckRequestQuit();
			case 5:
				return StateError.receive(in);
			case 6:
				return new NetworkError();
			default:
				throw new Receivable.ReceiveException(new byte[] { type },
						"Unknown Message.ServerClient type");
			}
		}

		/**
		 * Match making began on the server for this user - trying to find
		 * match.
		 * 
		 * @author Samuel
		 * 
		 */
		public static class AckBeginMM extends ServerClient {
		}

		/**
		 * Match making cancelled for this user on the server.
		 * 
		 * @author Samuel
		 * 
		 */
		public static class AckCancelMM extends ServerClient {

		}

		/**
		 * Found a match for a game for this user on the server.
		 * 
		 * @author Samuel
		 * 
		 */
		public static class FoundGame extends ServerClient {

		}

		/**
		 * Opponent quit the game - is processed so that this user knows they
		 * won.
		 * 
		 * @author Samuel
		 * 
		 */
		public static class OpponentQuit extends ServerClient {

		}

		/**
		 * A request to quit the game was sent from the other user.
		 * 
		 * @author Samuel
		 * 
		 */
		public static class AckRequestQuit extends ServerClient {

		}

		/**
		 * Some transaction of objects was unexpected and the games have
		 * diverged - state error.
		 * 
		 * @author Samuel
		 * 
		 */
		public static class StateError extends ServerClient implements Error {
			public StateError(Protocol.State state) {
				this.state = state;
			}

			public static StateError receive(InputStream in)
					throws IOException, Receivable.ReceiveException {
				byte state = (byte) in.read();
				switch (state) {
				case 0:
					return new StateError(Protocol.State.Game);
				case 1:
					return new StateError(Protocol.State.MM);
				case 2:
					return new StateError(Protocol.State.Idle);
				default:
					throw new Receivable.ReceiveException(new byte[] { state },
							"Unknown state");
				}
			}

			private final Protocol.State state;

			@Override
			public void log() {
				System.out.println("Received State Error: " + state.toString());
			}
		}

		public static class NetworkError extends ServerClient implements Error {
			@Override
			public void log() {
				System.out.println("Received Network Error");
			}

		}
	}

	/**
	 * Abstract class to represent the client.
	 * 
	 * @author Samuel
	 * 
	 */
	public static abstract class ClientServer extends Message implements
			Sendable {
		@Override
		public void send(OutputStream out, Kryo s) throws IOException,
				Sendable.SendException {
			out.write(1);
		}

		/**
		 * Send match making request
		 * 
		 * @author Samuel
		 * 
		 */
		public static class BeginMM extends ClientServer {
			@Override
			public void send(OutputStream out, Kryo s) throws IOException,
					Sendable.SendException {
				super.send(out, s);
				out.write(0);
			}
		}

		/**
		 * Cancel match making request
		 * 
		 * @author Samuel
		 * 
		 */
		public static class CancelMM extends ClientServer {
			@Override
			public void send(OutputStream out, Kryo s) throws IOException,
					Sendable.SendException {
				super.send(out, s);
				out.write(1);
			}
		}

		/**
		 * Match making request did quit unexpectedly.
		 * 
		 * @author Samuel
		 * 
		 */
		public static class RequestQuit extends ClientServer {
			@Override
			public void send(OutputStream out, Kryo s) throws IOException,
					Sendable.SendException {
				super.send(out, s);
				out.write(2);
			}
		}
	}

	/**
	 * Read data in, object sent from one client to hte other.
	 * 
	 * @author Samuel
	 * 
	 */
	public static abstract class ClientClient extends Message implements
			Receivable, Sendable {
		public static ClientClient receive(InputStream in, Kryo s)
				throws IOException, Receivable.ReceiveException {
			DataInputStream dataIn = new DataInputStream(in);
			{
				int len = dataIn.readInt();
				byte[] objData = new byte[len];
				for (int i = 0; i < len;)
					i += dataIn.read(objData, i, len - i);
				Input input = new Input(objData);
				{
					return new CCObject(s.readClassAndObject(input));
				}
			}
		}

		@Override
		public void send(OutputStream out, Kryo s) throws IOException,
				Sendable.SendException {
			out.write(2);
		}

		/**
		 * Object to send, send to output stream.
		 * 
		 * @author Samuel
		 * 
		 */
		public static class CCObject extends ClientClient {
			private final Object object;

			public CCObject(Object inputObject) {
				this.object = inputObject;
			}

			public Object getObject() {
				return this.object;
			}

			@Override
			public void send(OutputStream out, Kryo s) throws IOException,
					SendException {
				super.send(out, s);
				DataOutputStream dataOut = new DataOutputStream(out);
				{
					try (ByteArrayOutputStream bytes = new ByteArrayOutputStream()) {
						try (Output output = new Output(bytes)) {
							s.writeClassAndObject(output, object);
							output.flush();
						}
						byte[] b = bytes.toByteArray();
						dataOut.writeInt(b.length);
						dataOut.write(b);
					}
				}
			}
		}

	}

}
