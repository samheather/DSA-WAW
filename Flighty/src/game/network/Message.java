package game.network;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;

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

	public static Receivable receive(InputStream in) throws IOException,
			Receivable.ReceiveException {
		byte type = (byte) in.read();
		switch (type) {
		case 0:
			return ServerClient.receive(in);
		case 1:
			throw new Receivable.ReceiveException(new byte[] { type },
					"Received unreceivable message type: ClientServer");
		case 2:
			return ClientClient.receive(in);
		default:
			throw new Receivable.ReceiveException(new byte[] { type },
					"Unknown Message type");
		}
	}

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

	public static interface Sendable {
		public void send(OutputStream out) throws IOException, SendException;

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

	public static interface Error {
		void log();
	}

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

		public static class AckBeginMM extends ServerClient {
		}

		public static class AckCancelMM extends ServerClient {

		}

		public static class FoundGame extends ServerClient {

		}

		public static class OpponentQuit extends ServerClient {

		}

		public static class AckRequestQuit extends ServerClient {

		}

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

	public static abstract class ClientServer extends Message implements
			Sendable {
		@Override
		public void send(OutputStream out) throws IOException,
				Sendable.SendException {
			out.write(1);
		}

		public static class BeginMM extends ClientServer {
			@Override
			public void send(OutputStream out) throws IOException,
					Sendable.SendException {
				super.send(out);
				out.write(0);
			}
		}

		public static class CancelMM extends ClientServer {
			@Override
			public void send(OutputStream out) throws IOException,
					Sendable.SendException {
				super.send(out);
				out.write(1);
			}
		}

		public static class RequestQuit extends ClientServer {
			@Override
			public void send(OutputStream out) throws IOException,
					Sendable.SendException {
				super.send(out);
				out.write(2);
			}
		}
	}

	public static abstract class ClientClient extends Message implements
			Receivable, Sendable {
		public static ClientClient receive(InputStream in) throws IOException,
				Receivable.ReceiveException {
			try (DataInputStream dataIn = new DataInputStream(in)) {
				int len = dataIn.readInt();
				try (Input input = new Input(dataIn)) {
					Kryo kryo = new Kryo();
					return new CCObject(kryo.readClassAndObject(input));
				}
			}
		}

		@Override
		public void send(OutputStream out) throws IOException,
				Sendable.SendException {
			out.write(2);
		}

		public static class CCObject extends ClientClient {
			private final Object object;

			public CCObject(Object inputObject) {
				this.object = inputObject;
			}

			@Override
			public void send(OutputStream out) throws IOException,
					SendException {
				super.send(out);
				try (DataOutputStream dataOut = new DataOutputStream(out)) {
					byte[] objectBytes = new byte[0];
					try (Output output = new Output()) {
						Kryo kryo = new Kryo();
						kryo.writeObject(output, object);
						output.flush();
						objectBytes = output.getBuffer();
					}
					dataOut.writeInt(objectBytes.length);
					dataOut.write(objectBytes);
				}
			}
		}

	}

}
