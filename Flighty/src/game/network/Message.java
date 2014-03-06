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

	public static interface Receivable {
	}

	public static interface Sendable {
	}

	public static interface Error {
		void log();
	}

	public static class ServerClient extends Message implements Receivable {
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

	public static class ClientServer extends Message implements Sendable {
		public static class BeginMM extends ClientServer {

		}

		public static class CancelMM extends ClientServer {

		}

		public static class RequestQuit extends ClientServer {

		}
	}

	public static class ClientClient extends Message implements Receivable,
			Sendable {
		public static class CCObject extends ClientClient {
			private final Object object;

			public CCObject(Object inputObject) {
				this.object = inputObject;
			}
		}

	}

}
