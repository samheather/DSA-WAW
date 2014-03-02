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

	public static enum Type {
		NOP((byte) 0), HELLO((byte) 1), ACKHELLO((byte) 2), GAME((byte) 3), ERROR(
				(byte) 4);

		private Type(byte value) {
			this.value = value;
		}

		private byte value = 0;

		public byte value() {
			return value;
		}
	}

	private final Type type;

	public Type messageType() {
		return type;
	}

	public void write(OutputStream out) throws IOException {
		out.write(messageType().value());
	}

	public static Message read(InputStream in) throws IOException {
		DataInputStream dis = new DataInputStream(in);
		byte type = dis.readByte();
		switch (type) {
		case 0:
			return new Nop();
		case 1:
			return new Hello();
		case 2:
			return new AckHello();
		case 3:
			return Game.read(in);
		case 4:
			return Error.read(in);
		default:
			return null;
		}
	}

	private Message(Type type) {
		this.type = type;
	}

	public static class Nop extends Message {
		public Nop() {
			super(Type.NOP);
		}
	}

	public static class Hello extends Message {
		public Hello() {
			super(Type.HELLO);
		}
	}

	public static class AckHello extends Message {
		public AckHello() {
			super(Type.ACKHELLO);
		}
	}

	public abstract static class Error extends Message {
		public static enum Type {
			FATAL((byte) 0), WARNING((byte) 1);

			private Type(byte value) {
				this.value = value;
			}

			private byte value = 0;

			public byte value() {
				return value;
			}
		}

		private final Type type;
		private final String reason;

		public Type errorMessageType() {
			return type;
		}

		public String reason() {
			return reason;
		}

		public void write(OutputStream out) throws IOException {
			super.write(out);
			out.write(errorMessageType().value());
			DataOutputStream dos = new DataOutputStream(out);
			dos.writeInt(reason.length());
			dos.writeChars(reason);
		}

		public static Error read(InputStream in) throws IOException {
			DataInputStream dis = new DataInputStream(in);
			byte type = dis.readByte();
			int length = dis.readInt();
			String reason = "";
			for (int i = 0; i < length; ++i)
				reason = reason + dis.readChar();
			switch (type) {
			case 0:
				return new Fatal(reason);
			case 1:
				return new Warning(reason);
			default:
				return null;
			}
		}

		private Error(Type type, String reason) {
			super(Message.Type.ERROR);
			this.type = type;
			this.reason = reason;
		}

		public static class Fatal extends Error {
			public Fatal(String message) {
				super(Type.FATAL, message);
			}
		}

		public static class Warning extends Error {
			public Warning(String message) {
				super(Type.WARNING, message);
			}
		}
	}

	public static abstract class Game extends Message {

		public static enum Type {
			BEGIN((byte) 0), END((byte) 1), QUIT((byte) 2), OBJECT((byte) 3), MATCHMAKING(
					(byte) 4), ERROR((byte) 5);

			private Type(byte value) {
				this.value = value;
			}

			private byte value = 0;

			public byte value() {
				return value;
			}
		}

		private final Type type;

		public Type gameMessageType() {
			return type;
		}

		public void write(OutputStream out) throws IOException {
			super.write(out);
			out.write(gameMessageType().value());
		}

		public static Game read(InputStream in) throws IOException {
			DataInputStream dis = new DataInputStream(in);
			byte type = dis.readByte();
			switch (type) {
			case 0:
				return new Begin();
			case 1:
				return new End();
			case 2:
				return new Quit();
			case 3:
				return Object.read(in);
			case 4:
				return new Matchmaking();
			case 5:
				return Error.read(in);
			default:
				return null;
			}
		}

		private Game(Type type) {
			super(Message.Type.GAME);
			this.type = type;
		}

		public static class Begin extends Game {
			public Begin() {
				super(Type.BEGIN);
			}
		}

		public static class End extends Game {
			public End() {
				super(Type.END);
			}
		}

		public static class Quit extends Game {
			public Quit() {
				super(Type.QUIT);
			}
		}

		public static class Matchmaking extends Game {
			public Matchmaking() {
				super(Type.MATCHMAKING);
			}
		}

		public abstract static class Error extends Game {
			public static enum Type {
				FATAL((byte) 0), WARNING((byte) 1);

				private Type(byte value) {
					this.value = value;
				}

				private byte value = 0;

				public byte value() {
					return value;
				}
			}

			private final Type type;
			private final String reason;

			public Type errorGameMessageType() {
				return type;
			}

			public String reason() {
				return reason;
			}

			public void write(OutputStream out) throws IOException {
				super.write(out);
				out.write(errorGameMessageType().value());
				DataOutputStream dos = new DataOutputStream(out);
				dos.writeInt(reason.length());
				dos.writeChars(reason);
			}

			public static Error read(InputStream in) throws IOException {
				DataInputStream dis = new DataInputStream(in);
				byte type = dis.readByte();
				int length = dis.readInt();
				String reason = "";
				for (int i = 0; i < length; ++i)
					reason = reason + dis.readChar();
				switch (type) {
				case 0:
					return new Fatal(reason);
				case 1:
					return new Warning(reason);
				default:
					return null;
				}
			}

			private Error(Type type, String reason) {
				super(Game.Type.ERROR);
				this.type = type;
				this.reason = reason;
			}

			public static class Fatal extends Error {
				public Fatal(String message) {
					super(Type.FATAL, message);
				}
			}

			public static class Warning extends Error {
				public Warning(String message) {
					super(Type.WARNING, message);
				}
			}
		}

		public static class Object extends Game {
			private java.lang.Object object;

			public java.lang.Object object() {
				return object;
			}

			public Object(java.lang.Object object) {
				super(Type.OBJECT);
				this.object = object;
			}

			public static Object read(InputStream in) throws IOException {
				byte[] buffer = new byte[0];
				try (DataInputStream dataIn = new DataInputStream(in)) {
					int length = dataIn.readInt();
					buffer = new byte[length];
					for (int i = 0; i < length;)
						i += dataIn.read(buffer, i, length - i);
				}
				try (Input input = new Input(buffer)) {
					Kryo kryo = new Kryo();
					return new Object(kryo.readClassAndObject(input));
				}
			}

			@Override
			public void write(OutputStream out) throws IOException {
				super.write(out);
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
					dataOut.flush();
				}
			}
		}
	}
}
