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

	public static class ServerClient extends Message implements Receivable {
		public static class AckBeginMM{
		}
	}

	public static class ClientServer extends Message implements Sendable {
		public static class BeginMM {
			
		}
		
		public static class CancelMM {
			
		}
		
		public static class RequestQuit{
			
		}
	}

	public static class ClientClient extends Message implements Receivable,
			Sendable {
		public static class CCObject{
			public void CCObject(Object inputObject) {
				
			}
		}
		
	}

}
