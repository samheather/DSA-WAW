"""The most basic chat protocol possible.

run me with twistd -y chatserver.py, and then connect with multiple
telnet clients to port 1025
"""

import sys
from twisted.protocols import basic
from twisted.python import log
from twisted.internet import reactor
log.startLogging(sys.stdout)
log.msg("SLOG after start logging!")

class MyChat(basic.LineReceiver):
	__opponent = None

	def connectionMade(self):
		print "SLOG"
		self.factory.clients.append(self)
		print "SLOG 1"
		self.factory.notInGame.append(self)
		print "SLOG 2"
		while (len(self.factory.notInGame) >= 2):
			x = self.factory.notInGame.pop(0)
			y = self.factory.notInGame.pop(0)
			x.__opponent = y
			y.__opponent = x
		print "SLOG FINISH YAY"

	def connectionLost(self, reason):
		print "SLOG Lost a client!"
		self.factory.clients.remove(self)
		if (self.__opponent == None):
			self.factory.notInGame.remove(self)
		else:
			self.__opponent.__opponent = None
			self.factory.notInGame.append(self.__opponent)

	def lineReceived(self, data):
		print "SLOG Sender data received"
		if self.__opponent == None:
			self.transport.write("E0") # not in game
			print "SLOG E0"
			return
		self.__opponent.transport.write(data)


from twisted.internet import protocol
from twisted.application import service, internet

factory = protocol.ServerFactory()
factory.protocol = MyChat
factory.clients = []
factory.notInGame = []

application = service.Application("chatserver")
#internet.TCPServer(1025, factory).setServiceParent(application)
reactor.listenTCP(1025, factory)
reactor.run()