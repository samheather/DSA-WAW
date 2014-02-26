"""The most basic chat protocol possible.

run me with twistd -y chatserver.py, and then connect with multiple
telnet clients to port 1025
"""

import sys
from twisted.protocols import basic
from twisted.python import log
log.startLogging(sys.stdout)
log.msg("SLOG after start logging!")


class MyChat(basic.LineReceiver):

	__notInGame = []
	__games = []

	def connectionMade(self):
		print "SLOG"
		self.factory.clients.append(self)
		print "SLOG 1"
		self.__notInGame.append(self)
		print "SLOG 2"
		while (len(self.__notInGame) >= 2):
			self.__games.append(Game(self.__notInGame.pop(0),
				self.__notInGame.pop(0)))
		print "SLOG FINISH YAY"

	def connectionLost(self, reason):
		print "SLOG Lost a client!"
		for g in self.__games:
			if g.__clientA == self or g.__clientB == self:
				self.__games.remove(g)
		self.factory.clients.remove(self)


	def lineReceived(self, data):
		for c in self.__games:
			c.handleReceived(self, data)

	def message(self, message):
		self.transport.write(message + '\n')

class Game():
	def __init__(self, clientA, clientB):
		self.__clientA = clientA
		self.__clientB = clientB
	def handleReceived(self, client, data):
		if (client == self.__clientA):
			self.__clientB.message(data)
		elif (client == self.__clientB):
			self.__clientA.message(data)


from twisted.internet import protocol
from twisted.application import service, internet

factory = protocol.ServerFactory()
factory.protocol = MyChat
factory.clients = []

application = service.Application("chatserver")
internet.TCPServer(1025, factory).setServiceParent(application)