#############################################
#
# Logan Sims
# CSCI 402
# Summer 2015
# hw2 - JHusbands
#
#
#
#
##############################################
import itertools
import Queue

class Node(object):
	def __init__(self, rb1, rb2, parent, boat):
		self.rb1 = rb1
		self.rb2 = rb2
		self.boat = boat
		self.children = []
		self.parent = parent

	def addchild(self, child):
		self.children.append(child)

def hasNode(foundStates, node):

	for state in foundStates:
		if ((set(state.rb1) == set(node.rb1)) and (set(state.rb2) == set(node.rb2)) and (state.boat == node.boat)):
			return True
	return False


#main function
def JHusbands(couples, boatsize):
	rb1 = setup(couples)
	rb2 = []
	foundStates = []
	boat = 1

	root = Node(rb1, rb2, 0, boat)

	BFStree(root, boatsize)

	displayTree(root, 0)

def displayTree(node):

	for child in node.children:
		displayTree(child)

	if node.rb1 == []:
		displayPath(node)

def displayPath(node):
	if node.parent != 0:
		displayPath(node.parent)
	print node.rb1, node.rb2, "Boat at riverbank", node.boat



def BFStree(root, boatsize):
	foundStates = []
	Q = Queue.Queue()
	Q.put(root)
	foundStates.append(root)
	while not Q.empty():
		currentNode = Q.get()
		if currentNode.rb1 == []:
			return
		process(currentNode, foundStates, boatsize)
		for child in currentNode.children:
			Q.put(child)

def process(currentNode, foundStates, boatsize):
	if currentNode.boat == 1:
		src = currentNode.rb1
		dest = currentNode.rb2
	if currentNode.boat == 2:
		src = currentNode.rb2
		dest = currentNode.rb1

	actions = generateActions(src, dest, boatsize)

	for action in actions:

		newsrc = [person for person in src if person not in action]
		newdest = action + dest

		if currentNode.boat == 1:
			c = Node(newsrc, newdest, currentNode, 2)
		if currentNode.boat == 2:
			c = Node(newdest, newsrc, currentNode, 1)
		if not hasNode(foundStates, c):
			foundStates.append(c)
			currentNode.addchild(c)


def valid(location):
	noHusbands = True
	for person in location:
		if person.startswith("H"):
			noHusbands = False
	if noHusbands:
		return True
	for person in location:
		if person.startswith("W"):			
			if "H" + person[1] not in location:
				return False
	return True


def generateActions(src, dest, boatsize):
	Actions = []

	for i in range(1, boatsize+1):
		for boat in itertools.combinations(src, i):
			if valid(list(boat)):
				Actions.append(list(boat))

	for action in Actions:
		if not valid(dest + action):
			Actions.remove(action)

	return Actions



def setup(couples):
	rb1 = []
	for i in range(0, couples):
		rb1 = rb1 + ["W"+str(i), "H"+str(i)]
	return rb1

#start
couples = input('Enter the number of couples: ')
boatsize = input('Enter the number of people that can fit in the boat: ')
JHusbands(couples, boatsize)
