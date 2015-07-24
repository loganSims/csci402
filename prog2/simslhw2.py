#############################################
#
# Logan Sims
# CSCI 402
# Summer 2015
# hw2 - JHusbands
#
# Solves Jelous Husbands problem.
# Reports solution or that no solution exists.
#
# Start program with: python simslhw2.py
#
# At start program prompts for number of couples 
# follewed by the capacity of the boat.
#
#
##############################################
import itertools
import Queue


#A node in the search space
class Node(object):
	def __init__(self, rb1, rb2, parent, boat):
		self.rb1 = rb1
		self.rb2 = rb2
		self.boat = boat
		self.children = []
		self.parent = parent

	def addchild(self, child):
		self.children.append(child)

# function: hasNode
# input: 1. foundStates(list) - List of all found states.
#        2. node(Node) - the node being checked.
# output: Boolean - True if node has already been found, false otherwise.
#
# Checks if node has already been found in state space search.
def hasNode(foundStates, node):

	for state in foundStates:
		if ((set(state.rb1) == set(node.rb1)) and (set(state.rb2) == set(node.rb2)) and (state.boat == node.boat)):
			return True
	return False


# function: JHusbands
# input: 1. couples(int) - number of couples in problem
#        2. boatsize(int) - capacity of boat.
# output: None
#
# Attempts to solve jealous husbands problem.
#
def JHusbands(couples, boatsize):
	rb1 = setup(couples)
	rb2 = []
	foundStates = []
	boat = 1

	root = Node(rb1, rb2, 0, boat)

	BFStree(root, boatsize)

	noSolution = displayTree(root)

	if noSolution:
		displaySearch(root, 0)
		print
		print "All states searched, no state has riverbank 1 == []. No solution found"

# function: displayTree
# input: 1. node(Node) - current working node.
# output: boolean - True if solution exists, false otherwise.
#
# Uses displayPath to display the solution path
# if there is one.
#
def displayTree(node):

	noSolution = True

	for child in node.children:
		noSolution = displayTree(child)
		if noSolution == False:
			return noSolution

	if node.rb1 == []:
		displayPath(node)
		return False

	return noSolution

# function: dislpayPath
# input: 1. node(Node) - current working node.
# output: None
#
# Giving a goal state displays the solution to 
# reach it.
#	
def displayPath(node):
	if node.parent != 0:
		displayPath(node.parent)
	print node.rb1, node.rb2, "Boat at riverbank", node.boat


# function: displaySearch
# input: 1. node(Node) - currentnode to be displayed
#	 2. level(int) - depth of the search
# output: None
#
# Displays the search if no solution was found.
#
def displaySearch(node, level):
	print "level: " + str(level)
	print node.rb1, node.rb2, "Boat at riverbank", node.boat
	for child in node.children:
		displaySearch(child, level+1)
	if (node.children == []):
		print "Dead End"
	print


# function BFStree
# input: 1. root(Node) - The start state of the problem.
#	 2. boatsize(int) - capacity of boat.
# output: None
#	-Builds a tree from root.
#
# A BFS through the problem state space.
#
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

# funcion: process
# input: 1. currentNode(Node) - The node being processed.
#        2. foundStates(list) - A list of all visited states.
#        3. boatsize(int) - capacity of boat.
# output: None
#
# Finds currentNode's children states if they
# are not in the foundStates list. Adds them to
# currentNode's children list.
#
def process(currentNode, foundStates, boatsize):
	if currentNode.boat == 1:
		src = currentNode.rb1
		dest = currentNode.rb2
	if currentNode.boat == 2:
		src = currentNode.rb2
		dest = currentNode.rb1

	actions = generateActions(src, dest, boatsize, currentNode.boat)

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


# funtion: valid
# input: 1. location(list) - A location in a state (riverbank or boat).
# output: 1. Boolean - true if state is valid, false otherwise.
#
# Checks that a wife is with their husband if other husbands are 
# around. If no husbands are present returns true without check.
#
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


# function: generateActions
# input: 1. src(list) - The riverback the boat is currently at.
#        2. dest(list) - The destination riverbank.
#        3. boatsize(int) - determinds how many people can fit in the boat.
#        4. boatLoc(int) - gives location of boat (1 = rb1, 2 = rb2) 
#
# output: Actions(list) - A list of all legal actions from current state. 
#
# Generates all legal moves from a given state. Takes into account
# that there is no reason to take two people back from riverbank 2.
#
def generateActions(src, dest, boatsize, boatLoc):
	Actions = []

	if boatLoc == 1:
		count = 2
	else:
		count = 1

	for i in range(count, boatsize+1):
		for boat in itertools.combinations(src, i):
			newsrc = [person for person in src if person not in list(boat)]
			if valid(list(boat)) and valid(dest + list(boat)) and valid(newsrc):
				Actions.append(list(boat))

	return Actions


# function: setup
# input: 1. couples(int) - The number of couples in the problem.
# output: 1. rb1(list) - The state of riverbank 1. 
#
# Sets up riverbank 1 for the problem.
def setup(couples):
	rb1 = []
	for i in range(0, couples):
		rb1 = rb1 + ["W"+str(i), "H"+str(i)]
	return rb1

#start of program
couples = input('Enter the number of couples: ')
boatsize = input('Enter the number of people that can fit in the boat: ')
JHusbands(couples, boatsize)
