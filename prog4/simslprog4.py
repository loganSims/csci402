#############################################
#
# Logan Sims
# CSCI 402
# Summer 2015
# Financial Advisor (Expert System)
#
# Requests financial information from user
# to give an investment plan.
#
##############################################

# Global values for making investment plan.
savings = 0
depend = 0
earnings = 0
earningstype = 0
incomeStatus = 0
savingsStatus = 0

# Functions for calulating minimum savings and income.
def minsavings(X):
  return (500*X)

def minincome(X):
  return (15000 + (4000*X))


# function: getInfo
# input: N/A
# output: N/A
#
# Description: Request information about finances from
#              user. Fills global values.
#              Asks for confirmation at end.
# 
def getInfo():
  global savings
  global depend
  global earnings
  global earningstype
  done = False
  while(not(done)):
    print "Please answer the following questions with digits only."
    savings = input('How much have you saved?\n')
    depend = input('Okay, how many dependents do you have?\n')
    earnings = input('Got it, what are your earnings?\n')
    earningstype = input('Are they steady or intermittent? ' + 
                         '(1 for steady, 0 for unsteady)\n')
    print
    print "amount saved:    " + str(savings)
    print "dependants:      " + str(depend)
    print "earnings:        " + str(earnings)
    print "earnings type: " + str(earningstype)
    done = input("Is this information correct? (1 for yes, 0 for no)\n")

# function: processIncome
# input: N/A
# output: 1 if income adequate, 0 otherwise.
#
# Description: Using rules 6, 7 & 8 determines 
#              if income is adequate or not.
# 
def processIncome():

  global earnings
  global earningstype
  global depend

  if (earningstype == 0):
    return 0
  if (earnings > minincome(depend)):
    return 1
  else:
    return 0

# function: processSavings
# input: N/A
# output: 1 if savings adequate, 0 otherwise.
#
# Description: Using rules 4. & 5. determines
#              if savings account is adequate or not.
#
def processSavings():
  global savings
  global depend

  if (savings > minsavings(depend)):
    return 1
  else:
    return 0

# function: processSavings
# input: N/A
# output: 0 on completion
#
# Description: Displays a investment plan based on users
#              income and savings. 
#
# If incomeStatus = 1 it is adequate, 0 otherwise.
# If savingsStatus = 1 it is adequate, 0 otherwise.
#
def processFinances():
  global incomeStatus
  global savingsStatus

  if (not(savingsStatus)):
    print "You should invest in savings."

  elif(incomeStatus):
    print "You should invest in stocks."

  else:
    print "You should invest in combination."

  return 0

# Get financial information from user.
getInfo()

# Process income/savings 
incomeStatus = processIncome()
savingsStatus = processSavings()

# Make decision
processFinances()








