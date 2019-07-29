"""
Created June 2019
Back end system to update sailing rankings based on the results of a regatta
Written for python 2
@author - Dan Birmingham
"""

from elopy import *
import csv

################################################################################
## Problems/thoughts  ##########################################################
################################################################################

# Probably there is a better way to handle the devision (Class) of boat 6-23

# At some point should handle list import better

# Not sure why I'm having trouble with super implementation here 6-23
# def __init__(self, Class="College"):
#     super(sailingELO, self).__init__()
#     self.Class = Class #Class of boats  for ranking or College sailors
#### Twas because our friends class did not extend object
#### so im going to go ahead and make it extend object so that things work



#NOTE: added random devide by 10 for betterlooking numbers

#TODO: 
    # make sorted raking list method that prints pretty 
    # check the math so that we get better updating system/ contact chess.com

################################################################################
## Classes #####################################################################
################################################################################

class sailingELO(Implementation):
    """class for handling the ranking sailors according to the ELO system in the
    Implementation class found in elopy.py"""
    def __init__(self, Class="College"):
        super(sailingELO, self).__init__()
        self.Class = Class #Class of boats  for ranking or College sailors

    #Adopted from elopy for the sake of fidelity
    def __getPlayerList(self):
        """
        Returns this implementation's player list.
        @return - the list of all player objects in the implementation.
        """
        return self.players

    def updateRankings(self, results):
        """Function that will appropriately update the ELO rankings of all
        sailors who compete against each other in a race. Takes in as argument
        a list of strings representing the sailors in the order they finished 
        the race"""
        numOfSailors = len(results)

        #print results

        #from parent Implementation function recordMatch function
        k = len(self.__getPlayerList()) * 42

        #create list of adjustments to ELO rankings so they are all updated
        #at the same time. Each entry corresponds to the sailor who finished
        #in the respective place
        rankingAdjustments = [0] * numOfSailors
        for i in range(0,numOfSailors-1): #DOUBLE CHECK INDICIES
            for j in range(i+1,numOfSailors):
                print results[i]
                player1 = self.getPlayer(results[i])
                player2 = self.getPlayer(results[j])

                #expected result based on ELO philosophy
                expected1 = player1.compareRating(player2)
                expected2 = player2.compareRating(player1)

                rankingAdjustments[i] += k * (1 - expected1) #"winner"
                rankingAdjustments[j] += k * (0 - expected2) #"loser"

        #update the rankings in the database
        for i in range(0,numOfSailors):
            try:
                self.getPlayer(results[i]).rating += rankingAdjustments[i]/10
            except Exception as e:
                raise e
                print "Sailor ___ was not found in the database"


################################################################################
## testing workspace  ##########################################################
################################################################################
sailorEloDB = sailingELO() #create our ELO handler object

with open('rankings.csv', 'rb') as csvfile: # open test data to get names of schools
    dictReader = csv.DictReader(csvfile)


    #Add the players to our ELO DB
    for row in dictReader:
        sailorEloDB.addPlayer((row['school_coded']), rating = 1000)

#get list of list of regatta results not worrying about ties
seasonResults = []
with open('neisa_only.csv', 'rb') as csvfile: 
    dictReader = csv.DictReader(csvfile)

    regattaResults = []
    i = 1

    for row in dictReader:
        if 'NA' == row['regatta_id']:
            pass
        elif i == int(row['regatta_id']):
            regattaResults.append((row['school_coded']))
        else:
            while i < int(row['regatta_id']):
                i+=1
            seasonResults.append(regattaResults)
            regattaResults = []
            regattaResults.append((row['school_coded']))      

#Update the rankings after regatta
for lst in seasonResults:
    sailorEloDB.updateRankings(lst)
print sailorEloDB.getRatingList()


################################################################################
## program main function #######################################################
################################################################################

# if __name__ == '__main__':
#     main()