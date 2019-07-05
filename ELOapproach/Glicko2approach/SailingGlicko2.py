"""
Created July 2019
Back end system to update sailing rankings based on the results of a regatta
now using the glicko2 rating system
Written for python 2
@author - Dan Birmingham
"""

from glicko2 import *
import csv

################################################################################
## Problems/thoughts  ##########################################################
################################################################################

#Double check all the rankings are updating at once as they should

#TODO in update rankings prompts "Sailor not found in database, would you like
# to add now with base rating"... in fact the whole thing should be a lot more
#robust at some point

#might be able to use glicko2 to do a better job of weighting... ie adjusting 
#some of our variables based on race participants length if need be

###### THERE should be a better way to adopt scale up scale down and rate

################################################################################
## Classes #####################################################################
################################################################################
class sailorRating(Rating):
    """rating class modified to included name of sailor and class for which they
    are rated"""
    def __init__(self, name, Class="College", mu=MU, phi=PHI, sigma=SIGMA):
        super(sailorRating, self).__init__(mu, phi, sigma)
        self.name = name
        self.Class = Class
        
    def __repr__(self):
        args = (self.name, self.Class, self.mu, self.phi, self.sigma)
        return '(name:%s, class:%s, rating=%.3f, RD=%.3f, sigma=%.3f)' % args

#Create class to hold all of the sailors of one type of sailboat
class sailingGlicko2(Glicko2):
    """class for handling the ranking sailors according to the Glicko2 system in the
    Glicko2 class found in glicko2.py.

    wrapper class within which all the ranked sailors of a certain class are
    contained. Structure coped from elopy.py by @author Hank Hang Kai Sheehan
    TODO INSERT github link LINK"""
    def __init__(self, Class="College", mu=MU, phi=PHI, sigma=SIGMA, tau=TAU, epsilon=EPSILON):
        super(sailingGlicko2, self).__init__(mu, phi, sigma, tau, epsilon)
        self.Class = Class #Class of boats  for ranking or College sailors
        self.rankedMembers = []
        self.baseRating = mu

    def _getPlayerList(self):
        """
        Returns this implementation's player list.
        @return - the list of all player objects in the implementation.
        """
        return self.rankedMembers

    def getPlayer(self, name):
        """
        Returns the player in the implementation with the given name.
        @param name - name of the player to return.
        @return - the player with the given name.
        """
        for player in self.rankedMembers:
            if player.name == name:
                return player
        return None

    def _getPlayerAndIndex(self, name):
        """
        Returns the player in the implementation with the given name and 
        the index of that player in rankedMebers(for use in sailingGlicko2updateRankings)
        @param name - name of the player to return.
        @return - (the player with the given name,index of that player in rankedMembers).
        """
        for i in range(0,len(self.rankedMembers)):
            if self.rankedMembers[i].name == name:
                return (self.rankedMembers[i], i)
        return None

    def contains(self, name):
        """
        Returns true if this object contains a player with the given name.
        Otherwise returns false.
        @param name - name to check for.
        """
        for player in self.rankedMembers:
            if player.name == name:
                return True
        return False

    def create_rating(self, name, Class, mu=None, phi=None, sigma=None):
        if mu is None:
            mu = self.mu
        if phi is None:
            phi = self.phi
        if sigma is None:
            sigma = self.sigma
        return sailorRating(name, Class, mu=mu, phi=phi, sigma=sigma)


    def rate(self, rating, series):
        """
        Function that takes in a ranked member of the system 'rating' and adjusts
        their ratings based on a 'series' of wins and losses. This function is adapted
        straight from @author-Heungsub Lee in glicko2.py. FINISH HERE
        """
        # Step 2. For each player, convert the rating and RD's onto the
        #         Glicko-2 scale.
        rating = self.scale_down(rating)
        # Step 3. Compute the quantity v. This is the estimated variance of the
        #         team's/player's rating based only on game outcomes.
        # Step 4. Compute the quantity difference, the estimated improvement in
        #         rating by comparing the pre-period rating to the performance
        #         rating based only on game outcomes.
        d_square_inv = 0
        variance_inv = 0
        difference = 0
        for actual_score, other_rating in series:
            other_rating = self.scale_down(other_rating)
            impact = self.reduce_impact(other_rating)
            expected_score = self.expect_score(rating, other_rating, impact)
            variance_inv += impact ** 2 * expected_score * (1 - expected_score)
            difference += impact * (actual_score - expected_score)
            d_square_inv += (
                expected_score * (1 - expected_score) *
                (Q ** 2) * (impact ** 2))
        difference /= variance_inv
        variance = 1. / variance_inv
        denom = rating.phi ** -2 + d_square_inv
        mu = rating.mu + Q / denom * (difference / variance_inv)
        phi = math.sqrt(1 / denom)
        # Step 5. Determine the new value, Sigma', ot the sigma. This
        #         computation requires iteration.
        sigma = self.determine_sigma(rating, difference, variance)
        # Step 6. Update the rating deviation to the new pre-rating period
        #         value, Phi*.
        phi_star = math.sqrt(phi ** 2 + sigma ** 2)
        # Step 7. Update the rating and RD to the new values, Mu' and Phi'.
        phi = 1 / math.sqrt(1 / phi_star ** 2 + 1 / variance)
        mu = rating.mu + phi ** 2 * (difference / variance)
        # Step 8. Convert ratings and RD's back to original scale.
        return self.scale_up(self.create_rating(rating.name, rating.Class, mu, phi, sigma))

    def scale_down(self, rating, ratio=173.7178):
        mu = (rating.mu - self.mu) / ratio
        phi = rating.phi / ratio
        return self.create_rating(rating.name, rating.Class, mu, phi, rating.sigma)

    def scale_up(self, rating, ratio=173.7178):
        mu = rating.mu * ratio + self.mu
        phi = rating.phi * ratio
        return self.create_rating(rating.name, rating.Class, mu, phi, rating.sigma)


    def addPlayer(self, name, rating=None, phi=None, sigma=None):
        """
        Adds a new player to the implementation.
        @param name - The name to identify a specific player.
        @param rating - The player's rating.
        """
        if rating == None:
            rating = self.baseRating

        #check this
        self.rankedMembers.append(self.create_rating(name, self.Class, mu=rating, phi=phi, sigma=sigma))

    def removePlayer(self, name):
        """
        Remove player from the implementation
        @param name - The name to identify a specific player.
        """
        self._getPlayerList().remove(self.getPlayer(name))

    def getRatingList(self):
        """
        @return - list of ratings
        """
        lst = []
        for player in self._getPlayerList():
            lst.append(player)
        return lst

    def printRatingList(self):
        """
        @return - list of ratings
        """
        lst = []
        for player in self._getPlayerList():
            print player
    def sortRatings(self):
        """Sort the sailors in the dB based on highest ranking"""
        pass

    def updateRankings(self, strResults):
        """Function that will appropriately update the rankings of all
        sailors who compete against each other in a race. Takes in as argument
        a list of strings representing the sailors in the order they finished 
        the race
        @param- strResults: list of strings representing order in which sailors
                finished in the race
        @param- dB: database containing sailors in the race to be updated
        @return- list of updated sailingGlicko2 objects (after the race)"""
        numOfSailors = len(strResults)

        #change results to type sailing sailingGlicko2 based on list of strings
        #that is results
        results = []
        indexLst = []
        for name in strResults:
            tup = self._getPlayerAndIndex(name)
            results.append(tup[0])
            indexLst.append(tup[1])

        #use glicko2 to calculate rating adjustments without updating
        #updating the sailors yet by comparing all the sailors in a race one by one
        rankingAdjustments = [0] * numOfSailors
        for i in range(0,numOfSailors): #DOUBLE CHECK INDICIES
            lst = []
            currentSailor = results[i]
            for j in range(0,numOfSailors):
                if i<j:
                    lst.append((WIN, results[j]))
                elif i>j:
                    lst.append((LOSS, results[j]))

            rankingAdjustments[i] = self.rate(currentSailor, lst)

        #update the rankings in the database
        for i in range(0,numOfSailors):
            print"old: " + str(self.rankedMembers[indexLst[i]]) + "    and new:" + str(rankingAdjustments[i])
            self.rankedMembers[indexLst[i]]= rankingAdjustments[i]


################################################################################
## testing workspace  ##########################################################
################################################################################
sailorGlicko2TestDB = sailingGlicko2(tau=1.2) #create our glicko2 dB

with open('rankings.csv', 'rb') as csvfile: # open test data to get names of schools
    dictReader = csv.DictReader(csvfile)


    #Add the players to our DB
    for row in dictReader:
        sailorGlicko2TestDB.addPlayer((row['school_coded']))

#sailorGlicko2TestDB.printRatingList()

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
    sailorGlicko2TestDB.updateRankings(lst)
sailorGlicko2TestDB.printRatingList()


################################################################################
## program main function #######################################################
################################################################################

# if __name__ == '__main__':
#     main()