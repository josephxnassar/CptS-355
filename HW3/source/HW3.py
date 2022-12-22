# CptS 355 - Fall 2020 - Assignment 3
# Please include your name and the names of the students with whom you discussed any of the problems in this homework

debugging = False
def debug(*s): 
     if debugging: 
          print(*s)

## problem 1-a) organizebyFlavor - 15%

def organizebyFlavor(feedingLog):
     myLog = {}
     for date,log in feedingLog.items():
          for flavor,cans in log.items():
               if not flavor in myLog:
                    myLog[flavor] = {}
               myLog[flavor][date] = myLog[flavor].get(date,cans)
     return myLog

from functools import reduce
## problem 1-b) popularFlavor - 15%

def popularFlavor(feedingLog):
     flavorCans = organizebyFlavor(feedingLog)
     helper = lambda flavorData : (flavorData[0], reduce(lambda x,y : x + y, flavorData[1].values() ))
     temp = dict(map(helper,flavorCans.items() ))
     return reduce(lambda x,y : x if x[1] > y[1] else y, temp.items() )

## problem 2) unzip - 15%

def unzip(L):
     l1 = list(map(lambda x : x[0],L))
     l2 = list(map(lambda x : x[1],L))
     l3 = list(map(lambda x : x[2],L))
     return (l1,l2,l3)

## problem 3 - findCycle - 15%

def getCycle(graph,cur,visited):
     if cur is None:
          return
     else:
          if cur in visited:
               return visited.append(cur) # you can't put this line in front because if statement depends on element not in visited
          else:
               visited.append(cur)
               getCycle(graph,graph.get(cur),visited) 

def findCycle(graph,start):
     visited = []
     getCycle(graph,start,visited) # helper function including list of visited elements
     while visited[0] != visited[len(visited)-1]: # gets rid of prior elements not in cycle
          visited.pop(0) 
     if len(visited) is 1: # returns None if there is no cycle
          return None
     return visited

## problem 4 - getNextNode - 10%

def getNextNode(graph,start):
     cur = start
     while cur is not None:
          yield cur
          cur = graph.get(cur)

## problem 5 - DecodeIter - 25% 

class copyIter(object):
     def __init__(self,IT):
          self.input1 = IT
          self.current = self._getNextInput()
     def _getNextInput(self):
          try:
               current = self.input1.__next__()
          except:
               current = None
          return current
     def __next__(self):
          if self.current is None:
               raise StopIteration
          result = self.current
          self.current = self._getNextInput()
          return result
     def __iter__(self):
          return self

def DecodeIter():
     