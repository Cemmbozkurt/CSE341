% knowledge base

schedule(istanbul, ankara, 1). 
schedule(istanbul, rize, 4).
schedule(istanbul, izmir, 2). 
schedule(antalya, izmir, 2). 
schedule(antalya, diyarbakir, 4).
schedule(antalya, erzincan, 3). 
schedule(ankara, diyarbakir, 8). 
schedule(van, gaziantep, 3).
schedule(ankara, izmir, 6). 
schedule(ankara, rize, 5). 
schedule(ankara, van, 4).
schedule(diyarbakir, malatya, 4).
schedule(canakkale, mugla, 6).
schedule(canakkale, erzincan,6). 

%rules

reverseSchedule(X, Y, C) :- schedule(X, Y, C).
reverseSchedule(X, Y, C) :- schedule(Y, X, C).

calculateConnection(Visited, X, Y, C):- reverseSchedule(X, Y, C), 
                            \+(member(Y,Visited)).
calculateConnection(Visited, X, Y, C) :-  reverseSchedule(X, A, Cost1), 
					        \+(member(A,Visited)), 
					        calculateConnection([A|Visited], A, Y, Cost2),
                            C is Cost1 + Cost2.

connection(X, Y, C) :- calculateConnection([X], X, Y, C).