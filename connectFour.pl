/*
 * Programmer: Yuval Lavie
 * File Name: connectFour.pl
 * Description: Connect four written in prolog, 
 * 				based on alpha-beta algorithem. 
 * Input: Column number, as the program asks.
 * Output: The game board.
 * Synopsys: swipl connectFour.pl, and then run startGame(Difficulty).
 * 			 Recomended difficulty levels are 4/5/6
*/


% Const
size(7).

startGame(Difficulty):-
	printStartingMessage(),
	integer(Difficulty), Difficulty > 0,  size(S),
	createBoard(S, Board), innerStartGame(Board, Difficulty). 

innerStartGame(Board, Difficulty):-
	mainLoop(Board, Difficulty),
	shouldPlayAgain(), innerStartGame(Board, Difficulty),!.

% For returning true even if the user doesn't want to play.
innerStartGame(_,_).

% Program always starts, and plays as the max player.
% After choosing the next move, it prints the new board and then
% checks if won. If so, stops the game. Else, gets user's input
% and prints the board. Checks again if user won. If not, does it
% all over again until there's a wining or a draw.
mainLoop(Board, Difficulty):-
	alphabeta(max, Board, -1000, 1000, Difficulty, NewBoard, _), 
	printBoard(NewBoard), 
	(((printMessageIfWin(NewBoard, o),!) ; (   isBoardFull(NewBoard),!))
	;
	createBoardFromUser(NewBoard, NewBoard1),
	printBoard(NewBoard1), (printMessageIfWin(NewBoard1, x),!
	;
	(isBoardFull(NewBoard1),!
	; mainLoop(NewBoard1, Difficulty)))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%% Board helpers %%%%%%%%%%%%%%%%%%%%%%%%%%
createBoard(Size, Board):-
	createBoard(Size, Size, Board).

% Stop condition
createBoard(_, 0, []):-!.

% Creates one line at a time
createBoard(Size, LineNumber, Lines):-
	createLine(LineNumber, Size, Line),
	LineNumber1 is LineNumber-1,
	createBoard(Size, LineNumber1, Rest),
    append(Line, Rest, Lines).

% Stop condition
createLine(_, 0, []):-!.
% Creates one node at a time
createLine(LineNumber, AmountOfNodes, [Node|Nodes]):-
	%Column-Row-Type
	Node = AmountOfNodes-LineNumber-empty,
	AmountOfNodes1 is AmountOfNodes-1,
	createLine(LineNumber, AmountOfNodes1, Nodes).

isBoardFull(Board):-
	not(member(_-_-empty, Board)),write("It's a tie!"),nl.

printBoard(Board):-
	size(Size),
	printLineNumbers(Size), nl,
	printBoard(Board, Size).

% Stop condition
printBoard(_, 0):-!.
% Prints one line at a time
printBoard(Board, LineNumber):-
	size(Size),
	printLine(LineNumber, Size, Board), nl,
	LineNumber1 is LineNumber-1,
	printBoard(Board, LineNumber1).

% Stop condition
printLine(_, 0, _):-!.
% Prints one node at a time
printLine(LineNumber, NodeNumber, Board):-
	member(NodeNumber-LineNumber-Object, Board),
	% For empty nodes it prints a blank
    ((Object==empty, write(" "),!);
	write(Object)), write("|"),
	NodeNumber1 is NodeNumber-1,
	printLine(LineNumber, NodeNumber1, Board).

% Stop condition
printLineNumbers(0):-!.
% Prints columns' numbers
printLineNumbers(LineNumber):-
	write(LineNumber), write(" "),
	LineNumber1 is LineNumber-1,
	printLineNumbers(LineNumber1).

%%%%%%%%%%%%%%%%%%%%%%%% Alphabeta algorithm %%%%%%%%%%%%%%%%%%%%%%%%

nextTurn(max, min).
nextTurn(min, max).


/* 
 * alphabeta(Turn, Pos, Alpha, Beta, Depth, GoodPos, Val)
 * Like the algorithm mentioned in the book, but with a few changes:
 * a. Depth support was added.
 * b. greather(less)-equal instead of just greater(less) then helps 
 *    improve the performance by not checking the latter functions.
 * c. The way of detection min/max turn.
*/

% If it's a wining/losing position, there's no need to check the
% moves after, and then return a value (rank).
alphabeta(_, Pos, _, _, _, _, XRank):-
	isWin(Pos, Player), !,
	((Player==o, XRank=1000) ; 
	(Player==x, XRank= -1000)).

% For 0 depth, we'll get the value from our heuristic function
alphabeta(_, Pos, _, _, 0, _, Val) :-
	!, heuristic(Pos, Val).

alphabeta(Turn, Pos, Alpha, Beta, Depth, GoodPos, Val) :-
	D1 is Depth -1, 
	moves(Pos, Turn, PosList),
    nextTurn(Turn, NTurn),
	boundedbest(NTurn, PosList, Alpha, Beta, D1, GoodPos, Val).

boundedbest(Turn, [Pos|PosList], Alpha, Beta, Depth, GoodPos, GoodVal) :- 
	alphabeta(Turn, Pos, Alpha, Beta, Depth, _, Val),
	goodenough(Turn, PosList, Alpha, Beta, Pos, Val, Depth, GoodPos, GoodVal).

goodenough(_, [], _, _, Pos, Val, _, Pos, Val) :- !.
goodenough(Turn, _, Alpha, Beta, Pos, Val, _, Pos, Val) :-
	Turn==min, Val >= Beta, !
	;
	Turn==max, Val =< Alpha, !.

goodenough(Turn, PosList, Alpha, Beta, Pos, Val, Depth, GoodPos, GoodVal) :-
	newbounds(Turn, Alpha, Beta, Pos, Val, NewAlpha, NewBeta),
	boundedbest(Turn, PosList, NewAlpha, NewBeta, Depth, Pos1, Val1),
	betterof(Turn, Pos, Val, Pos1, Val1, GoodPos, GoodVal).

newbounds(Turn, Alpha, Beta, _, Val, Val, Beta) :-
	Turn==min, Val >= Alpha,!.

newbounds(Turn, Alpha, Beta, _, Val, Alpha, Val) :-
	Turn==max, Val =< Beta, !.

newbounds(_, Alpha, Beta, _, _, Alpha, Beta).

betterof(Turn, Pos, Val, _, Val1, Pos, Val):-
	Turn==min, Val>=Val1,!
	;
	Turn==max, Val=<Val1,!.

betterof(_, _, _, Pos1, Val1, Pos1, Val1).
	
% Gets all the possible boards after one move
% and returns it in the Moves list
moves(Board, Turn, Moves):-
	((Turn==min, Turn1=x,!) ; Turn1=o) ,
	setof(M, X^move(Board, Turn1, M, X), Moves).

% Get one possible boarrd if X is var, and checks if it's 
% a legal move if no vars were given.
move(Board, Turn, NewBoard, X):-
	size(S),   
	member(X-Y-empty, Board), Y=<S, 
	(Y==1 ; % It's the first row
	% There's something beneath Y
	(Y1 is Y-1, member(X-Y1-Taken, Board), Taken\=empty)),
    delete(Board, X-Y-empty, NewBoard1), append(NewBoard1, [X-Y-Turn], NewBoard).

%%%%%%%%%%%%%%%%%%%%%%%%% heuristic helpers %%%%%%%%%%%%%%%%%%%%%%%%%


% The max player's sign is o, so the value is calculated by
% substruction of x player's rank from o's one.
heuristic(Board, Value):-
	getRank(Board, o, ORank), 
	getRank(Board, x, XRank),
	Value is ORank-XRank.

% Rank of a player is the sum of his rank in horizontal quartet, 
% vertical one, and each direction of the diagonal quartet.
% For each quartet type it sums up it's value using aggregate_all method.
getRank(Board, Player, Rank):-
	aggregate_all(sum(Rank1), isMatch(Board, getQuartetRightDiagonal, Player, Rank1), HR),
	aggregate_all(sum(Rank2), isMatch(Board, getQuartetLeftDiagonal, Player, Rank2), VR),
	aggregate_all(sum(Rank3), isMatch(Board, getQuartetHorizontal, Player, Rank3), RDR),
	aggregate_all(sum(Rank4), isMatch(Board, getQuartetVertical, Player, Rank4), LDR),
	Rank is HR+VR+LDR+RDR.

% Creates quartet by the type that was given (function), and then
% return the appropiate rank by calling helpers functions.
isMatch(Board, Function, Player, Rank):-
	call(Function, Board, Q), countEmptyAndPlayer(Q, Player, EmptyCount, PlayerCount),
	getQuartetRank(EmptyCount, PlayerCount, Rank).

% Checks all of the quartet types if there's a win 
isWin(Board, Player):-
	getQuartetRightDiagonal(Board, Quartet),
	sameFour(Quartet, Player),!.
isWin(Board, Player):-
	getQuartetLeftDiagonal(Board, Quartet),
	sameFour(Quartet, Player),!.
isWin(Board, Player):-
	getQuartetHorizontal(Board, Quartet),
	sameFour(Quartet, Player),!.
isWin(Board, Player):-
	getQuartetVertical(Board, Quartet),
	sameFour(Quartet, Player).

sameFour([P, P, P, P], P):-
	P \= empty.

% One empty and three player earnes 2 points
getQuartetRank(1, 3, 2):-!.
% Two empty and two player earnes 1 points
getQuartetRank(2, 2, 1):-!.

% Counts the amount of empty nodes and player's nodes.
% If the opposite player's node is encountered, it fails.
countEmptyAndPlayer([], _, 0, 0):-!.
countEmptyAndPlayer([empty|Rest], Player, EmptyCount, PlayerCount):-
	countEmptyAndPlayer(Rest, Player, EmptyCount1, PlayerCount),
	EmptyCount is EmptyCount1+1,!. 

countEmptyAndPlayer([Player|Rest], Player, EmptyCount, PlayerCount):-
	countEmptyAndPlayer(Rest, Player, EmptyCount, PlayerCount1),
    PlayerCount is PlayerCount1+1,!.

% Get all of the quartet types
getQuartetRightDiagonal(Board, [Player1, Player2, Player3, Player4]):-
	member(X1-Y1-Player1, Board), Y2 is Y1+1, X2 is X1+1,
	member(X2-Y2-Player2, Board), Y3 is Y2+1, X3 is X2+1,
	member(X3-Y3-Player3, Board), Y4 is Y3+1, X4 is X3+1,
	member(X4-Y4-Player4, Board).

getQuartetLeftDiagonal(Board, [Player1, Player2, Player3, Player4]):-
	member(X1-Y1-Player1, Board), Y2 is Y1+1, X2 is X1-1,
	member(X2-Y2-Player2, Board), Y3 is Y2+1, X3 is X2-1,
	member(X3-Y3-Player3, Board), Y4 is Y3+1, X4 is X3-1,
	member(X4-Y4-Player4, Board).

getQuartetHorizontal(Board, [Player1, Player2, Player3, Player4]):-
	member(X1-Y-Player1, Board), X2 is X1+1,
	member(X2-Y-Player2, Board), X3 is X2+1, 
	member(X3-Y-Player3, Board), X4 is X3+1, 
	member(X4-Y-Player4, Board).

getQuartetVertical(Board, [Player1, Player2, Player3, Player4]):-
	member(X-Y1-Player1, Board), Y2 is Y1+1,
	member(X-Y2-Player2, Board), Y3 is Y2+1, 
	member(X-Y3-Player3, Board), Y4 is Y3+1, 
	member(X-Y4-Player4, Board).

%%%%%%%%%%%%%%%%%%%%%%%%%% User interactive %%%%%%%%%%%%%%%%%%%%%%%%%
printStartingMessage():-
	write("Hello and welcome to connect four!"), nl,
	write("During the game, you will be asked to enter a requested column number."), nl,
	write("You can allways type 'exit' for quitting the game."), nl,
	write("Thank you and goodluck!"), nl.

% Prints appropriate message for a win or a lose 
printMessageIfWin(Board, o):-
	isWin(Board, o), write("Sorry, you lost"),nl,!.
printMessageIfWin(Board, x):-
	isWin(Board, x), write("Great! You won!"),nl.

% Gets requested column from user, validates it and 
% when it's valid returns the new board
createBoardFromUser(Board, NewBoard):-
	size(S),
	repeat, (format("Choose column 1-~d:~n", S),
	read(X), ((X==exit,!, fail) ; ( 
	move(Board, x, NewBoard, X),! % Validation
	;write("Illeagal move :("), nl, fail))).

shouldPlayAgain():-
	write("Play another game? y/[N]"),nl, read(Ans), Ans==y.