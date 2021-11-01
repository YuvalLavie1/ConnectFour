del([X|Xs], X,Xs):-!.
del([Y|Ys], X,[Y|Zs]):-!,
	del(Ys,X,Zs).
del(_,[],[]).


% createBoard(8, B), alphabeta(max, B, -1000, 1000, 6, GoodPos, Val).

nextTurn(max, min).
nextTurn(min, max).


alphabeta(_, Pos, _, _, _, _, XRank):-
	isWin(Pos, Player), !,
	((Player==o, XRank=1000) ; 
	(Player==x, XRank= -1000)).

		
alphabeta(_, Pos, _, _, 0, _, Val) :-
	!, heuristic(Pos, Val).%, write(Pos), nl, write(Val).

alphabeta(Turn, Pos, Alpha, Beta, Depth, GoodPos, Val) :-
	%write(Depth),nl,
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
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

size(8).

moves(Board, Turn, Moves):-
	((Turn==min, Turn1=x,!) ; Turn1=o) ,
	setof(M, X^move(Board, Turn1, M, X), Moves).

move(Board, Turn, NewBoard, X):-
	size(S),   
	member(X-Y-empty, Board), Y=<S, 
	(Y==1 ;
	% There's something beneath Y '%
	(Y1 is Y-1, member(X-Y1-Taken, Board), Taken\=empty)),
    del(Board, X-Y-empty, NewBoard1), append(NewBoard1, [X-Y-Turn], NewBoard).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
heuristic(Board, Value):-
	getRank1(Board, o, ORank), 
	getRank1(Board, x, XRank),
	Value is ORank-XRank.
/*
getRank(Board, Player, Rank):-
	aggregate_all(count, getHorizontal3AndEmpty(Board, Player), HR),
	aggregate_all(count, getVertical3AndEmpty(Board, Player), VR),
	aggregate_all(count, getRightDiagonal3AndEmpty(Board, Player), RDR),
	aggregate_all(count, getLeftDiagonal3AndEmpty(Board, Player), LDR),
	Rank is HR+VR+LDR+RDR.

getHorizontal3AndEmpty(Board, Player):-
	member(X1-Y-Player, Board), X2 is X1+1,
	member(X2-Y-Player, Board), X3 is X2+1, 
	member(X3-Y-Player, Board), (   X4 is X3+1 ; X4 is X1-1), 
	member(X4-Y-empty, Board).

getVertical3AndEmpty(Board, Player):-
	member(X-Y1-Player, Board), Y2 is Y1+1,
	member(X-Y2-Player, Board), Y3 is Y2+1, 
	member(X-Y3-Player, Board), Y4 is Y3+1, 
	member(X-Y4-empty, Board).

getRightDiagonal3AndEmpty(Board, Player):-
	member(X1-Y1-Player, Board), Y2 is Y1+1, X2 is X1+1,
	member(X2-Y2-Player, Board), Y3 is Y2+1, X3 is X2+1,
	member(X3-Y3-Player, Board), 
	((Y4 is Y3+1, X4 is X3+1) ;  (Y4 is Y1-1, X4 is X1-1)),
	member(X4-Y4-empty, Board).

getLeftDiagonal3AndEmpty(Board, Player):-
	member(X1-Y1-Player, Board), Y2 is Y1+1, X2 is X1-1,
	member(X2-Y2-Player, Board), Y3 is Y2+1, X3 is X2-1,
	member(X3-Y3-Player, Board), 
	((Y4 is Y3+1, X4 is X3-1) ;  (Y4 is Y1-1, X4 is X1+1)),
	member(X4-Y4-empty, Board).
*/

getRank1(Board, Player, Rank):-
	aggregate_all(sum(Rank1), isMatch(Board, getQuartetRightDiagonal, Player, Rank1), HR),
	aggregate_all(sum(Rank2), isMatch(Board, getQuartetLeftDiagonal, Player, Rank2), VR),
	aggregate_all(sum(Rank3), isMatch(Board, getQuartetHorizontal, Player, Rank3), RDR),
	aggregate_all(sum(Rank4), isMatch(Board, getQuartetVertical, Player, Rank4), LDR),
	Rank is HR+VR+LDR+RDR.



isMatch(Board, Function, Player, Rank):-
	call(Function, Board, Q), countEmptyAndPlayer(Q, Player, EmptyCount, PlayerCount),
	getQuartetRank(EmptyCount, PlayerCount, Rank).

isWin(Board, Player):-
	getQuartetRightDiagonal(Board, Quartet),
	sameFour(Quartet, Player),
	Player \= empty,!.
isWin(Board, Player):-
	getQuartetLeftDiagonal(Board, Quartet),
	sameFour(Quartet, Player),
	Player \= empty,!.
isWin(Board, Player):-
	getQuartetHorizontal(Board, Quartet),
	sameFour(Quartet, Player),
	Player \= empty,!.
isWin(Board, Player):-
	getQuartetVertical(Board, Quartet),
	sameFour(Quartet, Player),
	Player \= empty,!.	

% One empty and three player earnes 2 points
getQuartetRank(1, 3, 2):-!.
% Two empty and two player earnes 1 points
getQuartetRank(2, 2, 1):-!.

sameFour([P, P, P, P], P).


countEmptyAndPlayer([], _, 0, 0):-!.
countEmptyAndPlayer([empty|Rest], Player, EmptyCount, PlayerCount):-
	countEmptyAndPlayer(Rest, Player, EmptyCount1, PlayerCount),
	EmptyCount is EmptyCount1+1,!. 

countEmptyAndPlayer([Player|Rest], Player, EmptyCount, PlayerCount):-
	countEmptyAndPlayer(Rest, Player, EmptyCount, PlayerCount1),
    PlayerCount is PlayerCount1+1,!.

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


%%%%%%%%%%%%%%%%%%%%%%%%
createBoard(Size, Board):-
	createBoard(Size, Size, Board).
createBoard(_, 0, []):-!.
createBoard(Size, LineNumber, Lines):-
	createLine(LineNumber, Size, Line),
	LineNumber1 is LineNumber-1,
	createBoard(Size, LineNumber1, Rest),
    append(Line, Rest, Lines).

createLine(_, 0, []):-!.
createLine(LineNumber, AmountOfNodes, [Node|Nodes]):-
	Node = AmountOfNodes-LineNumber-empty,
	AmountOfNodes1 is AmountOfNodes-1,
	createLine(LineNumber, AmountOfNodes1, Nodes).

printBoard(Board):-
	size(Size),
	printLineNumbers(Size), nl,
	printBoard(Board, Size).
printBoard(_, 0):-!.
printBoard(Board, LineNumber):-
	size(Size),
	printLine(LineNumber, Size, Board), nl,
	LineNumber1 is LineNumber-1,
	printBoard(Board, LineNumber1).

printLine(_, 0, _):-!.
printLine(LineNumber, NodeNumber, Board):-
	member(NodeNumber-LineNumber-Object, Board),
	((Object==empty, write(" "),!);
	write(Object)), write("|"),
	NodeNumber1 is NodeNumber-1,
	printLine(LineNumber, NodeNumber1, Board).

printLineNumbers(0):-!.
printLineNumbers(LineNumber):-
	write(LineNumber), write(" "),
	LineNumber1 is LineNumber-1,
	printLineNumbers(LineNumber1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printMessageIfWin(Board, o):-
	isWin(Board, o), write("Sorry, you lost"),!.
printMessageIfWin(Board, x):-
	isWin(Board, x), write("Great! You won!").

createBoardFromUser(Board, NewBoard):-
	repeat, (write("Choose column 1-8:"),nl,
	read(X), move(Board, x, NewBoard, X),!
	;write("Illeagal move :("), fail).

startGame(Difficulty):-
	size(S), createBoard(S, Board), mainLoop(Board, Difficulty).


mainLoop(Board, Difficulty):-
	alphabeta(max, Board, -1000, 1000, Difficulty, NewBoard, _), 
	printBoard(NewBoard), 
	(printMessageIfWin(NewBoard, o),!
	;
	createBoardFromUser(NewBoard, NewBoard1),
	printBoard(NewBoard1), (printMessageIfWin(NewBoard1, x),!
	;mainLoop(NewBoard1, Difficulty))).