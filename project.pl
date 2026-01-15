:- dynamic edge/3, num_nodes/1.

tsp_from_file(File, Start, Tour, Cost) :-
    load_graph(File),
    num_nodes(N),
    findall(I, (between(1, N, I), I \= Start), Unvisited),
    get_mst_cost([Start|Unvisited], H0),
    astar([f(H0, 0, [Start], Start)], N, Tour, Cost).

load_graph(File) :-
    retractall(edge(_,_,_)),
    retractall(num_nodes(_)),
    open(File, read, S),
    read_line_to_codes(S, L),
    atom_codes(A, L),
    atomic_list_concat([N_At, _], ' ', A),
    atom_number(N_At, N),
    assertz(num_nodes(N)),
    read_edges(S),
    close(S).

read_edges(S) :-
    read_line_to_codes(S, L),
    ( L == end_of_file -> true
    ; atom_codes(A, L),
      atomic_list_concat([I_At, J_At, W_At], ' ', A),
      maplist(atom_number, [I_At, J_At, W_At], [I, J, W]),
      assertz(edge(I,J,W)), assertz(edge(J,I,W)),
      read_edges(S)
    ).

get_mst_cost([], 0).
get_mst_cost([_], 0).
get_mst_cost([H|T], Cost) :-
    prim_loop(T, [H], 0, Cost).

prim_loop([], _, Acc, Acc).
prim_loop(Unvisited, Tree, Acc, Total) :-
    findall(W-V, (member(U, Tree), member(V, Unvisited), get_w(U, V, W)), Edges),
    keysort(Edges, [MinW-NextV|_]),
    delete(Unvisited, NextV, Remaining),
    NewAcc is Acc + MinW,
    prim_loop(Remaining, [NextV|Tree], NewAcc, Total).

get_w(U, V, W) :- edge(U, V, W), !.
get_w(_, _, 1000000).

astar([f(_, G, Path, Curr)|_], N, FinalPath, FinalCost) :-
    length(Path, N),
    last(Path, Start),
    get_w(Curr, Start, Wret),
    FinalCost is G + Wret,
    reverse(Path, FinalPath).

astar([f(_, G, Path, Curr)|Rest], N, FinalTour, FinalCost) :-
    last(Path, StartNode),
    findall(f(F, NewG, [Next|Path], Next),
            ( between(1, N, Next),
              \+ member(Next, Path),
              get_w(Curr, Next, W),
              NewG is G + W,
              findall(U, (between(1, N, U), \+ member(U, [Next|Path])), Unvisited),
              get_mst_cost([StartNode|Unvisited], H),
              F is NewG + H ),
            Successors),
    append(Successors, Rest, NewQueue),
    sort(NewQueue, SortedQueue),
    astar(SortedQueue, N, FinalTour, FinalCost).