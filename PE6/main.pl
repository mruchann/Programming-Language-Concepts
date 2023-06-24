:- module(main, [is_vote_wasted/2, is_candidate_elected/2, candidate_count_from_city/3, all_parties/1, all_candidates_from_party/2, all_elected_from_party/2, election_rate/2, council_percentage/2, alternative_debate_setups/2]).
:- [kb].

is_vote_wasted(City, PoliticalParty) :- 
    not(elected(City, PoliticalParty, _)).

% ----------------------------------------------------------------------------------------------------

is_candidate_elected(Name, PoliticalParty) :- 
    candidate(Name, PoliticalParty, City, Row), 
    elected(City, PoliticalParty, ElectedRepresentativeCount), 
    Row =< ElectedRepresentativeCount.

% ----------------------------------------------------------------------------------------------------

candidate_count_from_city([], _, 0).    

candidate_count_from_city([Name|Rest], City, Count) :- 
    not(candidate(Name, _, City, _)),
    candidate_count_from_city(Rest, City, Count).

candidate_count_from_city([Name|Rest], City, Count) :- 
    candidate(Name, _, City, _),
    candidate_count_from_city(Rest, City, RestCount), Count is RestCount + 1.

% ----------------------------------------------------------------------------------------------------      

all_parties(Parties) :- 
    findall(PoliticalParty, party(PoliticalParty, _), Parties).

% ----------------------------------------------------------------------------------------------------

all_candidates_from_party(PoliticalParty, ListOfCandidates) :- 
    findall(Name, candidate(Name, PoliticalParty, _, _), ListOfCandidates).

% ----------------------------------------------------------------------------------------------------

all_elected_from_party(PoliticalParty, ListOfCandidates) :-
    findall(Name, is_candidate_elected(Name, PoliticalParty), ListOfCandidates).

% ----------------------------------------------------------------------------------------------------

election_rate(PoliticalParty, Percentage) :-
    all_elected_from_party(PoliticalParty, ListOfElectedCandidates),
    all_candidates_from_party(PoliticalParty, ListOfAllCandidates),
    length(ListOfElectedCandidates, LengthOfElectedCandidates),
    length(ListOfAllCandidates, LengthOfAllCandidates),
    Percentage is LengthOfElectedCandidates / LengthOfAllCandidates,
    format('~2f', Percentage).

% ----------------------------------------------------------------------------------------------------

council_percentage(PoliticalParty, Percentage) :-
    to_elect(RepresentativeCount),
    all_elected_from_party(PoliticalParty, ListOfElectedCandidates),
    length(ListOfElectedCandidates, LengthOfElectedCandidates),
    Percentage is LengthOfElectedCandidates / RepresentativeCount,
    format('~2f', Percentage).

% ----------------------------------------------------------------------------------------------------

alternative_debate_setups(DescriptionString, OrderedListOfCandidates) :- 
    atom_chars(DescriptionString, DescriptionList),
    alternative_debate_setupsStringToList(DescriptionList, OrderedListOfCandidates).

alternative_debate_setupsStringToList([], []).

alternative_debate_setupsStringToList([Initial|DescriptionListRest], [Name|OrderedListOfCandidatesRest]) :- 
    party(PoliticalParty, Initial),
    all_candidates_from_party(PoliticalParty, ListOfCandidatesFromParty),
    member(Name, ListOfCandidatesFromParty),
    alternative_debate_setupsStringToList(DescriptionListRest, OrderedListOfCandidatesRest),
    not(member(Name, OrderedListOfCandidatesRest)).
