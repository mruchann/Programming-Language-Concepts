#include "tournament_round.h"

// TournamentRound functions goes here

TournamentRound::TournamentRound() {

}
TournamentRound::TournamentRound(std::list<MusicBand*>_bands) {
    bands = _bands;

}
TournamentRound::TournamentRound(std::vector<MusicBand*>_bands) {
    std::list<MusicBand*> l(_bands.begin(), _bands.end());
    bands = l;
}

std::size_t TournamentRound::size() {
    return bands.size();
}
    
//TournamentRound::TournamentRound(TournamentRound& other) { }
//TournamentRound::TournamentRound(TournamentRound&& other) { }

TournamentRound& TournamentRound::operator=(TournamentRound&& other) {
    if (this != &other) {
        bands = std::move(other.bands);
    }
    return *this;
}
TournamentRound& TournamentRound::get_next_round() {
    int n = size();

    TournamentRound* nextTournamentRound = new TournamentRound();

    std::list<MusicBand*>::iterator itL = bands.begin();
    std::list<MusicBand*>::iterator itR = bands.end();
    itR--;

    for (int i = 0; i < n/2; i++, itL++, itR--) {
        MusicBand* left = (*itL);
        MusicBand* right = (*itR);

        if (left->get_energy() < 0) {
            nextTournamentRound->bands.push_back(right);
            right->set_fan_count(right->get_fan_count() + left->get_fan_count());
            left->set_fan_count(0);
            continue;
        }

        else if (right->get_energy() < 0) {
            nextTournamentRound->bands.push_back(left);
            left->set_fan_count(left->get_fan_count() + right->get_fan_count());
            right->set_fan_count(0);
            continue;
        }

        int leftScore = left->play(right);
        int rightScore = right->play(left);

        int fan_change = abs(leftScore - rightScore);

        if (leftScore >= rightScore) {
            nextTournamentRound->bands.push_back(left);
            if (right->get_fan_count() < fan_change) {
                left->set_fan_count(left->get_fan_count() + right->get_fan_count());
                right->set_fan_count(0);
            }
            else {
                left->set_fan_count(left->get_fan_count() + fan_change);
                right->set_fan_count(right->get_fan_count() - fan_change);
            }
        }
        else if (rightScore > leftScore) {
            nextTournamentRound->bands.push_back(right);
            if (left->get_fan_count() < fan_change) {
                right->set_fan_count(right->get_fan_count() + left->get_fan_count());
                left->set_fan_count(0);
            }
            else {
                right->set_fan_count(right->get_fan_count() + fan_change);
                left->set_fan_count(left->get_fan_count() - fan_change);
            }
        }
    }
    if (n % 2 == 1) {
        nextTournamentRound->bands.push_back(*itL);
    }
    return *nextTournamentRound;
}

std::ostream& operator<< (std::ostream &os, TournamentRound &other) {
    string delim = "";
    for (std::list<MusicBand*>::iterator it = other.bands.begin(); it != other.bands.end(); it++) {
        os << delim << (*it)->get_name();
        delim = "\t";
    }
    return os;
}