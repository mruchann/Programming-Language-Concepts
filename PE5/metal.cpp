#include "kpop.h"
#include "metal.h"
#include "rock.h"
#include "jazz.h"

int MetalBand::play(MusicBand *other)
{
    MusicBand *isKPopBand = dynamic_cast<KPopBand *>(other);
    MusicBand *isMetalBand = dynamic_cast<MetalBand *>(other);
    MusicBand *isRockBand = dynamic_cast<RockBand *>(other);
    MusicBand *isJazzBand = dynamic_cast<JazzBand *>(other);

    double C = -1;

    if (isKPopBand) {
        C = 0.5;
    }
    else if (isMetalBand) {
        C = 1.0;
    }
    else if (isRockBand) {
        C = 1.5;
    }
    else if (isJazzBand) {
        C = 1.1;
    }

    int Score = (get_fan_count() + 0.1 * get_talent() * get_energy()) * C;

    double K = 0.16;
    set_energy(get_energy() - get_energy() * K);

    return Score;
}

void MetalBand::rehearse(void) 
{
    double K = 0.16;
    set_energy(get_energy() - get_energy() * 0.5 * K);

    int T = -5;
    set_talent(get_talent() + T);
}