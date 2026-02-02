/*
 * Layer 1 step 2: Layer1 + qb_screenhide() before qb_screenshow().
 * IDE has $SCREENHIDE at startup then later _SCREENSHOW. This tests that sequence.
 * If the window does not appear, the break is hide-before-show.
 */
#include "qb64fresh_rt.h"
#include <stdlib.h>
#include <stdio.h>

int main(void) {
    if (qb_gfx_screen(-1, -1, 1, 0) != 0) {
        fprintf(stderr, "qb_gfx_screen failed\n");
        return 1;
    }
    qb_screenhide();   /* ADD: IDE does this first ($SCREENHIDE) */
    qb_screenshow();
    while (qb_gfx_poll_events() == 1) {
        qb_sleep(0.01);
    }
    return 0;
}
