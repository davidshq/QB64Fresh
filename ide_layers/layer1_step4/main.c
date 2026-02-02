/*
 * Layer 1 step 4: Layer1_step3 + qb_screenmove(0, 0) after qb_icon().
 * IDE does _SCREENMOVE IDELeftPosition, IDETopPosition when IDEAutoPosition.
 * This step adds qb_screenmove(0, 0) (no-op position).
 * If the window does not appear, the break is in screenmove.
 */
#include "qb64fresh_rt.h"
#include <stdlib.h>
#include <stdio.h>

int main(void) {
    if (qb_gfx_screen(-1, -1, 1, 0) != 0) {
        fprintf(stderr, "qb_gfx_screen failed\n");
        return 1;
    }
    qb_screenhide();
    qb_screenshow();
    (void)qb_icon();
    qb_screenmove(0, 0);   /* ADD: IDE does _SCREENMOVE when IDEAutoPosition */
    while (qb_gfx_poll_events() == 1) {
        qb_sleep(0.01);
    }
    return 0;
}
