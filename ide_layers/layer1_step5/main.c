/*
 * Layer 1 step 5: Layer1_step4 + a BATCH of several calls the IDE may do early.
 * If the window does not appear, remove calls one by one (see README) to find the culprit.
 *
 * Batch (in order):
 *   A. qb_gfx_cls()       - clear screen
 *   B. qb_gfx_display()   - flush display
 *   C. qb_gfx_autodisplay(1) - enable auto display
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
    qb_screenmove(0, 0);

    /* Batch: several calls the IDE may do early */
    (void)qb_gfx_cls();           /* A */
    (void)qb_gfx_display();       /* B */
    (void)qb_gfx_autodisplay(1); /* C */

    while (qb_gfx_poll_events() == 1) {
        qb_sleep(0.01);
    }
    return 0;
}
