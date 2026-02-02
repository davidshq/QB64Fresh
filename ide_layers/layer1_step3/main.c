/*
 * Layer 1 step 3: Layer1_step2 + qb_icon() after qb_screenshow().
 * IDE does _CONSOLE OFF, _SCREENSHOW, _ICON. This step adds _ICON (qb_icon).
 * If the window does not appear, the break is in the icon call.
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
    (void)qb_icon();   /* ADD: IDE does _ICON after _SCREENSHOW */
    while (qb_gfx_poll_events() == 1) {
        qb_sleep(0.01);
    }
    return 0;
}
