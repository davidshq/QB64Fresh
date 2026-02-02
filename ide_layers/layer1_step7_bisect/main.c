/*
 * Layer 1 step 7 BISECT: Step6 + Batch B only (drawing primitives).
 * No Batch A (view_reset, window_reset, palette_reset, etc.) and no Batch C (mouse/keyboard).
 *
 * Run this first. If you see title + text + shapes (line, box, circle, pixel),
 * then the culprit for "blank" in full step7 is in Batch A or C. If you still
 * see no shapes, the issue may be in Batch B or in display order/coords.
 */
#include "qb64fresh_rt.h"
#include <stdlib.h>
#include <stdio.h>

int main(void) {
    if (qb_gfx_screen(-1, -1, 0, 0) != 0) {
        fprintf(stderr, "qb_gfx_screen failed\n");
        return 1;
    }
    qb_screenhide();
    qb_screenshow();
    (void)qb_icon();
    qb_screenmove(0, 0);

    (void)qb_gfx_cls();
    (void)qb_gfx_display();
    (void)qb_gfx_autodisplay(1);

    {
        QbString* title = qb_string_new("Layer1 step 7 bisect (B only)");
        qb_sub__title(title);
        qb_string_release(title);
    }
    (void)qb_gfx_color(7, 0);
    (void)qb_gfx_locate(1, 1);
    (void)qb_gfx_print("Layer1 step 7 bisect - OK");
    (void)qb_gfx_display();

    /* Batch B only: drawing primitives (no A, no C) */
    (void)qb_gfx_pset(50, 50, 7);
    (void)qb_gfx_line(10, 10, 100, 10, 7);
    (void)qb_gfx_box(10, 20, 150, 80, 7, 0);
    (void)qb_gfx_circle(200, 100, 30, 7, 0);

    (void)qb_gfx_display();

    while (qb_gfx_poll_events() == 1) {
        qb_sleep(0.01);
    }
    return 0;
}
