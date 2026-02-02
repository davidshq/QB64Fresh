/*
 * Layer 1 step 6: Layer1_step5 + batch: set window title, set color, print one line.
 * So the window is no longer blank — we should see a title and text.
 *
 * We use active_page=0, visual_page=0 so drawing (cls, color, print) goes to the
 * displayed page. The IDE uses (1, 0) so it draws to page 1 while showing page 0;
 * that's why the full IDE window can look blank until it flips pages.
 *
 * Batch (in order):
 *   A. qb_sub__title("Layer1 step 6")  - window title
 *   B. qb_gfx_color(7, 0)               - foreground 7, background 0
 *   C. qb_gfx_locate(1, 1); qb_gfx_print("Layer1 step 6 - OK"); qb_gfx_display()
 */
#include "qb64fresh_rt.h"
#include <stdlib.h>
#include <stdio.h>

int main(void) {
    /* Use page 0 for both so drawn content is visible (IDE uses 1,0 which shows blank page 0) */
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

    /* Batch: title, color, print one line so window isn't blank */
    {
        QbString* title = qb_string_new("Layer1 step 6");
        qb_sub__title(title);
        qb_string_release(title);
    }
    (void)qb_gfx_color(7, 0);   /* foreground 7, background 0 */
    (void)qb_gfx_locate(1, 1);
    (void)qb_gfx_print("Layer1 step 6 - OK");
    (void)qb_gfx_display();

    while (qb_gfx_poll_events() == 1) {
        qb_sleep(0.01);
    }
    return 0;
}
