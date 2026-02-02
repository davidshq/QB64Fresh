/*
 * Layer 1 step 7 NO VIEW/WINDOW: Step7 with only qb_gfx_view_reset() and
 * qb_gfx_window_reset() commented out. Use to confirm those two calls
 * are the culprit for blank/no shapes.
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
        QbString* title = qb_string_new("Layer1 step 7 (no view/window)");
        qb_sub__title(title);
        qb_string_release(title);
    }
    (void)qb_gfx_color(7, 0);
    (void)qb_gfx_locate(1, 1);
    (void)qb_gfx_print("Layer1 step 7 - OK");
    (void)qb_gfx_display();

    /* ----- Batch A with view_reset/window_reset COMMENTED OUT ----- */
    (void)qb_gfx_width();
    (void)qb_gfx_height();
    (void)qb_gfx_get_foreground();
    (void)qb_gfx_get_background();
    (void)qb_gfx_csrlin();
    (void)qb_gfx_pos();
    /* (void)qb_gfx_view_reset(); */
    /* (void)qb_gfx_window_reset(); */
    (void)qb_gfx_palette_reset();
    (void)qb_gfx_palette_get(0);

    /* ----- Batch B ----- */
    (void)qb_gfx_pset(50, 50, 7);
    (void)qb_gfx_line(10, 10, 100, 10, 7);
    (void)qb_gfx_box(10, 20, 150, 80, 7, 0);
    (void)qb_gfx_circle(200, 100, 30, 7, 0);

    /* ----- Batch C ----- */
    (void)qb_mouse_x();
    (void)qb_mouse_y();
    qb_mouse_hide();
    qb_mouse_show();
    (void)qb_keyhit();
    (void)qb_keydown(0);
    qb_keyclear();

    (void)qb_gfx_display();

    while (qb_gfx_poll_events() == 1) {
        qb_sleep(0.01);
    }
    return 0;
}
