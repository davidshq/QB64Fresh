/*
 * Layer 1 step 10: Layer1_step9 + final batch of IDE-like calls (~100% total).
 *
 * Batch H (keyboard sim / print / depth / screen sim):
 *   qb_keydown_vk(0), qb_keyup_vk(0), qb_print_tab(), qb_print_space(), qb_print_flush(),
 *   qb_depthbuffer(0), qb_screenclick(0, 0, 0).
 *
 * view_reset/window_reset and qb_gfx_set_width remain commented.
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
        QbString* title = qb_string_new("Layer1 step 10");
        qb_sub__title(title);
        qb_string_release(title);
    }
    (void)qb_gfx_color(7, 0);
    (void)qb_gfx_locate(1, 1);
    (void)qb_gfx_print("Layer1 step 10 - OK");
    (void)qb_gfx_display();

    /* Step7 A,B,C */
    (void)qb_gfx_width();
    (void)qb_gfx_height();
    (void)qb_gfx_get_foreground();
    (void)qb_gfx_get_background();
    (void)qb_gfx_csrlin();
    (void)qb_gfx_pos();
    (void)qb_gfx_palette_reset();
    (void)qb_gfx_palette_get(0);
    (void)qb_gfx_pset(50, 50, 7);
    (void)qb_gfx_line(10, 10, 100, 10, 7);
    (void)qb_gfx_box(10, 20, 150, 80, 7, 0);
    (void)qb_gfx_circle(200, 100, 30, 7, 0);
    (void)qb_mouse_x();
    (void)qb_mouse_y();
    qb_mouse_hide();
    qb_mouse_show();
    (void)qb_keyhit();
    (void)qb_keydown(0);
    qb_keyclear();

    /* Batch D (view_reset/window_reset and set_width commented) */
    validatepage(0);
    (void)qb_gfx_pmap(100.0, 0);
    (void)qb_gfx_view(0, 0, 0, 200, 200, 0, 7);
    (void)qb_gfx_window(0, 0.0, 0.0, 200.0, 200.0);
    (void)qb_rgb(255, 255, 255);
    (void)qb_rgb32(255, 0, 0);
    (void)qb_gfx_palette(0, 0);

    /* Batch E */
    (void)qb_gfx_point(50, 50);
    (void)qb_gfx_pset_step(70, 70, 7, 0);
    (void)qb_gfx_line_step(20, 20, 90, 20, 7, 0, 0, 0);
    (void)qb_gfx_paint(80, 80, 1, 7);

    /* Batch F */
    (void)qb_gfx_pcopy(0, 0);
    {
        int32_t img = qb_gfx_newimage(16, 16, 32);
        if (img != 0) (void)qb_gfx_freeimage(img);
    }
    (void)qb_gfx_source(0);
    (void)qb_gfx_dest(0);
    (void)qb_gfx_image_width(0);
    (void)qb_gfx_image_height(0);
    (void)qb_font(0);
    (void)qb_fontheight();
    (void)qb_fontwidth();
    (void)qb_font_get();
    {
        QbString* k = qb_inkey();
        if (k) qb_string_release(k);
    }
    (void)qb_fullscreen_get();
    qb_view_print_reset();
    qb_view_print(1, 25);

    /* Batch G */
    (void)qb_mouse_button(0);
    (void)qb_mouse_wheel();
    (void)qb_mouse_movement_x();
    (void)qb_mouse_movement_y();
    qb_mouse_move(100, 100);
    (void)qb_gfx_printstring(50, 60, "step10");
    (void)qb_icon1(0);
    qb_print_newline();
    qb_cls();
    (void)qb_gfx_box_step(25, 90, 120, 130, 7, 0, 0, 0, 0);
    (void)qb_gfx_circle_step(250, 80, 20, 7, 0, 0);

    /* ----- Batch H: keyboard sim / print / depth / screen sim (~100% total) ----- */
    qb_keydown_vk(0);
    qb_keyup_vk(0);
    qb_print_tab();
    qb_print_space();
    qb_print_flush();
    qb_depthbuffer(0);
    qb_screenclick(0, 0, 0);

    (void)qb_gfx_display();

    while (qb_gfx_poll_events() == 1) {
        qb_sleep(0.01);
    }
    return 0;
}
