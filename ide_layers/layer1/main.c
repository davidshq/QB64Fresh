/*
 * Layer 1: Minimal window with qb_gfx_screen first (matches IDE init).
 * Same as layer0 but calls qb_gfx_screen(-1, -1, 1, 0) before show.
 * Separate version: do not edit layer0.
 */
#include "qb64fresh_rt.h"
#include <stdlib.h>
#include <stdio.h>

int main(void) {
    if (qb_gfx_screen(-1, -1, 1, 0) != 0) {
        fprintf(stderr, "qb_gfx_screen failed\n");
        return 1;
    }
    qb_screenshow();
    /*
     * Event loop: pump events so window close (X) sets stop_program and we exit.
     * Yield to the OS so the window manager can deliver events (fixes close
     * button not working when the loop would otherwise spin at 100% CPU).
     */
    while (qb_gfx_poll_events() == 1) {
        qb_sleep(0.01);  /* 10 ms - yield so close/X and other events are delivered */
    }
    return 0;
}
