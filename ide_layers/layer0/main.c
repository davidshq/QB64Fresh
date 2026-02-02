/*
 * Layer 0: Minimal window only.
 * Shows a window using the QB64Fresh runtime; event loop until user closes (X).
 * No QB64pe, no generated BASIC code. Proves runtime + SDL can show a window.
 */
#include "qb64fresh_rt.h"
#include <stdlib.h>
#include <stdio.h>

int main(void) {
    qb_screenshow();
    /*
     * Event loop: pump events so window close (X) sets stop_program and we exit.
     * Yield to the OS so the window manager can deliver events.
     */
    while (qb_gfx_poll_events() == 1) {
        qb_sleep(0.01);  /* 10 ms - yield so close/X and other events are delivered */
    }
    return 0;
}
