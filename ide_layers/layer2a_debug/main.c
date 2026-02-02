/*
 * Layer 2a debug: Minimal test to isolate graphics vs console printing issue
 *
 * This directly calls both qb_print_string (console) and qb_gfx_print (window)
 * to verify which one actually works.
 */
#include "qb64fresh_rt.h"
#include <stdlib.h>
#include <stdio.h>

int main(void) {
    fprintf(stderr, "DEBUG: Starting...\n");
    fflush(stderr);

    /* Initialize runtime */
    qb_runtime_init();
    fprintf(stderr, "DEBUG: Runtime initialized\n");
    fflush(stderr);

    /* Initialize graphics (SCREEN 12 = 640x480) */
    int result = qb_gfx_screen(12, -1, -1, -1);
    fprintf(stderr, "DEBUG: qb_gfx_screen(12) returned %d\n", result);
    fflush(stderr);

    if (result != 0) {
        fprintf(stderr, "ERROR: Graphics initialization failed\n");
        return 1;
    }

    /* Set title */
    {
        QbString* title = qb_string_new("Layer2a Debug Test");
        qb_sub__title(title);
        qb_string_release(title);
    }
    fprintf(stderr, "DEBUG: Title set\n");
    fflush(stderr);

    /* Clear screen */
    qb_gfx_cls();
    fprintf(stderr, "DEBUG: CLS done\n");
    fflush(stderr);

    /* Set text color and position */
    qb_gfx_color(15, 0);  /* White on black */
    qb_gfx_locate(1, 1);
    fprintf(stderr, "DEBUG: Color and locate set\n");
    fflush(stderr);

    /* Print to graphics window (this SHOULD work) */
    int print_result = qb_gfx_print("Graphics Window: Hello from qb_gfx_print!");
    fprintf(stderr, "DEBUG: qb_gfx_print returned %d\n", print_result);
    fflush(stderr);

    /* Print to console (this goes to stdout, NOT the window) */
    {
        QbString* s = qb_string_new("Console: This goes to stdout, not the window");
        qb_print_string(s);
        qb_print_newline();
        qb_string_release(s);
    }

    /* Force display update */
    qb_gfx_display();
    fprintf(stderr, "DEBUG: Display updated\n");
    fflush(stderr);

    /* Draw something visible to confirm window is working */
    qb_gfx_pset(100, 100, 0xFFFFFF);  /* White pixel */
    qb_gfx_line(50, 50, 200, 100, 0xFF0000);  /* Red line */
    qb_gfx_box(150, 150, 300, 200, 0x00FF00, 1);  /* Green filled box */
    qb_gfx_display();
    fprintf(stderr, "DEBUG: Drew pixel, line, and box\n");
    fflush(stderr);

    /* Event loop - exit on window close or ESC */
    fprintf(stderr, "DEBUG: Entering event loop. Close window or press ESC to exit.\n");
    fflush(stderr);

    while (qb_gfx_poll_events() == 1) {
        QbString* k = qb_inkey();
        if (k) {
            if (qb_string_len(k) > 0) {
                const char* data = qb_string_data(k);
                if (data && data[0] == 27) {  /* ESC */
                    qb_string_release(k);
                    break;
                }
            }
            qb_string_release(k);
        }
        qb_sleep(0.01);
    }

    fprintf(stderr, "DEBUG: Exited event loop\n");
    fflush(stderr);

    qb_runtime_shutdown();
    fprintf(stderr, "DEBUG: Shutdown complete\n");
    fflush(stderr);

    return 0;
}
