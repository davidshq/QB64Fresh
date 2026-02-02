/*
 * Layer 2b debug: Check if qb_gfx_poll_events returns 0 immediately
 */
#include "qb64fresh_rt.h"
#include <stdlib.h>
#include <stdio.h>

int main(void) {
    fprintf(stderr, "DEBUG: Starting...\n");
    fflush(stderr);

    qb_runtime_init();
    fprintf(stderr, "DEBUG: Runtime initialized\n");
    fflush(stderr);

    int result = qb_gfx_screen(12, -1, -1, -1);
    fprintf(stderr, "DEBUG: qb_gfx_screen(12) returned %d\n", result);
    fflush(stderr);

    if (result != 0) {
        fprintf(stderr, "ERROR: Graphics init failed\n");
        return 1;
    }

    {
        QbString* title = qb_string_new("Layer2b Debug");
        qb_sub__title(title);
        qb_string_release(title);
    }

    qb_gfx_cls();
    qb_gfx_color(15, 0);
    qb_gfx_printstring(10, 10, "Test - press ESC or close window");
    qb_gfx_display();
    fprintf(stderr, "DEBUG: Display shown\n");
    fflush(stderr);

    /* Test poll_events multiple times */
    for (int i = 0; i < 5; i++) {
        int poll_result = qb_gfx_poll_events();
        fprintf(stderr, "DEBUG: poll_events #%d returned %d\n", i+1, poll_result);
        fflush(stderr);
        if (poll_result == 0) {
            fprintf(stderr, "DEBUG: Window closed, exiting\n");
            return 0;
        }
        qb_sleep(0.5);
    }

    fprintf(stderr, "DEBUG: Entering main loop\n");
    fflush(stderr);

    int loop_count = 0;
    int poll_result;
    while ((poll_result = qb_gfx_poll_events()) == 1) {
        loop_count++;
        if (loop_count <= 5 || loop_count % 100 == 0) {
            fprintf(stderr, "DEBUG: main loop #%d, poll=%d\n", loop_count, poll_result);
            fflush(stderr);
        }
        QbString* k = qb_inkey();
        if (k) {
            size_t len = qb_string_len(k);
            if (len > 0) {
                const char* data = qb_string_data(k);
                fprintf(stderr, "DEBUG: inkey len=%zu, data[0]=%d\n", len, (int)(unsigned char)data[0]);
                fflush(stderr);
                if (data && data[0] == 27) {
                    fprintf(stderr, "DEBUG: ESC pressed\n");
                    qb_string_release(k);
                    break;
                }
            }
            qb_string_release(k);
        }
        qb_sleep(0.01);
    }

    fprintf(stderr, "DEBUG: Loop exited after %d iterations, last poll=%d\n", loop_count, poll_result);
    qb_runtime_shutdown();
    return 0;
}
