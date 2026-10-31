/* Narrow host-clock compatibility, not business logic. The caller supplies
 * an integer POSIX instant and receives the host/TZ offset at that instant.
 * Keeping struct tm inside C avoids any Haskell ABI/layout assumption and
 * retains second-resolution historical offsets that TimeZone cannot store.
 */
#ifndef _DARWIN_C_SOURCE
#define _DARWIN_C_SOURCE 1
#endif
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE 1
#endif

#include <errno.h>
#include <stdint.h>
#include <time.h>

/* Returns zero on success, otherwise an errno-style failure code. It does
 * not read or change TZ itself; tzset asks libc to honor the inherited TZ.
 * Baseline and migrated tests must run with the same TZ and host tzdata.
 */
int hikaru_local_offset_seconds(int64_t epoch_seconds, int64_t *offset_seconds)
{
    time_t epoch = (time_t)epoch_seconds;
    struct tm local;

    if (offset_seconds == NULL)
        return EINVAL;
    if ((int64_t)epoch != epoch_seconds)
        return ERANGE;

    tzset();
    errno = 0;
    if (localtime_r(&epoch, &local) == NULL)
        return errno != 0 ? errno : EINVAL;

    *offset_seconds = (int64_t)local.tm_gmtoff;
    return 0;
}
