/* POSIX descriptor transport only; cache/source policy lives in Haskell. */
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/stat.h>
#include <stdio.h>
#include <dirent.h>
#include <string.h>
int hikaru_log_scan_root(void) {
    return open("/", O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
}
int hikaru_log_scan_dir_at(int parent, const char *name, int create) {
    if (create && mkdirat(parent, name, 0700) != 0 && errno != EEXIST) return -1;
    return openat(parent, name, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
}
int hikaru_log_scan_file_at(int parent, const char *name, int kind) {
    /* Inspect the opened descriptor before any I/O. In particular a FIFO
       with no peer must not block before Haskell can reject its file type. */
    int flags = O_NOFOLLOW | O_CLOEXEC | O_NONBLOCK;
    if (kind == 0) flags |= O_RDONLY;
    else if (kind == 1) flags |= O_WRONLY | O_CREAT | O_EXCL;
    else if (kind == 2) flags |= O_RDWR | O_CREAT;
    else { errno = EINVAL; return -1; }
    return openat(parent, name, flags, 0600);
}
int hikaru_log_scan_rename_at(int parent, const char *old_name, const char *new_name) {
    return renameat(parent, old_name, parent, new_name);
}
int hikaru_log_scan_unlink_at(int parent, const char *name) {
    return unlinkat(parent, name, 0);
}
void *hikaru_log_scan_list_open(int parent) {
    int copy = fcntl(parent, F_DUPFD_CLOEXEC, 0);
    if (copy == -1) return NULL;
    DIR *stream = fdopendir(copy);
    if (!stream) { int error = errno; close(copy); errno = error; }
    return stream;
}
int hikaru_log_scan_list_next(void *pointer, char *output) {
    errno = 0;
    struct dirent *entry = readdir((DIR *)pointer);
    if (!entry) return errno ? -1 : 0;
    size_t length = strlen(entry->d_name);
    if (length >= 1024) { errno = ENAMETOOLONG; return -1; }
    memcpy(output, entry->d_name, length + 1);
    return 1;
}
int hikaru_log_scan_list_close(void *pointer) {
    return closedir((DIR *)pointer);
}
