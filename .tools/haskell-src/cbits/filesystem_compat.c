/* Only portable libc structure/constant access. Traversal, symlink identity,
 * error handling and deletion policy are all implemented in Haskell. */
#include <dirent.h>
#include <fcntl.h>
#include <stdint.h>
#include <sys/stat.h>
#include <unistd.h>

const char *hikaru_readdir_name(DIR *stream) {
    struct dirent *entry = readdir(stream);
    return entry ? entry->d_name : NULL;
}

int hikaru_stat_at(int fd, const char *name, uint64_t *device,
                   uint64_t *inode, uint64_t *mode) {
    struct stat value;
    int result = fstatat(fd, name, &value, AT_SYMLINK_NOFOLLOW);
    if (result == 0) {
        *device = (uint64_t)value.st_dev;
        *inode = (uint64_t)value.st_ino;
        *mode = (uint64_t)value.st_mode;
    }
    return result;
}

int hikaru_unlink_at(int fd, const char *name, int directory) {
    return unlinkat(fd, name, directory ? AT_REMOVEDIR : 0);
}
