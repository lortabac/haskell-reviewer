/* Thin wrappers over the *at calls that the unix package lacks. See
   Note [Posix at] in HaskellReviewer.Effects.PosixAt. Wrappers, not
   direct foreign imports, so struct stat and DIR come from the system
   headers. */

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

/* lstat of name in dirfd. Kind: 0 other, 1 file, 2 dir, 3 symlink.
   -1 and errno on error. */
int hr_stat_at(int dirfd, const char *name, int *kind, long long *size)
{
    struct stat st;
    if (fstatat(dirfd, name, &st, AT_SYMLINK_NOFOLLOW) != 0)
        return -1;
    if (S_ISREG(st.st_mode))
        *kind = 1;
    else if (S_ISDIR(st.st_mode))
        *kind = 2;
    else if (S_ISLNK(st.st_mode))
        *kind = 3;
    else
        *kind = 0;
    *size = (long long)st.st_size;
    return 0;
}

/* readlinkat. The result is not NUL-terminated. */
long hr_readlink_at(int dirfd, const char *name, char *buf, size_t len)
{
    return (long)readlinkat(dirfd, name, buf, len);
}

/* fdopendir. Owns the fd from now on: closedir closes it. */
void *hr_fdopendir(int fd)
{
    return fdopendir(fd);
}

/* The next name, or NULL at the end (errno 0) or on error (errno set). */
const char *hr_readdir(void *dir)
{
    errno = 0;
    struct dirent *e = readdir((DIR *)dir);
    return e == NULL ? NULL : e->d_name;
}

int hr_closedir(void *dir)
{
    return closedir((DIR *)dir);
}
