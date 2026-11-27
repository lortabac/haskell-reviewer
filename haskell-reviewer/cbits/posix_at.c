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

/* Full lstat of name in dirfd, for the stat check. out gets 10 values:
   st_mode, size, mtime s and ns, ctime s and ns, ino, dev, uid, gid.
   -1 and errno on error. */
int hr_lstat_full(int dirfd, const char *name, long long *out)
{
    struct stat st;
    if (fstatat(dirfd, name, &st, AT_SYMLINK_NOFOLLOW) != 0)
        return -1;
    out[0] = (long long)st.st_mode;
    out[1] = (long long)st.st_size;
#ifdef __APPLE__
    out[2] = (long long)st.st_mtimespec.tv_sec;
    out[3] = (long long)st.st_mtimespec.tv_nsec;
    out[4] = (long long)st.st_ctimespec.tv_sec;
    out[5] = (long long)st.st_ctimespec.tv_nsec;
#else
    out[2] = (long long)st.st_mtim.tv_sec;
    out[3] = (long long)st.st_mtim.tv_nsec;
    out[4] = (long long)st.st_ctim.tv_sec;
    out[5] = (long long)st.st_ctim.tv_nsec;
#endif
    out[6] = (long long)st.st_ino;
    out[7] = (long long)st.st_dev;
    out[8] = (long long)st.st_uid;
    out[9] = (long long)st.st_gid;
    return 0;
}

/* readlinkat. The result is not NUL-terminated. */
long hr_readlink_at(int dirfd, const char *name, char *buf, size_t len)
{
    return (long)readlinkat(dirfd, name, buf, len);
}

/* A DIR over a close-on-exec dup of fd, read from the start. The
   caller keeps fd; closedir closes the dup. NULL and errno on error,
   with the dup closed. A dup shares the read position with fd, hence
   the rewind. */
void *hr_dup_opendir(int fd)
{
    int d = fcntl(fd, F_DUPFD_CLOEXEC, 0);
    if (d < 0)
        return NULL;
    DIR *dir = fdopendir(d);
    if (dir == NULL) {
        int e = errno;
        close(d);
        errno = e;
        return NULL;
    }
    rewinddir(dir);
    return dir;
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
