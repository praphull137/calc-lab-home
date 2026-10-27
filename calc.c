
/*
 * Name: Praphull Kumar Pandey
 * Student ID: 251ADB002
 * Repository: https://github.com/praphull137/calc-lab-home
 * Compile: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
 * Resources used: OpenAI ChatGPT. I used ChatGPT to help write,
 * review, debug, and fix errors in the code.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <limits.h>
#include <dirent.h>
#include <sys/stat.h>

#define CAP 8192
#define PATH_CAP 2048

typedef struct {
    long double n;
    int floating;
} Value;

static const char *src;
static size_t pos, offset, errpos;
static int failed;

/*
 * I save the first error and ignore later ones because the output
 * should show only the first mistake. The offset helps me find its
 * position in the whole file, not just in the current line.
 */
static void error_at(size_t p)
{
    if (!failed) {
        failed = 1;
        errpos = offset + p + 1;
    }
}

/* Ignore spaces and tabs between parts of an expression. */
static void spaces(void)
{
    while (src[pos] == ' ' || src[pos] == '\t' ||
           src[pos] == '\r')
        pos++;
}

static int expr(Value *v);

/*
 * A value can be a number or an expression inside parentheses.
 * If there is an opening parenthesis, I evaluate what is inside
 * and then check that the closing parenthesis is there too.
 */
static int primary(Value *v)
{
    spaces();

    if (src[pos] == '(') {
        size_t open = pos++;

        if (!expr(v))
            return 0;

        spaces();

        if (src[pos] != ')') {
            error_at(pos > open ? pos : open);
            return 0;
        }

        pos++;
        return 1;
    }

    size_t start = pos, i = pos;
    int digits = 0;

    if (src[i] == '.') {
        error_at(i);
        return 0;
    }

    while (isdigit((unsigned char)src[i])) {
        i++;
        digits++;
    }

    if (src[i] == '.') {
        size_t dot = i++;

        /* A decimal number needs digits after the dot as well. */
        if (!isdigit((unsigned char)src[i])) {
            error_at(dot);
            return 0;
        }

        while (isdigit((unsigned char)src[i])) {
            i++;
            digits++;
        }
    }

    if (!digits) {
        error_at(start);
        return 0;
    }

    char buf[CAP];
    size_t len = i - start;

    if (len >= sizeof buf) {
        error_at(start);
        return 0;
    }

    memcpy(buf, src + start, len);
    buf[len] = '\0';

    errno = 0;
    char *end;
    long double n = strtold(buf, &end);

    if (errno == ERANGE || *end || !isfinite(n)) {
        error_at(start);
        return 0;
    }

    v->n = n;
    v->floating = strchr(buf, '.') != NULL;
    pos = i;

    return 1;
}

/*
 * I call power() again to read the exponent. This makes
 * 2 ** 3 ** 2 work as 2 ** (3 ** 2), from right to left.
 */
static int power(Value *v)
{
    if (!primary(v))
        return 0;

    spaces();

    if (src[pos] == '*' && src[pos + 1] == '*') {
        size_t op = pos;
        pos += 2;
        spaces();

        size_t rhs_start = pos;
        Value rhs;

        if (!power(&rhs))
            return 0;

        /* The assignment does not allow negative exponents. */
        if (rhs.n < 0) {
            error_at(rhs_start);
            return 0;
        }

        /* A negative base with a fractional exponent is not real here. */
        if (v->n < 0 && rhs.n != floorl(rhs.n)) {
            error_at(op);
            return 0;
        }

        if (v->n == 0 && rhs.n < 0) {
            error_at(rhs_start);
            return 0;
        }

        long double n = powl(v->n, rhs.n);

        if (!isfinite(n) || fabsl(n) > (long double)LLONG_MAX) {
            error_at(op);
            return 0;
        }

        v->n = n;
        v->floating = v->floating || rhs.floating ||
                      rhs.n != floorl(rhs.n);
    }

    return 1;
}

/*
 * Multiplication and division are handled before addition and
 * subtraction. I use power() here so exponentiation is handled first.
 */
static int term(Value *v)
{
    if (!power(v))
        return 0;

    for (;;) {
        spaces();
        char op = src[pos];

        if (op != '*' && op != '/')
            return 1;

        /* Two stars together mean exponentiation, not multiplication. */
        if (op == '*' && src[pos + 1] == '*')
            return 1;

        size_t at = pos++;
        spaces();

        size_t rhs_start = pos;
        Value rhs;

        if (!power(&rhs))
            return 0;

        int floating = v->floating || rhs.floating;
        long double n;

        if (op == '*') {
            n = v->n * rhs.n;
        } else {
            if (rhs.n == 0) {
                error_at(rhs_start);
                return 0;
            }

            /*
             * If either value is a decimal, I keep the fractional
             * part. Otherwise, division truncates toward zero.
             */
            n = floating ? v->n / rhs.n
                         : truncl(v->n / rhs.n);
        }

        if (!isfinite(n) ||
            (!floating &&
             (n > (long double)LLONG_MAX ||
              n < (long double)LLONG_MIN))) {
            error_at(at);
            return 0;
        }

        v->n = n;
        v->floating = floating;
    }
}

/*
 * By the time we get here, multiplication, division and powers
 * have already been handled. Addition and subtraction are then
 * evaluated from left to right.
 */
static int expr(Value *v)
{
    if (!term(v))
        return 0;

    for (;;) {
        spaces();
        char op = src[pos];

        if (op != '+' && op != '-')
            return 1;

        size_t at = pos++;
        Value rhs;

        if (!term(&rhs))
            return 0;

        long double n = op == '+'
                      ? v->n + rhs.n
                      : v->n - rhs.n;

        int floating = v->floating || rhs.floating;

        if (!isfinite(n) ||
            (!floating &&
             (n > (long double)LLONG_MAX ||
              n < (long double)LLONG_MIN))) {
            error_at(at);
            return 0;
        }

        v->n = n;
        v->floating = floating;
    }
}

/*
 * I reset the character position for every expression but keep
 * the file offset. This lets errors point to the right character
 * in the original file. Any leftover character makes it invalid.
 */
static int evaluate(const char *line, size_t line_offset, Value *v)
{
    src = line;
    pos = 0;
    offset = line_offset;
    failed = 0;

    if (!expr(v))
        return 0;

    spaces();

    if (src[pos] != '\0' && src[pos] != '\n') {
        error_at(pos);
        return 0;
    }

    return 1;
}

/*
 * I write results to a temporary file first. If a later expression
 * is wrong, I can discard the earlier results and output only the
 * first error, instead of leaving partial answers in the output.
 */
static int process_stream(FILE *in, FILE *out)
{
    char line[CAP];
    FILE *temp = tmpfile();

    if (!temp)
        return 0;

    size_t total = 0;
    size_t first_error = 0;
    int has_error = 0;

    while (fgets(line, sizeof line, in)) {
        size_t len = strlen(line);
        size_t first = 0;

        while (first < len &&
               (line[first] == ' ' || line[first] == '\t' ||
                line[first] == '\r'))
            first++;

        /* Blank lines and lines starting with # are skipped. */
        if (first == len || line[first] == '\n' ||
            line[first] == '#') {
            total += len;
            continue;
        }

        Value v;

        if (!evaluate(line, total, &v)) {
            has_error = 1;
            first_error = errpos;
            break;
        }

        int n;

        if (!v.floating &&
            v.n >= (long double)LLONG_MIN &&
            v.n <= (long double)LLONG_MAX) {
            n = fprintf(temp, "%.0Lf\n", v.n);
        } else {
            n = fprintf(temp, "%.15Lg\n", v.n);
        }

        if (n < 0) {
            fclose(temp);
            return 0;
        }

        total += len;
    }

    if (ferror(in)) {
        fclose(temp);
        return 0;
    }

    if (has_error) {
        if (fprintf(out, "ERROR:%zu\n", first_error) < 0) {
            fclose(temp);
            return 0;
        }
    } else {
        /* Copy the saved answers only after every expression succeeds. */
        if (fflush(temp) != 0 || fseek(temp, 0, SEEK_SET) != 0) {
            fclose(temp);
            return 0;
        }

        int ch;

        while ((ch = fgetc(temp)) != EOF) {
            if (fputc(ch, out) == EOF) {
                fclose(temp);
                return 0;
            }
        }

        if (ferror(temp)) {
            fclose(temp);
            return 0;
        }
    }

    fclose(temp);
    return !ferror(out);
}

/* Get the filename without the folders before it. */
static const char *basename_of(const char *path)
{
    const char *p = strrchr(path, '/');
    return p ? p + 1 : path;
}

/* Remove the extension before building the output filename. */
static void remove_extension(char *s)
{
    char *p = strrchr(s, '.');

    if (p)
        *p = '\0';
}

/* Use the directory if it already exists; otherwise, create it. */
static int ensure_dir(const char *path)
{
    if (mkdir(path, 0777) == 0)
        return 1;

    struct stat st;
    return stat(path, &st) == 0 && S_ISDIR(st.st_mode);
}

/*
 * I open the input and create its output file using the required
 * naming format. If either file operation fails, the run is reported
 * as unsuccessful instead of silently continuing.
 */
static int process_file(const char *path, const char *outdir)
{
    char stem[PATH_CAP], output[PATH_CAP];

    if (strlen(basename_of(path)) >= sizeof stem)
        return 0;

    strcpy(stem, basename_of(path));
    remove_extension(stem);

    int n = snprintf(output, sizeof output,
                     "%s/%s_Praphull_Kumar_251ADB002.txt",
                     outdir, stem);

    if (n < 0 || (size_t)n >= sizeof output)
        return 0;

    FILE *in = fopen(path, "r");

    if (!in) {
        perror(path);
        return 0;
    }

    FILE *out = fopen(output, "w");

    if (!out) {
        perror(output);
        fclose(in);
        return 0;
    }

    int ok = process_stream(in, out);

    if (fclose(in) != 0)
        ok = 0;

    if (fclose(out) != 0)
        ok = 0;

    return ok;
}

/*
 * I only process .txt files directly inside the chosen folder.
 * Checking for regular files means that subfolders are skipped.
 */
static int process_directory(const char *path, const char *outdir)
{
    DIR *dir = opendir(path);

    if (!dir) {
        perror(path);
        return 0;
    }

    struct dirent *entry;
    int ok = 1;

    while ((entry = readdir(dir)) != NULL) {
        size_t len = strlen(entry->d_name);

        if (len <= 4 ||
            strcmp(entry->d_name + len - 4, ".txt") != 0)
            continue;

        char file[PATH_CAP];
        int n = snprintf(file, sizeof file, "%s/%s",
                         path, entry->d_name);

        if (n < 0 || (size_t)n >= sizeof file) {
            ok = 0;
            continue;
        }

        struct stat st;

        if (stat(file, &st) != 0 || !S_ISREG(st.st_mode))
            continue;

        if (!process_file(file, outdir))
            ok = 0;
    }

    closedir(dir);
    return ok;
}

static void usage(const char *program)
{
    fprintf(stderr,
            "Usage: %s [-d DIR] [-o OUTDIR] [input.txt]\n",
            program);
}

/*
 * I read the command-line options first, then choose the output
 * directory. After checking the arguments, I process either one
 * input file or all matching files in a directory.
 */
int main(int argc, char **argv)
{
    const char *file = NULL;
    const char *dir = NULL;
    const char *outdir = NULL;
    char default_dir[PATH_CAP];

    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "-d") ||
            !strcmp(argv[i], "--dir")) {
            if (++i >= argc) {
                usage(argv[0]);
                return 2;
            }

            dir = argv[i];
        } else if (!strcmp(argv[i], "-o") ||
                   !strcmp(argv[i], "--output-dir")) {
            if (++i >= argc) {
                usage(argv[0]);
                return 2;
            }

            outdir = argv[i];
        } else if (argv[i][0] == '-') {
            usage(argv[0]);
            return 2;
        } else if (!file) {
            file = argv[i];
        } else {
            usage(argv[0]);
            return 2;
        }
    }

    if ((!file && !dir) || (file && dir)) {
        usage(argv[0]);
        return 2;
    }

    if (!outdir) {
        const char *base = basename_of(dir ? dir : file);

        if (strlen(base) >= sizeof default_dir - 64) {
            fprintf(stderr, "Input name too long.\n");
            return 2;
        }

        char stem[PATH_CAP];
        strcpy(stem, base);

        if (!dir)
            remove_extension(stem);

        int n = snprintf(default_dir, sizeof default_dir,
                         "%s_Praphull_251ADB002", stem);

        if (n < 0 || (size_t)n >= sizeof default_dir)
            return 2;

        outdir = default_dir;
    }

    if (!ensure_dir(outdir)) {
        fprintf(stderr, "Cannot create output directory: %s\n",
                outdir);
        return 2;
    }

    if (dir)
        return process_directory(dir, outdir) ? 0 : 2;

    return process_file(file, outdir) ? 0 : 2;
}
