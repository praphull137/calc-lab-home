/*
 * Name: Praphull Kumar Pandey
 * Student ID: 251ADB002
 * Repository: https://github.com/praphull137/calc-lab-home
 * Compile: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
 *
 * AI (ChatGPT): I used ChatGPT to discuss recursive-descent parsing,
 * operator precedence, error positions, numeric handling, and file I/O.
 * Conversation history: https://chatgpt.com/share/6ae12e1b-fe80-83eb-87b7-2798938006bb
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
 * Keep the first error so a later parser failure cannot hide the
 * original cause. Adding offset converts this line's position into
 * a position in the complete input file.
 * AI prompt summary: "How can I track error positions in a C parser?"
 */
static void error_at(size_t p)
{
    if (!failed) {
        failed = 1;
        errpos = offset + p + 1;
    }
}

/*
 * Ignore only the whitespace accepted by this grammar. Keeping this
 * in one helper means every parser level treats spacing consistently.
 */
static void spaces(void)
{
    while (src[pos] == ' ' || src[pos] == '\t' ||
           src[pos] == '\r')
        pos++;
}

static int expr(Value *v);

/*
 * Parentheses let a full expression act like one primary value, so
 * grouping can override the usual operator precedence. Number parsing
 * also validates the token before converting it to a numeric value.
 * AI prompt summary: "How should I parse numbers and parentheses?"
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

        /*
         * Require digits on both sides of the decimal point so forms
         * such as .5 and 5. are rejected by this assignment's grammar.
         */
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
 * Exponentiation is right-associative, so the exponent is parsed by
 * power() again. This groups 2 ** 3 ** 2 as 2 ** (3 ** 2), matching
 * the intended grammar instead of evaluating the left pair first.
 * AI prompt summary: "Why does recursive power parsing make ** right-associative?"
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

        /* Negative exponents are not allowed by the assignment. */
        if (rhs.n < 0) {
            error_at(rhs_start);
            return 0;
        }

        /* Fractional powers of negative numbers are not real-valued here. */
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
 * Each operand is parsed as a power before * or / is applied. Keeping
 * this level below power() and above expr() makes exponentiation bind
 * more tightly than multiplication/division, which bind more tightly
 * than addition/subtraction.
 * AI prompt summary: "How do parser functions enforce operator precedence?"
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

        /*
         * Do not consume the first character of ** here: power() owns
         * exponentiation, while a lone * is multiplication.
         */
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
             * Keep fractional results if either operand is floating-point.
             * Otherwise, truncate the quotient toward zero.
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
 * Parse a whole term before applying + or -, so multiplication,
 * division, and exponentiation are handled first. The loop then
 * applies addition/subtraction from left to right.
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
 * Reset the parser for each line but retain that line's absolute file
 * offset, allowing errors to use whole-file positions. After parsing,
 * reject leftover characters instead of silently accepting bad input.
 * AI prompt summary: "How can I report positions across multiple lines?"
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
 * Write results to a temporary stream first. If a later expression is
 * invalid, the program can print one error without leaving earlier
 * answers in the final output; valid results are copied only after
 * the input has been checked successfully.
 * AI prompt summary: "How can I avoid partial output when parsing fails?"
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

        /*
         * Ignore blank lines and full-line comments so they do not
         * produce output or affect expression evaluation.
         */
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
        /* Publish saved answers only after every expression has passed validation. */
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

/*
 * Use only the final path component when building output names, so
 * input directories are not accidentally copied into the filename.
 */
static const char *basename_of(const char *path)
{
    const char *p = strrchr(path, '/');
    return p ? p + 1 : path;
}

/*
 * Remove the last extension so the generated output name uses the
 * input's base name rather than ending up with two extensions.
 */
static void remove_extension(char *s)
{
    char *p = strrchr(s, '.');

    if (p)
        *p = '\0';
}

/*
 * Allow an existing directory as well as a newly created one, but
 * reject a path that exists as a non-directory or cannot be created.
 */
static int ensure_dir(const char *path)
{
    if (mkdir(path, 0777) == 0)
        return 1;

    struct stat st;
    return stat(path, &st) == 0 && S_ISDIR(st.st_mode);
}

/*
 * Build the required output filename from the input's base name, then
 * check each open/close operation so filesystem errors are not mistaken
 * for successful processing.
 * AI prompt summary: "How should a C program handle input and output files?"
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
 * Process only regular .txt files in this directory. Skipping folders
 * avoids unintentionally recursing into unrelated or generated output
 * directories.
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
 * Validate the command-line choices before opening files. Requiring
 * either one input file or one directory keeps the two processing
 * modes unambiguous and gives invalid options a clear usage message.
 * AI prompt summary: "How should I validate file and directory options?"
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