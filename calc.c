
/*
 * Praphull Kumar Pandey
 * Student ID: 251ADB002
 * Repository: https://github.com/praphull137/calc-lab-home
 * Compile: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
 * Resources: ChatGPT (OpenAI). Record the prompts used and
 * describe the specific code sections where assistance was used.
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>

static const char *input;
static size_t pos;
static size_t error_pos;

/* Skip spaces and tabs between tokens. */
static void skip_spaces(void)
{
    while (input[pos] == ' ' || input[pos] == '\t')
        pos++;
}

/* Read one non-negative integer. */
static long long parse_number(void)
{
    skip_spaces();

    if (!isdigit((unsigned char)input[pos])) {
        error_pos = pos;
        return 0;
    }

    long long value = 0;

    while (isdigit((unsigned char)input[pos])) {
        int digit = input[pos] - '0';

        if (value > (LLONG_MAX - digit) / 10) {
            error_pos = pos;
            return 0;
        }

        value = value * 10 + digit;
        pos++;
    }

    return value;
}

/* Evaluate a sequence of additions and subtractions. */
static int evaluate(long long *result)
{
    long long value = parse_number();

    if (error_pos != (size_t)-1)
        return 0;

    for (;;) {
        skip_spaces();

        char op = input[pos];

        if (op != '+' && op != '-')
            break;

        pos++;

        long long rhs = parse_number();

        if (error_pos != (size_t)-1)
            return 0;

        if (op == '+') {
            if (value > LLONG_MAX - rhs) {
                error_pos = pos;
                return 0;
            }
            value += rhs;
        } else {
            if (value < LLONG_MIN + rhs) {
                error_pos = pos;
                return 0;
            }
            value -= rhs;
        }
    }

    skip_spaces();

    if (input[pos] != '\0' && input[pos] != '\n') {
        error_pos = pos;
        return 0;
    }

    *result = value;
    return 1;
}

int main(void)
{
    char line[4096];

    while (fgets(line, sizeof line, stdin) != NULL) {
        if (line[0] == '\n' || line[0] == '\0')
            continue;

        input = line;
        pos = 0;
        error_pos = (size_t)-1;

        long long result;

        if (evaluate(&result)) {
            printf("%lld\n", result);
        } else {
            printf("ERROR:%zu\n", error_pos + 1);
        }
    }

    return 0;
}
