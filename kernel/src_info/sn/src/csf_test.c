#include <assert.h>
#include "db_capi.c"

/* Simple test for CSF (Character Separated Fields) implementation.
 * Test is PASSED if test executable is finished with exit code 0
 * and without any messages on stderr/stdout.
 * Otherwise test is FAILED.
 */

#define N 6
char *s[] = {
    0,
    "",
    "one",
    "one\001",
    "\001one",
    "one\001two"
};

char *p[][2] = {
    {0, 0},
    {"", 0},
    {"one", 0},
    {"one", ""},
    {"", "one"},
    {"one", "two"}
};

int nof[] = { 0, 1, 1, 2, 2, 2 };

void test()
{
    CSF *csf;
    int i, j;

    for (j = 0; j < N; j++) {

	csf = csf_init(s[j]);
	assert(nof[j] == csf_get_field_count(csf));
	for (i = 1; i <= nof[j]; i++) {
	    if (p[j][i - 1] == 0) {
		assert(csf_get_field(csf, i) == 0);
	    } else {
		assert(strcmp(csf_get_field(csf, i), p[j][i - 1]) == 0);
	    }
	}
	csf_free(csf);

    }
}


int main()
{
    test();
    return 0;
}
