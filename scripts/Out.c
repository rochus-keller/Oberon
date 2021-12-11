#include "Out.h"

void Out$Int(int32_t i, int32_t n)
{
    printf("%*d", n, i);
}

void Out$Ln()
{
	printf("\n");
}

void Out$Char(char c)
{
	printf("%c", c );
}

void Out$init$()
{
}
