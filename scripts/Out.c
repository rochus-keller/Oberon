#include "Out.h"

void Out$Int(int32_t i, int32_t n)
{
    printf("%*d", n, i);
}

void Out$Real(float x, int32_t n)
{
    printf("%*e", n, x);
}

void Out$Ln()
{
	printf("\n");
}

void Out$Char(char c)
{
	printf("%c", c );
}

void Out$String(const struct OBX$Array$1 str)
{
	OBX$PrintA(0,(const char*)str.$a);
}

void Out$init$()
{
}

void Out$Open()
{
}

void Out$cmd$(const char* name)
{
	if(name==0) return Out$init$;
	return 0;
}
