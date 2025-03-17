#include <stdlib.h>
#include<cstdio>

int counter = 0;

int g()
{
    return counter++;
}

int f( int x, int y) {
    return x * 2 + y;
}
int main()
{
    printf("%d\n", f(g(), g()));
}while(m != 0)
{
    int r = n % m;
    n = m;
    m = r;
}