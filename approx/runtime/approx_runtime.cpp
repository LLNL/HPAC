#include <stdio.h>
#include "approx.h"

void __approx_exec_call( void (*f)(void *), void *arg)
{
    printf("Start of Original Function\n");
    f(arg);
    printf("End of Original Function\n");
}
