#include "approx.h"

void __approx_exec_call( void (*f)(void *), void *arg)
{
    f(arg);
}
