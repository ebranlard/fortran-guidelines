#include <limits.h> // for CHAR_BIT
#include <stdio.h>

int main(){
    // The line below wont compile if a double is not 64 bits
    // It's a safety if you want to make sure C and fortran agrees
    //  on the size of those variables. On Fortran size use real(REAL64)
    char DUMMY1[sizeof(double) * CHAR_BIT == 64]; 
    char DUMMY2[sizeof(float)  * CHAR_BIT == 32]; 
    char DUMMY3[sizeof(int)    * CHAR_BIT == 32]; 

    printf("[ OK ] C double is 64 bits \n");

    return 0;
}
