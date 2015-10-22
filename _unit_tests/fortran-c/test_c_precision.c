/* This program computes the size of variable using sizeof operator.*/

#include <stdio.h>
#include <limits.h> // for CHAR_BIT

int main(){
    // The line below wont compile if a double is not 64 bits
    char DUMMY[sizeof(double) * CHAR_BIT == 64]; 

    int a;
    float b;
    double c;
    char d;
    printf("Size of byte  : %2d bits\n",CHAR_BIT);
    printf("Size of char  : %2d bits\n" ,sizeof(d)*CHAR_BIT);
    printf("Size of int   : %2d bits\n",sizeof(a)*CHAR_BIT);
    printf("Size of float : %2d bits\n",sizeof(b)*CHAR_BIT);
    printf("Size of double: %2d bits\n",sizeof(c)*CHAR_BIT);
    return 0;
}
