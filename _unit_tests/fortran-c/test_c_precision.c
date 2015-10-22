
#include <stdio.h>
#include <limits.h> // for CHAR_BIT
#include <stdint.h> // for standards int types

int main(){
    // The line below wont compile if a double is not 64 bits
    char DUMMY[sizeof(double) * CHAR_BIT == 64]; 

    int a;
    float b;
    double c;
    char d;
    int8_t  i8;
    int32_t i32;
    printf("       Size of byte   : %2d bits\n",CHAR_BIT);
    printf("       Size of char   : %2d bits\n" ,sizeof(d)*CHAR_BIT);
    printf("       Size of int8_t : %2d bits\n",sizeof(i8)*CHAR_BIT);
    printf("       Size of int32_t: %2d bits\n",sizeof(i32)*CHAR_BIT);
    printf("       Size of int    : %2d bits\n",sizeof(a)*CHAR_BIT);
    printf("       Size of float  : %2d bits\n",sizeof(b)*CHAR_BIT);
    printf("       Size of double : %2d bits\n",sizeof(c)*CHAR_BIT);
    
    printf("[ OK ] C double is 64 bits\n");
    return 0;
}
