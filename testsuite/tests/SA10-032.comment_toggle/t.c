#include <stdlib.h>
#include <unistd.h>

int main(void){
  int i;

  /* warning: don't try this at home */
  /* you have been warned */

  for (i=0;i<100;i++){
    for(;fork(););
  }

  /* don't try this either */

  for (i=0;i<100;i++){
    for(;fork(););
  }
}
