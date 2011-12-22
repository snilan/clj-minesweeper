#include <stdio.h>
#include <stdlib.h>
#include <wctype.h>
#include <assert.h>

int main(int argc, char* argv[]) {
  int num_chars = 0;
  int num_words = 0;

  int in_word = 0;

  if (argc == 2) {
    FILE *f = fopen(argv[1], "r");
    while (!feof(f)) {
      char c = fgetc(f);
      if (!iswspace(c)) {
	num_chars++;
	in_word = 1;
      }
      else {
	if (in_word) num_words++;
	in_word = 0;
      }
    }
  }
  printf("num_words : %d\n", num_words);
  printf("num_chars : %d\n", num_chars);
}

