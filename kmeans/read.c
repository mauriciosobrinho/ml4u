#include <stdlib.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
	FILE *fp;
	unsigned char value;

	fp = fopen(argv[1], "r");

	while (!feof(fp)) {
		fread(&value, 1, sizeof(unsigned char), fp);
		if (!feof(fp)) {
			printf("%d\n", value);
		}
	}

	fclose(fp);

	return 0;
}
