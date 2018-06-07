#include <stdlib.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
	FILE *fp;
	unsigned char c;
	unsigned int value;

	fp = fopen(argv[1], "r");

	while (!feof(fp)) {
		fscanf(fp, "%d", &value);
		if (!feof(fp)) {
			c = (unsigned char) value;
			fwrite(&c, 1, sizeof(unsigned char), stdout);
		}
	}

	fclose(fp);

	return 0;
}
