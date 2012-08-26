#include <stdio.h>

#define BYTE unsigned char
#define UINT unsigned int
#define CUST(r) (0x7FFFF&(r))
#define DATE(r) ((r)>>19)
#define RATING unsigned int
#define MAKE_RATING(cust,date) ((date)<<19|(cust))

#define NUM_SLOTS 100536000
RATING r[NUM_SLOTS];

void print_all() {
	UINT movie_id = 1, rating = 1, customer_id = 0, rating_date = 0;
	int i = 0;
	for (i = 0; i < NUM_SLOTS; i++) {
		if (r[i] == 0) {
			if (rating >= 5) {
				movie_id++;
				rating = 1;
			}
			else {
				rating++;
			}
		}
		else {
			customer_id = CUST(r[i]);
			rating_date = DATE(r[i]);
			//printf("%u, %u, %u, %u\n", movie_id, rating, customer_id, rating_date);
		}
	}
	printf("printed all\n");
}

int main(int argc, char **argv)
{
	FILE* fh;
	fh = fopen("ratings.dat", "rb");
	fread(r, sizeof(RATING), NUM_SLOTS, fh);
	fclose(fh);
	print_all();
	print_all();
	print_all();
}

