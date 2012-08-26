#include <stdio.h>

#define BYTE unsigned char
#define UINT unsigned int
#define CUST(r) (0x7FFFF&(r))
#define DATE(r) ((r)>>19)
#define RATING unsigned int
#define MAKE_RATING(cust,date) ((date)<<19|(cust))

#define NUM_RATINGS 20
RATING r[NUM_RATINGS];

int main(int argc, char **argv)
{
	int i = 0;
	FILE* fh;
	UINT customer_id = 382018, rating_date = 382;
	for (i = 0; i < NUM_RATINGS; i++) {
		r[i] = MAKE_RATING(customer_id, rating_date);
		customer_id += 1;
		rating_date += 3;
	}
	for (i = 0; i < NUM_RATINGS; i++) {
		printf("%u, %u\n", CUST(r[i]), DATE(r[i]));
	}
	fh = fopen("ratings.dat", "wb");
	fwrite(r, sizeof(RATING), NUM_RATINGS, fh);
	fclose(fh);
}



