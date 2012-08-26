#include <stdio.h>

#define BYTE unsigned char
#define UINT unsigned int
#define CUST(r) (0x7FFFF&(r))
#define DATE(r) ((r)>>19)
#define RATING unsigned int
#define MAKE_RATING(cust,date) ((date)<<19|(cust))

#define NUM_MOVIES 17700
#define NUM_SCORES 5
#define RATINGS_PER_MOVIE 1135 
#define NUM_RATINGS NUM_MOVIES * NUM_SCORES * RATINGS_PER_MOVIE
#define NUM_SLOTS NUM_RATINGS + NUM_MOVIES * NUM_SCORES
#define FIRST_CUST_ID 18
#define FIRST_DATE 2
#define CUST_INCREMENT 1
#define DATE_INCREMENT 3
#define MAX_CUST_ID 480000
#define MAX_DATE 1500
RATING r[NUM_SLOTS];

int main(int argc, char **argv)
{
	int i = 0;
	FILE* fh;
	UINT customer_id = FIRST_CUST_ID, rating_date = FIRST_DATE;
	UINT movie_id = 1;
	UINT rating = 1;
	int k = 0;

	for (movie_id = 1; movie_id <= NUM_MOVIES; movie_id ++) {
		for (rating = 1; rating <= NUM_SCORES; rating++) {
			for (k = 0; k < RATINGS_PER_MOVIE; k++) {
				r[i++] = MAKE_RATING(customer_id, rating_date);
				//printf("%u, %u, %u, %u\n", movie_id, rating, customer_id, rating_date);
				customer_id += CUST_INCREMENT;
				if (customer_id > MAX_CUST_ID)
					customer_id = FIRST_CUST_ID;
				rating_date += DATE_INCREMENT;
				if (rating_date > MAX_DATE)
					rating_date = FIRST_DATE;
			}
			r[i++] = 0;
		}
	}

	fh = fopen("ratings.dat", "wb");
	fwrite(r, sizeof(RATING), NUM_SLOTS, fh);
	fclose(fh);
}



