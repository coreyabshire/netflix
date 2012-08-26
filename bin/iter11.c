#include <stdio.h>
#include <string.h>
#include <math.h>

#define BYTE unsigned char
#define UINT unsigned int
#define CUST(r) (0x7FFFF&(r))
#define DATE(r) ((r)>>19)
#define RATING unsigned int
#define MAKE_RATING(cust,date) ((date)<<19|(cust))
#define PREDF(p) ((float)(p)/10000.0)
#define PREDI(p) ((unsigned short)((p)*10000.0))

#define NUM_CUSTOMERS 480189
#define NUM_MOVIES 17770
#define NUM_FACTORS 10
#define NUM_RATINGS 100480507
#define NUM_SLOTS 100569357
RATING r[NUM_SLOTS];
unsigned short p[NUM_SLOTS];
float cV[NUM_FACTORS][NUM_CUSTOMERS];
float mV[NUM_FACTORS][NUM_MOVIES];
float avg[NUM_MOVIES];

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

void init_resid() {
	UINT movie_id = 1, rating = 1, customer_id = 0, rating_date = 0;
	int i = 0;
	float pred = 0.0;
	UINT rCount[NUM_MOVIES];
	UINT rSum[NUM_MOVIES];
	for (i = 0; i < NUM_SLOTS; i++) {
		if (r[i] == 0) { if (rating >= 5) { movie_id++; rating = 1; } else { rating++; } } else { customer_id = CUST(r[i]); rating_date = DATE(r[i]);
			rCount[movie_id-1]++;
			rSum[movie_id-1]+=rating;
			pred = (float) rating;
			p[i] = (unsigned short) round(pred * 10000.0);
		}
	}
	for (i = 0; i < NUM_MOVIES; i++) {
		avg[i] = (float) ((double)rCount[i] / (double)rSum[i]);
	}
	for (i = 0; i < NUM_SLOTS; i++) {
		if (r[i] == 0) { if (rating >= 5) { movie_id++; rating = 1; } else { rating++; } } else { customer_id = CUST(r[i]); rating_date = DATE(r[i]);
			p[i] = PREDI(avg[movie_id - 1]);
		}
	}
	printf("resid initted\n");
}

void rmse() {
	UINT movie_id = 1, rating = 1, customer_id = 0, rating_date = 0;
	int i = 0;
	float pred = 0.0;
	float rmse = 0.0;
	float delta = 0.0;
	float sum = 0.0;
	int count = 0;
	for (i = 0; i < NUM_SLOTS; i++) {
		if (r[i] == 0) { if (rating >= 5) { movie_id++; rating = 1; } else { rating++; } } else { customer_id = CUST(r[i]); rating_date = DATE(r[i]);
			pred = PREDF(p[i]);
			delta = (float) rating - pred;
			sum += delta * delta;
			count++;
		}
	}
	rmse = sqrt(sum/count);
	printf("rmse: %f\n", rmse);
}

void train() 
{
	UINT movie_id = 1, rating = 1, customer_id = 0, rating_date = 0;
	int i = 0;
	float pred = 0.0;
	for (i = 0; i < NUM_SLOTS; i++) {
		if (r[i] == 0) { if (rating >= 5) { movie_id++; rating = 1; } else { rating++; } } else {
			customer_id = CUST(r[i]);
			rating_date = DATE(r[i]);
			pred = (float) rating;
			p[i] = (unsigned short) round(pred * 10000.0);
		}
	}
	printf("trained\n");
}

int main(int argc, char **argv)
{
	FILE* fh;
	printf("clearing r\n");
	memset((void *) r, 0, sizeof(r));
	printf("clearing p\n");
	memset((void *) p, 0, sizeof(p));
	printf("init print\n");
	print_all();
	printf("loading file\n");
	fh = fopen("ratings.dat", "rb");
	fread(r, sizeof(RATING), NUM_SLOTS, fh);
	fclose(fh);
	printf("file loaded\n");
	init_resid();
	rmse();
}

