#include <stdio.h>

#define BYTE unsigned char
#define UINT unsigned int
#define CUST(r) (0xFFFFF&(r))
#define DATE(r) ((r)>>19)
#define RATING unsigned int
#define MAKE_RATING(cust,date) ((date)<<19|(cust))

int main(int argc, char **argv)
{
	int customer_id = 382018, rating_date = 382;
	printf("%d, %d\n", customer_id, rating_date);
	RATING r = MAKE_RATING(customer_id, rating_date);
	printf("%d, %d\n", CUST(r), DATE(r));
}



