#include <mysql/mysql.h> 
#include <stdio.h> 
#include <string.h> 

#define BYTE unsigned char
#define UINT unsigned int
#define CUST(r) (0x7FFFF&(r))
#define DATE(r) ((r)>>19)
#define RATING unsigned int
#define MAKE_RATING(cust,date) ((date)<<19|(cust))
#define NUM_SLOTS 100569007
RATING r[NUM_SLOTS];

int main(int argc, char **argv) 
{ 
	MYSQL mysql; 
	MYSQL_RES *res; 
	MYSQL_ROW row; 
	FILE* fh;
	int count = 0, i = 0;
	UINT movie_id = 0, last_movie_id = 1, rating = 0, last_rating = 1, customer_id = 0, rating_date = 0;

	char query[255]; 
	mysql_init(&mysql); 
	mysql_real_connect(&mysql,"localhost","","","netflix",0,NULL,0); 
	sprintf(query,"SELECT movie_id, rating, customer_altid, rating_altdate FROM quick_train"); 
	mysql_real_query(&mysql,query,(unsigned int)strlen(query)); 
	res = mysql_use_result(&mysql); 
	while(row = mysql_fetch_row(res)) 
	{
		movie_id = atoi(row[0]);
		rating = atoi(row[1]);
		customer_id = atoi(row[2]);
		rating_date = atoi(row[3]);
		/* output nulls for movie changes, including gaps */
		if (movie_id != last_movie_id) {
		 	while (last_movie_id < movie_id) {
				while (rating <= 5) {
					r[i++] = 0;
					rating++;
				}
				rating = 1;
				last_movie_id++;
			}
		}
		/* output 1 null per rating increase, including any gaps */
		if (rating != last_rating) {
			while (last_rating < rating) {
				r[i++] = 0;
				last_rating++;
			}
		}
		last_rating = rating;
		last_movie_id = movie_id;
		r[i++] = MAKE_RATING(customer_id, rating_date);
	}
	fh = fopen("ratings.dat", "wb");
	fwrite(r, sizeof(RATING), NUM_SLOTS, fh);
	fclose(fh);
	printf("%d\n", count);
	mysql_free_result(res); 
	return 0; 
} 
