#include <mysql/mysql.h> 
#include <stdio.h> 
#include <string.h> 

#define BYTE unsigned char
#define UINT unsigned int
#define CUST(r) (0x7FFFF&(r))
#define DATE(r) ((r)>>19)
#define RATING unsigned int
#define NUM_MOVIES 17770
#define NUM_SCORES 5
#define NUM_RATINGS 100480507
#define MAKE_RATING(cust,date) ((date)<<19|(cust))
#define NUM_SLOTS ((NUM_MOVIES * NUM_SCORES) + NUM_RATINGS)
RATING r[NUM_SLOTS];

int main(int argc, char **argv) 
{ 
	MYSQL mysql; 
	MYSQL_RES *res; 
	MYSQL_STMT *stmt;
	MYSQL_BIND bind[4];
	UINT row[4];
	my_bool rownull[4];
	FILE* fh;
	int count = 0, i = 0, im = 1, ir = 1;
	char *query = "SELECT movie_id, rating, customer_altid, rating_altdate FROM quick_train_table"; 
	int fetch_result = 0;

	printf("NUM_SLOTS: %d\n", NUM_SLOTS);
	mysql_init(&mysql); 
	mysql_real_connect(&mysql,"localhost","","","netflix",0,NULL,0); 
	stmt = mysql_stmt_init(&mysql);
	mysql_stmt_prepare(stmt, query, strlen(query));
	memset((void *) bind, 0, sizeof(bind));
	memset((void *) r, 0, sizeof(r));

	for (i = 0; i < 4; i++) {
		bind[i].buffer_type = MYSQL_TYPE_LONG;
		bind[i].buffer = &row[i];
		bind[i].is_unsigned = 1;
		bind[i].is_null = &rownull[i];
	}

	mysql_stmt_bind_result(stmt, bind);
	mysql_stmt_execute(stmt);

	i = 0;
	fetch_result = mysql_stmt_fetch(stmt);
	for (im = 1; im <= NUM_MOVIES; im++) {
		for (ir = 1; ir <= NUM_SCORES; ir++) {
			//printf("movie: %u, rating: %u; (%d, %d)\n", im, ir, row[0], row[1]); 
			while (fetch_result == 0 && im == row[0] && ir == row[1]) {
				//printf("%d: %u, %u, %u, %u\n", i, row[0], row[1], row[2], row[3]); 
				if (count % 1000000 == 0) 
					printf("%d\n", count / 1000000);
				r[i++] = MAKE_RATING(row[2], row[3]);
				count++;
				fetch_result = mysql_stmt_fetch(stmt);
			}
			//printf("inserting null\n"); 
			r[i++] = 0;
		}
	}
	fh = fopen("ratings.dat", "wb");
	fwrite(r, sizeof(RATING), NUM_SLOTS, fh);
	fclose(fh);
	printf("%d\n", count);
	mysql_stmt_free_result(stmt); 
	mysql_stmt_close(stmt);
	return 0; 
} 
