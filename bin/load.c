#include <mysql/mysql.h> 
#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 

#define NUM_MOVIES 17700

typedef struct {
	unsigned int movie_id;
	unsigned int *r1cust;
	unsigned int *r2cust;
	unsigned int *r3cust;
	unsigned int *r4cust;
	unsigned int *r5cust;
} 
RatingBlock;

RatingBlock ratings[NUM_MOVIES];

int main() 
{ 
	MYSQL mysql; 
	MYSQL_RES *res; 
	MYSQL_ROW row; 
	int count = 0;
	int i = 0;

	char query[255]; 
	mysql_init(&mysql); 
	mysql_real_connect(&mysql,"localhost","","","netflix",0,NULL,0); 
	sprintf(query,"SELECT movie_id,r1count,r2count,r3count,r4count,r5count FROM rating_counts"); 
	mysql_real_query(&mysql,query,(unsigned int)strlen(query)); 
	res = mysql_use_result(&mysql); 
	while(row = mysql_fetch_row(res)) 
	{
		ratings[i].movie_id = atoi(row[0]);
		printf("%d\n", ratings[i].movie_id);
		ratings[i].r1cust = (unsigned int *) malloc(atoi(row[1]) * sizeof(int));
		if (ratings[i].r1cust == NULL) {
			printf("could not allocate\n");
			return 0;
		}
		ratings[i].r2cust = (unsigned int *) malloc(atoi(row[2]) * sizeof(int));
		if (ratings[i].r2cust == NULL) {
			printf("could not allocate\n");
			return 0;
		}
		ratings[i].r3cust = (unsigned int *) malloc(atoi(row[3]) * sizeof(int));
		if (ratings[i].r3cust == NULL) {
			printf("could not allocate\n");
			return 0;
		}
		ratings[i].r4cust = (unsigned int *) malloc(atoi(row[4]) * sizeof(int));
		if (ratings[i].r4cust == NULL) {
			printf("could not allocate\n");
			return 0;
		}
		ratings[i].r5cust = (unsigned int *) malloc(atoi(row[5]) * sizeof(int));
		if (ratings[i].r5cust == NULL) {
			printf("could not allocate\n");
			return 0;
		}
		i++;
		count++;
	}
	printf("%d\n", count);
	mysql_free_result(res); 
	printf("all done. waiting 10s for inspection.\n");
	sleep(10);
	return 0; 
} 
