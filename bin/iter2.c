#include <mysql/mysql.h> 
#include <stdio.h> 
#include <string.h> 

int main() 
{ 
	MYSQL mysql; 
	MYSQL_RES *res; 
	MYSQL_ROW row; 
	int count = 0;

	char query[255]; 
	mysql_init(&mysql); 
	mysql_real_connect(&mysql,"localhost","","","netflix",0,NULL,0); 
	sprintf(query,"SELECT movie_id, rating, customer_id FROM ratings ORDER BY movie_id, rating"); 
	mysql_real_query(&mysql,query,(unsigned int)strlen(query)); 
	res = mysql_use_result(&mysql); 
	while(row = mysql_fetch_row(res)) 
	{
		count++;
	}
	printf("%d\n", count);
	mysql_free_result(res); 
	return 0; 
} 
