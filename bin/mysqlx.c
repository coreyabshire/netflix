#include <mysql/mysql.h> 
#include <stdio.h> 
#include <string.h> 

int main() 
{ 
   MYSQL mysql; 
   MYSQL_RES *res; 
   MYSQL_ROW row; 

   char query[80]; 
   mysql_init(&mysql); 
   mysql_real_connect(&mysql,"localhost","","","netflix",0,NULL,0); 
   sprintf(query,"SELECT id,title FROM movies"); 
   mysql_real_query(&mysql,query,(unsigned int)strlen(query)); 
   res = mysql_use_result(&mysql); 
   while(row = mysql_fetch_row(res)) 
    printf("%s\t%s\n",row[0],row[1]); 
   mysql_free_result(res); 
   return 0; 
} 
