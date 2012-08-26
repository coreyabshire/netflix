import MySQLdb

f = open("/Users/Corey/Downloads/download/qualifying.txt")
db = MySQLdb.connect(db = "netflix")
c = db.cursor()
movie_id = 0
for line in f:
	line = line.strip()
	if line.endswith(":"):
		movie_id = int(line.replace(":", ""))
	else:
		fields = line.split(",", 1)
		customer_id = int(fields[0])
		rating_date = fields[1]
		c.execute("insert into qual (movie_id, customer_id, rating_date) values (%s, %s, %s)",
				  (movie_id, customer_id, rating_date))
f.close()
c.close()
db.close()

