import MySQLdb

f = open("/Users/Corey/Downloads/download/probe.txt")
db = MySQLdb.connect(db = "netflix")
c = db.cursor()
movie_id = 0
for line in f:
	line = line.strip()
	if line.endswith(":"):
		movie_id = int(line.replace(":", ""))
	else:
		customer_id = int(line)
		c.execute("insert into probe (movie_id, customer_id) values (%s, %s)",
				  (movie_id, customer_id))
f.close()
c.close()
db.close()

