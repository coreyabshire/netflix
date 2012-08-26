import MySQLdb

basedir = "/Users/Corey/Netflix"
infile = open(basedir + "/download/qualifying.txt")
outfile = open(basedir + "/out/submission.txt", "w")
db = MySQLdb.connect(db = "netflix")
c = MySQLdb.cursors.SSCursor(db)
print "here i am"
c.execute("select  * from qual")
print "after query"
movie_id = 0
global_average = 3.6043
i = 0
for line in infile:
	line = line.strip()
	if line.endswith(":"):
		movie_id = int(line.replace(":", ""))
		outfile.write("%s:\n" % movie_id)
	else:
		print i
		fields = line.split(",", 1)
		customer_id = int(fields[0])
		rating_date = fields[1]
		(m, c, r, d) = c.fetchone()
		outfile.write("%s,%s\n" % (c,d))
	print i
	i = i + 1
outfile.close()
infile.close()
c.close()
db.close()
