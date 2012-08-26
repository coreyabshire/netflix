f = open("/Users/Corey/Downloads/download/qualifying.txt")
movie_id = 0
movie_lines = 0
rating_lines = 0
total_lines = 0
for line in f:
	line = line.strip()
	if line.endswith(":"):
		movie_id = int(line.replace(":", ""))
		movie_lines = movie_lines + 1
	else:
		rating_lines = rating_lines + 1
	total_lines = total_lines + 1
f.close()
print "movie lines", movie_lines
print "rating lines", rating_lines
print "total lines", total_lines

