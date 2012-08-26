import MySQLdb

download_dir = "/Users/Corey/Downloads/Download/"
movie_filename = download_dir + "movie_titles.txt"
movies = {}
customers = {}
dates = {}
ratings = []
db = MySQLdb.connect(db = "netflix")
cursor = db.cursor()

class Movie():
    def __init__(self, id, year, title):
        self.id = id
        self.year = year
        self.title = title
        self.ratings = []
    def add_rating(self, rating):
        self.ratings.append(rating)
    def get_training_filename(self):
        return download_dir + "training_set/" + "mv_%07d.txt" % self.id

class Customer():
    def __init__(self, id):
        self.id = id
        self.ratings = []
    def add_rating(self, rating):
        self.ratings.append(rating)

class Date():
    def __init__(self, date_string):
            [self.year, self.month, self.date] = [ int(x) for x in date_string.split("-") ]

class Rating():
    def __init__(self, movie, customer, score, date):
        self.movie = movie
        self.customer = customer
        self.score = score
        self.date = date


def get_movie(movie_id):
    return movies[movie_id]

def get_customer(customer_id):
    if customer_id in customers:
        customer = customers[customer_id]
    else:
        customer = Customer(customer_id)
        customers[customer_id] = customer
    return customer

def get_date(date_string):
    if date_string in dates:
        date = dates[date_string]
    else:
        date = Date(date_string)
        dates[date_string] = date
    return date

def load_movies():
    f = open(movie_filename)
    linenum = 1
    for line in f:
        [id, year, title] = line.split(",", 2)
        id = int(id)
        if year == "NULL":
            year = None
        else:
            year = int(year)
        title = title.strip()
        movies[id] = Movie(id, year, title)
    f.close()

def import_movies():
    keys = movies.keys()
    keys.sort()
    db = MySQLdb.connect(db = "netflix")
    cursor = db.cursor()
    for movie_id in keys:
        movie = get_movie(movie_id)
        cursor.execute("""insert into movies (id, title, year)
                          values (%s, %s, %s)""",
                       (movie.id, movie.title, movie.year))

def count_ratings(movie_id):
    movie = get_movie(movie_id)
    f = open(movie.get_training_filename())
    count = 0
    for line in f:
        if line.endswith(":"):
            continue
        count = count + 1
    f.close()
    return count

def count_all_ratings():
    f = open("out.txt", "w")
    keys = movies.keys()
    keys.sort()
    total = 0
    for movie_id in keys:
        movie = get_movie(movie_id)
        count = count_ratings(movie.id)
        total = total + count
        print >>f, "%d, %d, %s" % (count, movie.id, movie.title)
    print "Grand total: ", total
    f.close()

def count_all_ratings_db():
    cursor
    

def import_ratings(movie_id):
    movie = get_movie(movie_id)
    f = open(movie.get_training_filename())
    db = MySQLdb.connect(db = "netflix")
    cursor = db.cursor()
    for line in f:
        line = line.strip()
        if line.endswith(":"):
            continue
        fields = line.split(",", 2)
        customer_id = int(fields[0])
        rating = int(fields[1])
        rating_date = fields[2]
        cursor.execute("""INSERT INTO ratings (movie_id, customer_id, rating, rating_date)
                          VALUES (%s, %s, %s, %s)""",
                       (movie_id, customer_id, rating, rating_date))
    cursor.close()
    db.close()
        
def import_all_ratings():
    keys = movies.keys()
    keys.sort()
    for movie_id in keys:
        movie = get_movie(movie_id)
        print "importing %d - %s... " % (movie_id, movie.title), 
        import_ratings(movie_id)
        print "done"
    print "all done."

def load_ratings(movie_id):
    movie = get_movie(movie_id)
    f = open(movie.get_training_filename())
    for line in f:
        line = line.strip()
        if line.endswith(":"):
            continue
        else:
            fields = line.split(",", 2)
            customer = int(fields[0])
            rating = int(fields[1])
            date = fields[2]
            rating = Rating(movie, customer, rating, date)
            ratings.append(rating)
            #customer.add_rating(rating)
            movie.add_rating(rating)
    f.close()

def load_all_ratings(upto):
    for movie_id in movies.keys():
        print "loading %d - %s" % (movie_id, movies[movie_id].title)
        load_ratings(movie_id)
        if movie_id >= upto:
            break

def load():
    load_movies()
    load_all_ratings(20000)


def group_scores(movie_id):
    x = {}
    for i in range(1, 6):
        x[i] = 0
    for r in movies[movie_id].ratings:
        x[r.score] = x[r.score] + 1
    return x

def group_scores_pct(movie_id):
    x = {}
    for i in range(1, 6):
        x[i] = 0
    for r in movies[movie_id].ratings:
        x[r.score] = x[r.score] + 1
    total = len(movies[movie_id].ratings)
    for i in range(1, 6):
        x[i] = int(round(float(x[i]) / float(total) * 100.0))
    return x


