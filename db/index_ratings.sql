create index ratings_index_01
on ratings (customer_id, movie_id);

create index ratings_index_02
on ratings (movie_id, rating_date);

