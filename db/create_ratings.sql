drop table if exists ratings;
create table ratings (
	movie_id smallint unsigned not null,
	customer_id mediumint unsigned not null,
	rating tinyint unsigned not null,
	rating_date date not null);
