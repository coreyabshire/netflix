drop table if exists customers;
create table customers (
	id mediumint unsigned primary key,
	rcount mediumint unsigned not null);
insert into customers
select * from customers_old;
