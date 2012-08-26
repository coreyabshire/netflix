create or replace view quick_train as
select r.movie_id, r.rating, c.altid customer_altid, datediff(r.rating_date, '1999-11-11') rating_altdate
from ratings r straight_join customers c on r.customer_id = c.id
