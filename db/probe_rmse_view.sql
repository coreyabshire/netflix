create or replace view probe_rmse_base as 
select 0.9474 as cinematch_rmse,
	   round(sqrt(sum(pow(rating-prediction,2))/count(*)), 4) as rmse
from probe;

create or replace view probe_rmse as 
select cinematch_rmse, rmse, round((cinematch_rmse - rmse) / cinematch_rmse * 100.0, 2) as improvement
from probe_rmse_base;
