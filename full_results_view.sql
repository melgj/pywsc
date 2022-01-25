create view full_results as (
	select r.*, t1.team_name as home_team, t2.team_name as away_team, home_odds, draw_odds, away_odds
		from results r 
		left join odds o on r.match_id = o.match_id 
		left join teams t1 on r.home_team_id = t1.team_id 
		left join teams t2 on r.away_team_id = t2.team_id
		order by match_id
	);

