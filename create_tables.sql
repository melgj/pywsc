create table teams (
	team_name varchar(50),
	team_id integer primary key
);

create table results (
	season_id integer,
	game_week integer,
	match_id integer primary key,
	home_team_id integer,
	home_score integer,
	home_shots integer,
	away_team_id integer,
	away_score integer,
	away_shots integer,
	
	constraint resht_fk foreign key(home_team_id) references teams(team_id) on delete cascade,
	constraint resat_fk foreign key(away_team_id) references teams(team_id) on delete cascade
);

create table odds (
	match_id integer primary key,
	home_odds numeric,
	draw_odds numeric,
	away_odds numeric,
	
	constraint odds_fk foreign key(match_id) references results(match_id) on delete cascade
);

create table fixtures (
	season_id integer,
	game_week integer,
	match_id integer primary key,
	home_team_id integer,
	away_team_id integer
);

create table players (
	player_name varchar(50),
	team_id integer,
	position_id integer,
	
	constraint players_fk foreign key(team_id) references teams(team_id) on delete cascade
);

create table starters (
	match_id integer,
	starting_xi varchar(500)	
);

