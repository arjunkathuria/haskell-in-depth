psql -c 'drop database IF EXISTS sakila_films' -U pgadmin
psql -c 'create database sakila_films' -U pgadmin
psql -d sakila_films -a -f "$PWD/film-schema.sql" -U 'pguser_1'
psql -d sakila_films -a -f films-insert.sql -U 'pguser_1'
