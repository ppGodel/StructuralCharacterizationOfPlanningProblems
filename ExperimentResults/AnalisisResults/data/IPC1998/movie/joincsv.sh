head strips-movie-x-1.csv -n 1 > strips-movie-all.csv
awk 'FNR > 1' strips-movie-x-*.csv >> strips-movie-all.csv
