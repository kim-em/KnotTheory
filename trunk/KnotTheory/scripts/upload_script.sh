echo \$Path \= \$Path~Join~{\".\", \"..\", \"../..\"}\; > upload_script.m
echo \<\<KnotTheory\` >> upload_script.m
echo CreateWikiConnection[\"http://katlas.math.toronto.edu/w/index.php\", \"$1\", \"$2\"] >> upload_script.m
echo ProcessKnotAtlasUploadQueue[\"$3\"] >> upload_script.m
cat upload_script.m | $4math
