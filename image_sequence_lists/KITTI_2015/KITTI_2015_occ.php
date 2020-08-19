<?php
parse_str(implode('&', array_slice($argv, 1)), $_GET);

$selection = array();
for ($x=0;$x<=199;$x++) {
	$nr = sprintf('%03d', $x);
	$selection[] = array('image_2', $nr);
}

for ($x=0;$x<count($selection);$x++) {
  echo ';';
  echo $selection[$x][0].'/000'.$selection[$x][1].'_10.png,'.$selection[$x][0].'/000'.$selection[$x][1].'_11.png;';
  echo '0;';
  echo 'flow_occ/000'.$selection[$x][1].'_10.png;';
  echo '1.0;';
  echo $selection[$x][0].'/edges/000'.$selection[$x][1].'_10.png.edges';
  if ($x < count($selection)-1)
    echo "\n";
}
?>