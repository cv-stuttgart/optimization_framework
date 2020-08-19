<?php
parse_str(implode('&', array_slice($argv, 1)), $_GET);

$sequence_name = array();
  $sequence_name[] = array('alley_1',    50 );
  $sequence_name[] = array('alley_2',    50 );
  $sequence_name[] = array('ambush_2',   21 );
  $sequence_name[] = array('ambush_4',   33 );
  $sequence_name[] = array('ambush_5',   50 );
  $sequence_name[] = array('ambush_6',   20 );
  $sequence_name[] = array('ambush_7',   50 );
  $sequence_name[] = array('bamboo_1',   50 );
  $sequence_name[] = array('bamboo_2',   50 );
  $sequence_name[] = array('bandage_1',  50 );
  $sequence_name[] = array('bandage_2',  50 );
  $sequence_name[] = array('cave_2',     50 );
  $sequence_name[] = array('cave_4',     50 );
  $sequence_name[] = array('market_2',   50 );
  $sequence_name[] = array('market_5',   50 );
  $sequence_name[] = array('market_6',   40 );
  $sequence_name[] = array('mountain_1', 50 );
  $sequence_name[] = array('shaman_2',   50 );
  $sequence_name[] = array('shaman_3',   50 );
  $sequence_name[] = array('sleeping_1', 50 );
  $sequence_name[] = array('sleeping_2', 50 );
  $sequence_name[] = array('temple_2',   50 );
  $sequence_name[] = array('temple_3',   50 );

for ($s=0;$s<count($sequence_name);$s++)
for ($x=1;$x<$sequence_name[$s][1];$x++)
  {  
  $gtruth = 'flow/'.$sequence_name[$s][0].sprintf('/frame_%04d.flo',$x);
  $frame1 = 'clean/'.$sequence_name[$s][0].sprintf('/frame_%04d.png',$x);
  $frame2 = 'clean/'.$sequence_name[$s][0].sprintf('/frame_%04d.png',$x+1);

  echo ';';
  echo $frame1.','.$frame2.';';
  echo '0;';
  echo $gtruth.';';
  echo '1.0;';
  echo 'final/'.$sequence_name[$s][0].'/edges'.sprintf('/frame_%04d.png.edges',$x);
  if ($s<count($sequence_name)-1 or $x<$sequence_name[$s][1]-1 )
    echo "\n";
  }
?>
