<ul>
<?php

if ($handle = opendir('.')) {

  // read all files in the current directory
  while (false !== ($entry = readdir($handle))) {

    // extract the extension of the file
    $ext = strtolower(substr(strrchr($entry,"."),1));

    if ($ext == "png") {
      $func = substr($entry,0,strpos($entry,"."));
      echo "<li><a href=\"$entry\">$func</a></li>";
      }
    }

    closedir($handle);
}
?>
</ul>
