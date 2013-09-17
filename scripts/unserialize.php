#!/usr/bin/env php

<?php


set_error_handler('notice_exception');

function notice_exception($severity, $msg, $file, $line) {
  if (error_reporting() & $severity) {
    throw new ErrorException($msg, 0, $severity, $file, $line);
  }
}

$fh = fopen('php://stdin', 'r');
$stdin = '';
while ($line = fgets($fh)) {
  $stdin .= $line;
}

try {
  $out = unserialize(trim($stdin));
  print_r($out);
} catch (Exception $e) {
  //PHP Fatal error:  Uncaught exception 'ErrorException' with message 'unserialize(): Error at offset 1500 of 2060 bytes' in /home/sagotsky/.dotfiles/scripts/unserialize.php:20
  $msg = explode(' ',$e->getMessage());
  $offset_count = array_filter($msg, 'is_numeric');
  echo $e->getMessage() . "\n\n";
  echo substr($stdin, current($offset_count) - 8);
}
  

