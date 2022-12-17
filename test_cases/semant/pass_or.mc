function main() gives bool {
  a = bool true;
  b = bool true;
  return a || b;
}

function main2() gives bool {
  a = bool true;
  b = bool false;
  return a || b;
}

function main3() gives bool {
  a = bool false;
  b = bool false;
  return a || b;
}