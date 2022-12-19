function main() gives int {
  console << main1();
  console << main2();
  console << main3();
  return 0;
}

function main1() gives bool {
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