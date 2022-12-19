function main() gives int {
  console << main1();
  console << main2();
  console << main3(123);
  console << main4();
  console << main5(123, 321);
  console << main6();
  return 0;
}

function main1() gives int {
  return 1;
}

function main2() gives int {
  return main1();
}

function main3(int arg) gives int {
  return arg;
}

function main4() gives int {
  return main3(main2());
}

function main5(int arg1, int arg2) gives int {
  return arg1 + arg2;
}

function main6() gives int {
  return main5(main1(), main1());
}
