function main() gives int {
  return 0;
}

function main2() gives int {
  return main();
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
  return main5(main(), main());
}
