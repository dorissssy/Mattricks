function main() gives int {
  i = int 0;
  while (i < 5) {
    i = i + 1;
  }
  console << i;
  console << main2();
  return i;
}

function main2() gives int {
  i = int 0;
  while (i <= 5) {
    i = i + 1;
  }
  return i;
}
